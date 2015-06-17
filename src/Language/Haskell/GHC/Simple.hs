{-# LANGUAGE CPP, PatternGuards #-}
-- | Simplified interface to the GHC API.
module Language.Haskell.GHC.Simple (
    -- * Entry points
    compile, compileWith, compileFold,

    -- * Configuration, input and output types
    module Simple.Types,
    StgModule,
    getDynFlagsForConfig,

    -- * GHC re-exports for processing STG and Core
    module CoreSyn, module StgSyn, module Module,
    module Id, module IdInfo, module Var, module Literal, module DataCon,
    module OccName, module Name,
    module Type, module TysPrim, module TyCon,
    module ForeignCall, module PrimOp,
    module DynFlags, module SrcLoc,
    ModSummary (..), ModGuts (..),
    PkgKey,
    pkgKeyString, modulePkgKey
  ) where

-- GHC scaffolding
import GHC hiding (Warning)
import GhcMonad (liftIO)
import DynFlags
import HscTypes
import ErrUtils
import Bag
import SrcLoc
import Outputable
import Hooks
import StaticFlags (discardStaticFlags)

-- Convenience re-exports for fiddling with STG
import StgSyn
import CoreSyn
import Name hiding (varName)
import Type
import TysPrim
import TyCon
import Literal
import Var hiding (setIdExported, setIdNotExported, lazySetIdInfo)
import Id
import IdInfo
import OccName hiding (varName)
import DataCon
import ForeignCall
import PrimOp
import Module

-- Misc. stuff
import GHC.Paths (libdir)
import Data.IORef
import Control.Monad
import Language.Haskell.GHC.Simple.PrimIface as Simple.PrimIface
import Language.Haskell.GHC.Simple.Types as Simple.Types
import Language.Haskell.GHC.Simple.Impl
import System.IO.Unsafe

-- | Compile a list of targets and their dependencies into intermediate code.
--   Uses settings from the the default 'CompConfig'.
compile :: Compile a
        => [String]
        -- ^ List of compilation targets. A target can be either a module
        --   or a file name.
        -> IO (CompResult [CompiledModule a])
compile = compileWith defaultConfig

-- | Compile a list of targets and their dependencies using a custom
--   configuration.
compileWith :: Compile a
            => CompConfig a
            -- ^ GHC pipeline configuration.
            -> [String]
            -- ^ List of compilation targets. A target can be either a module
            --   or a file name. Targets may also be read from the specified
            --   'CompConfig', if 'cfgUseTargetsFromFlags' is set.
            -> IO (CompResult [CompiledModule a])
compileWith cfg = compileFold (cfg {cfgGhcPipeline = toCode}) consMod []

consMod :: [CompiledModule a] -> CompiledModule a -> IO [CompiledModule a]
consMod xs x = return (x:xs)

-- | Obtain the dynamic flags and extra targets that would be used to compile
--   anything with the given config.
getDynFlagsForConfig :: CompConfig a -> IO (DynFlags, [String])
getDynFlagsForConfig cfg = initStaticFlags `seq` do
  ws <- newIORef []
  runGhc (maybe (Just libdir) Just (cfgGhcLibDir cfg)) $ do
    setDFS cfg (discardStaticFlags (cfgGhcFlags cfg)) ws

-- | Set and return the appropriate dynflags and extra targets for the given
--   config.
setDFS :: CompConfig a     -- ^ Compilation configuration.
       -> [String]         -- ^ Dynamic GHC command line flags.
       -> IORef [Warning]  -- ^ IORef to use for logging warnings.
       -> Ghc (DynFlags, [String])
setDFS cfg flags warns = do
    -- Parse and update dynamic flags
    dfs <- getSessionDynFlags
    (dfs', files2, _dynwarns) <- parseDynamicFlags dfs (map noLoc flags)
    let dfs'' = cfgUpdateDynFlags cfg $ dfs' {
                    log_action = logger (log_action dfs') warns
                  }

    -- Update prim interface hook name and cache if we're using a custom
    -- GHC.Prim interface, setting the dynflags in the process.
    case cfgCustomPrimIface cfg of
      Just (nfo, strs) -> setPrimIface dfs'' nfo strs
      _                -> void $ setSessionDynFlags dfs''
    finaldfs <- getSessionDynFlags
    return (finaldfs, map unLoc files2)
  where
    logger deflog warns dfs severity srcspan style msg
      | cfgUseGhcErrorLogger cfg = do
        logger' deflog warns dfs severity srcspan style msg
        -- Messages other than warnings and errors are already logged by GHC
        -- by default.
        case severity of
          SevWarning -> deflog dfs severity srcspan style msg
          SevError   -> deflog dfs severity srcspan style msg
          _          -> return ()
      | otherwise = do
        logger' deflog warns dfs severity srcspan style msg

    -- Collect warnings and supress errors, since we're collecting those
    -- separately.
    logger' _ w dfs SevWarning srcspan _style msg = do
      liftIO $ atomicModifyIORef' w $ \ws ->
        (Warning srcspan (showSDoc dfs msg) : ws, ())
    logger' _ _ _ SevError _ _ _ = do
      return ()
    logger' output _ dfs sev srcspan style msg = do
      output dfs sev srcspan style msg

    setPrimIface dfs nfo strs = do
      void $ setSessionDynFlags dfs {
          hooks = (hooks dfs) {ghcPrimIfaceHook = Just $ primIface nfo strs}
        }
      getSession >>= liftIO . fixPrimopTypes nfo strs


-- | Left fold over a list of compilation targets and their dependencies.
--
--   Sometimes you don't just want a huge pile of intermediate code lying
--   around; chances are you either want to dump it to file or combine it with
--   some other intermediate code, without having to keep it all in memory at
--   the same time.
compileFold :: CompConfig b
            -- ^ GHC pipeline configuration.
            -> (a -> CompiledModule b -> IO a)
            -- ^ Compilation function.
            -> a
            -- ^ Initial accumulator.
            -> [String]
            -- ^ List of compilation targets. A target can be either a module
            --   or a file name. Targets may also be read from the specified
            --   'CompConfig', if 'cfgUseTargetsFromFlags' is set.
            -> IO (CompResult a)
compileFold cfg comp acc files = initStaticFlags `seq` do
    warns <- newIORef []
    runGhc (maybe (Just libdir) Just (cfgGhcLibDir cfg)) $ do
      (_, files2) <- setDFS cfg (discardStaticFlags (cfgGhcFlags cfg)) warns
      ecode <- genCode cfg ghcPipeline comp acc (files ++ files2)
      ws <- liftIO $ readIORef warns
      case ecode of
        Right (finaldfs, code) ->
          return Success {
              compResult = code,
              compWarnings = ws,
              compDynFlags = finaldfs
            }
        Left es ->
          return Failure {
              compErrors = es,
              compWarnings = ws
            }
  where
    ghcPipeline = toCompiledModule $ cfgGhcPipeline cfg

{-# NOINLINE initStaticFlags #-}
-- | Use lazy evaluation to only call 'parseStaticFlags' once.
initStaticFlags :: [Located String]
initStaticFlags = unsafePerformIO $ fmap fst (parseStaticFlags [])

-- | Map a compilation function over each 'ModSummary' in the dependency graph
--   of a list of targets.
genCode :: GhcMonad m
         => CompConfig t
         -> (ModSummary -> m a)
         -> (b -> a -> IO b)
         -> b
         -> [String]
         -> m (Either [Error] (DynFlags, b))
genCode cfg comp usercomp acc files = do
    dfs <- getSessionDynFlags
    merrs <- handleSourceError (maybeErrors dfs) $ do
      ts <- mapM (flip guessTarget Nothing) files
      setTargets ts
      loads <- load LoadAllTargets
      return $ if succeeded loads then Nothing else Just []
    case merrs of
      Just errs -> return $ Left errs
      _         -> do
        mss <- depanal [] False
        code <- foldM (\a x -> comp (noLog x) >>= liftIO . usercomp a) acc mss
        return $ Right (dfs, code)
  where
    -- We logged everything when we did @load@, we don't want to do it twice.
    noLog m =
      m {ms_hspp_opts = (ms_hspp_opts m) {log_action = \_ _ _ _ _ -> return ()}}
    maybeErrors dfs
      | cfgUseGhcErrorLogger cfg = \srcerr -> liftIO $ do
        let msgs = srcErrorMessages srcerr
        printBagOfErrors dfs msgs
        return . Just . map (fromErrMsg dfs) $ bagToList msgs
      | otherwise =
        return . Just . map (fromErrMsg dfs) . bagToList . srcErrorMessages

fromErrMsg :: DynFlags -> ErrMsg -> Error
fromErrMsg dfs e = Error {
    errorSpan      = errMsgSpan e,
    errorMessage   = showSDocForUser dfs ctx (errMsgShortDoc e),
    errorExtraInfo = showSDocForUser dfs ctx (errMsgExtraInfo e)
  }
  where
    ctx = errMsgContext e
