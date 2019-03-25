{-# LANGUAGE CPP, PatternGuards #-}
-- | Simplified interface to the GHC API.
module Language.Haskell.GHC.Simple (
    -- * Entry points
    compile, compileWith, compileFold,

    -- * Configuration, input and output types
    module Simple.Types,
    getDynFlagsForConfig,

    -- * GHC re-exports for processing STG and Core
    module CoreSyn, module StgSyn, module Module,
    module Id, module IdInfo, module Var, module Literal, module DataCon,
    module OccName, module Name,
    module Type, module TysPrim, module TyCon,
    module ForeignCall, module PrimOp,
    module DynFlags, module SrcLoc,
    module DriverPhases,
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
import DriverPhases
import DriverPipeline

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
import Data.Binary
import qualified Data.ByteString.Lazy as BS
import Data.IORef
import Control.Monad
import GHC.Paths (libdir)
import System.Directory
import System.FilePath
import System.IO
import System.IO.Unsafe

-- Internals
import Language.Haskell.GHC.Simple.PrimIface as Simple.PrimIface
import Language.Haskell.GHC.Simple.Types as Simple.Types
import Language.Haskell.GHC.Simple.Impl

-- | Compile a list of targets and their dependencies into intermediate code.
--   Uses settings from the the default 'CompConfig'.
compile :: (Intermediate a, Binary b)
        => (ModMetadata -> a -> IO b)
        -- ^ Compilation function from some intermediate language to the
        --   desired output. The output type needs to be an instance of
        --   'Binary', as it will be cached after compilation to speed up
        --   future recompilation.
        --   This function is called once per module. Due to caching of modules
        --   which don't need to be recompiled, it will not necessarily be
        --   called once per module included in the return value of @compile@.
        -> [String]
        -- ^ List of compilation targets. A target can be either a module
        --   or a file name.
        -> IO (CompResult [CompiledModule b])
compile = compileWith defaultConfig

-- | Compile a list of targets and their dependencies using a custom
--   configuration.
compileWith :: (Intermediate a, Binary b)
            => CompConfig
            -- ^ GHC pipeline configuration.
            -> (ModMetadata -> a -> IO b)
            -- ^ Compilation function.
            -> [String]
            -- ^ List of compilation targets. A target can be either a module
            --   or a file name. Targets may also be read from the specified
            --   'CompConfig', if 'cfgUseTargetsFromFlags' is set.
            -> IO (CompResult [CompiledModule b])
compileWith cfg comp = compileFold cfg comp consMod []

consMod :: [CompiledModule a] -> CompiledModule a -> IO [CompiledModule a]
consMod xs x = return (x:xs)

-- | Obtain the dynamic flags and extra targets that would be used to compile
--   anything with the given config.
getDynFlagsForConfig :: CompConfig -> IO (DynFlags, [String])
getDynFlagsForConfig cfg = do
  ws <- newIORef []
  runGhc (maybe (Just libdir) Just (cfgGhcLibDir cfg)) $ do
    setDFS cfg (cfgGhcFlags cfg) ws noComp

noComp :: FilePath -> ModSummary -> CgGuts -> CompPipeline ()
noComp _ _ _ = return ()

-- | Set and return the appropriate dynflags and extra targets for the given
--   config.
setDFS :: CompConfig       -- ^ Compilation configuration.
       -> [String]         -- ^ Dynamic GHC command line flags.
       -> IORef [Warning]  -- ^ IORef to use for logging warnings.
       -> (FilePath -> ModSummary -> CgGuts -> CompPipeline ())
                           -- ^ Per-module compilation function.
       -> Ghc (DynFlags, [String])
setDFS cfg flags warns comp = do
    -- Parse and update dynamic flags
    dfs <- getSessionDynFlags
    (dfs', files2, _dynwarns) <- parseDynamicFlags dfs (map noLoc flags)
    let ps = cfgStopPhases cfg
        dfs'' = cfgUpdateDynFlags cfg $ dfs' {
                    log_action = logger (log_action dfs') warns,
                    hooks = (hooks dfs') {runPhaseHook = Just $ phaseHook ps}
                  }

    -- Update prim interface hook name and cache if we're using a custom
    -- GHC.Prim interface, setting the dynflags in the process.
    case cfgCustomPrimIface cfg of
      Just (nfo, strs) -> setPrimIface dfs'' nfo strs
      _                -> void $ setSessionDynFlags dfs''
    finaldfs <- getSessionDynFlags
    return (finaldfs, map unLoc files2)
  where
#if __GLASGOW_HASKELL__ >= 800
#define LOG(dfs,sev,span,sty,msg) (deflog dfs reason sev span sty msg)
    logger deflog warns dfs reason severity srcspan style msg
#else
#define LOG(dfs,sev,span,sty,msg) (deflog dfs sev span sty msg)
    logger deflog warns dfs severity srcspan style msg
#endif
      | cfgUseGhcErrorLogger cfg = do
#if __GLASGOW_HASKELL__ >= 800
        logger' deflog warns dfs reason severity srcspan style msg
#else
        logger' deflog warns dfs severity srcspan style msg
#endif
        -- Messages other than warnings and errors are already logged by GHC
        -- by default.
        case severity of
          SevWarning -> LOG(dfs, severity, srcspan, style, msg)
          SevError   -> LOG(dfs, severity, srcspan, style, msg)
          _          -> return ()
      | otherwise = do
#if __GLASGOW_HASKELL__ >= 800
        logger' deflog warns dfs reason severity srcspan style msg
#else
        logger' deflog warns dfs severity srcspan style msg
#endif

    -- Collect warnings and supress errors, since we're collecting those
    -- separately.
#if __GLASGOW_HASKELL__ >= 800
    logger' _ w dfs _ SevWarning srcspan _style msg = do
      liftIO $ atomicModifyIORef' w $ \ws ->
        (Warning srcspan (showSDoc dfs msg) : ws, ())
    logger' _ _ _ _ SevError _ _ _ = do
      return ()
    logger' output _ dfs reason sev srcspan style msg = do
      output dfs reason sev srcspan style msg
#else
    logger' _ w dfs SevWarning srcspan _style msg = do
      liftIO $ atomicModifyIORef' w $ \ws ->
        (Warning srcspan (showSDoc dfs msg) : ws, ())
    logger' _ _ _ SevError _ _ _ = do
      return ()
    logger' output _ dfs sev srcspan style msg = do
      output dfs sev srcspan style msg
#endif

    setPrimIface dfs nfo strs = do
      void $ setSessionDynFlags dfs {
          hooks = (hooks dfs) {ghcPrimIfaceHook = Just $ primIface nfo strs}
        }
      getSession >>= liftIO . fixPrimopTypes nfo strs

    -- TODO: get rid of the @runPhase@ from the HscOut phase
    phaseHook _ p@(HscOut src mod_name result) inp dfs = do
      loc <- getLocation src mod_name
      setModLocation loc
      let next = hscPostBackendPhase dfs src (hscTarget dfs)
      case result of
        HscRecomp cgguts ms -> do
          outfile <- phaseOutputFilename next
          comp (ml_hi_file loc) ms cgguts
          runPhase p inp dfs
        _ ->
          runPhase p inp dfs
    phaseHook stop (RealPhase p) inp _ | p `elem` stop =
      return (RealPhase StopLn, inp)
    phaseHook _ p inp dfs =
      runPhase p inp dfs

-- | Write a module to cache file.
writeModCache :: Binary a => CompConfig -> ModSummary -> a -> IO ()
writeModCache cfg ms m = do
    createDirectoryIfMissing True (takeDirectory cachefile)
    BS.writeFile cachefile (encode m)
  where
    cachefile = cacheFileFor cfg (ms_mod_name ms)

-- | Read a module from cache file.
readModCache :: Binary a
             => CompConfig
             -> ModMetadata
             -> [Target]
             -> IO (CompiledModule a)
readModCache cfg meta tgts = do
    m <- decode `fmap` BS.readFile cachefile
    return $ CompiledModule m meta (mmSummary meta `isTarget` tgts)
  where
    cachefile = cacheFileFor cfg (ms_mod_name (mmSummary meta))

-- | Get the cache file for the given 'ModSummary' under the given
--   configuration.
cacheFileFor :: CompConfig -> ModuleName -> FilePath
cacheFileFor cfg name =
    maybe "" id (cfgCacheDirectory cfg) </> modfile
  where
    modfile = moduleNameSlashes name <.> cfgCacheFileExt cfg

-- | Left fold over a list of compilation targets and their dependencies.
--
--   Sometimes you don't just want a huge pile of intermediate code lying
--   around; chances are you either want to dump it to file or combine it with
--   some other intermediate code, without having to keep it all in memory at
--   the same time.
compileFold :: (Intermediate a, Binary b)
            => CompConfig
            -- ^ GHC pipeline configuration.
            -> (ModMetadata -> a -> IO b)
            -- ^ Per module compilation function.
            -> (acc -> CompiledModule b -> IO acc)
            -- ^ Folding function.
            -> acc
            -- ^ Initial accumulator.
            -> [String]
            -- ^ List of compilation targets. A target can be either a module
            --   or a file name. Targets may also be read from the specified
            --   'CompConfig', if 'cfgUseTargetsFromFlags' is set.
            -> IO (CompResult acc)
compileFold cfg comp f acc files = do
    warns <- newIORef []  -- all warnings produced by GHC
    runGhc (maybe (Just libdir) Just (cfgGhcLibDir cfg)) $ do
      (_, files2) <- setDFS cfg dfs warns compileToCache
      ecode <- genCode cfg f acc (files ++ files2)
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
    dfs = cfgGhcFlags cfg
    compileToCache hifile ms cgguts = do
      source <- prepare ms cgguts
      liftIO $ comp (toModMetadata cfg ms) source >>= writeModCache cfg ms

-- | Is @ms@ in the list of targets?
isTarget :: ModSummary -> [Target] -> Bool
isTarget ms = any (`isTargetOf` ms)

-- | Is @t@ the target that corresponds to @ms@?
isTargetOf :: Target -> ModSummary -> Bool
isTargetOf t ms =
  case targetId t of
    TargetModule mn                                -> ms_mod_name ms == mn
    TargetFile fn _
      | ModLocation (Just f) _ _ <- ms_location ms -> f == fn
    _                                              -> False

-- | Map a compilation function over each 'ModSummary' in the dependency graph
--   of a list of targets.
genCode :: (GhcMonad m, Binary b)
        => CompConfig
        -> (a -> CompiledModule b -> IO a)
        -> a
        -> [String]
        -> m (Either [Error] (DynFlags, a))
genCode cfg f acc files = do
    dfs <- getSessionDynFlags
    eerrs <- handleSourceError (maybeErrors dfs) $ do
      ts <- mapM (flip guessTarget Nothing) files
      setTargets ts

      -- Compile all modules; if cached code file is gone, then force
      -- recompilation
      (loads, mss) <- do
        loads <- load LoadAllTargets
        mss <- mgModSummaries <$> depanal [] False
        recomp <- filterM needRecomp mss
        if null recomp
          then return (loads, mss)
          else do
            mapM_ (liftIO . removeFile . ml_obj_file . ms_location) recomp
            loads' <- load LoadAllTargets
            mss' <- mgModSummaries <$> depanal [] False
            return (loads', mss')

      acc' <- liftIO $ foldM (loadCachedMod ts) acc mss
      return $ if succeeded loads then Right acc' else Left []
    case eerrs of
      Left errs -> return $ Left errs
      Right acc -> return $ Right (dfs, acc)
  where
    needRecomp =
      liftIO . fmap not . doesFileExist . cacheFileFor cfg . ms_mod_name
    loadCachedMod tgts acc ms =
      readModCache cfg (toModMetadata cfg ms) tgts >>= f acc

    maybeErrors dfs
      | cfgUseGhcErrorLogger cfg = \srcerr -> liftIO $ do
        let msgs = srcErrorMessages srcerr
        printBagOfErrors dfs msgs
        return . Left . map (fromErrMsg dfs) $ bagToList msgs
      | otherwise =
        return . Left . map (fromErrMsg dfs) . bagToList . srcErrorMessages

fromErrMsg :: DynFlags -> ErrMsg -> Error
fromErrMsg dfs e = Error {
    errorSpan      = errMsgSpan e,
#if __GLASGOW_HASKELL__ >= 800
    errorMessage   = showSDocForUser dfs ctx (pprLocErrMsg e),
    errorExtraInfo = ""
#else
    errorMessage   = showSDocForUser dfs ctx (errMsgShortDoc e),
    errorExtraInfo = showSDocForUser dfs ctx (errMsgExtraInfo e)
#endif
  }
  where
    ctx = errMsgContext e
