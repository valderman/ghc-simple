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
import StaticFlags (discardStaticFlags)
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
import Data.List (sort, sortBy)
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
getDynFlagsForConfig cfg = initStaticFlags `seq` do
  ws <- newIORef []
  runGhc (maybe (Just libdir) Just (cfgGhcLibDir cfg)) $ do
    setDFS cfg (discardStaticFlags (cfgGhcFlags cfg)) ws noComp

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

    -- TODO: get rid of the @runPhase@ from the HscOut phase
    phaseHook _ p@(HscOut src_flavour mod_name result) inp dfs = do
      loc <- getLocation src_flavour mod_name
      setModLocation loc
      let next = hscPostBackendPhase dfs src_flavour (hscTarget dfs)
      case result of
        HscRecomp cgguts ms -> do
          outfile <- phaseOutputFilename next
          comp (ml_hi_file loc) ms cgguts
          runPhase p inp dfs -- return (RealPhase next, outfile)
        _ ->
          runPhase p inp dfs -- return (RealPhase next, ml_obj_file loc)
    phaseHook stop (RealPhase p) inp _ | p `elem` stop =
      return (RealPhase StopLn, inp)
    phaseHook _ p inp dfs =
      runPhase p inp dfs

-- | Write a module to cache file.
writeModCache :: Binary a => CompConfig -> CompiledModule a -> IO ()
writeModCache cfg (CompiledModule m meta) = do
    createDirectoryIfMissing True (takeDirectory cachefile)
    BS.writeFile cachefile (encode m)
  where
    ext = cfgCacheFileExt cfg
    modfile = moduleNameSlashes (ms_mod_name (mmSummary meta)) <.> ext
    cachefile = maybe "" id (cfgCacheDirectory cfg) </> modfile

-- | Read a module from cache file.
readModCache :: Binary a => CompConfig -> ModMetadata -> IO (CompiledModule a)
readModCache cfg meta = do
    m <- decode `fmap` BS.readFile cachefile
    return $ CompiledModule m meta
  where
    ext = cfgCacheFileExt cfg
    modfile = moduleNameSlashes (ms_mod_name (mmSummary meta)) <.> ext
    cachefile = maybe "" id (cfgCacheDirectory cfg) </> modfile

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
compileFold cfg comp f acc files = initStaticFlags `seq` do
    warns <- newIORef []     -- all warnings produced by GHC
    accref <- newIORef acc   -- accumulator for compilation fold function
    tgtref <- newIORef []    -- ref containing all compilation targets
    recomp <- newIORef [] -- ref containing all modules that were recompiled
    runGhc (maybe (Just libdir) Just (cfgGhcLibDir cfg)) $ do
      (_, files2) <- setDFS cfg dfs warns (comp' accref recomp tgtref)
      ecode <- genCode cfg f accref tgtref recomp (files ++ files2)
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
    dfs = discardStaticFlags (cfgGhcFlags cfg)
    comp' accref recompref tgtref hifile ms cgguts = do
      source <- prepare ms cgguts
      liftIO $ do
        tgts <- readIORef tgtref
        let meta = toModMetadata cfg False tgts ms
        code <- comp meta source
        let cm = CompiledModule code meta
        writeModCache cfg cm
        -- TODO: if we already recompiled the module, we shouldn't do it
        --       again, for instance when recompilation is forced due to
        --       missing cache file
        atomicModifyIORef' recompref $ \xs -> (ms_mod ms : xs, ())
        readIORef accref >>= flip f cm >>= writeIORef accref

{-# NOINLINE initStaticFlags #-}
-- | Use lazy evaluation to only call 'parseStaticFlags' once.
initStaticFlags :: [Located String]
initStaticFlags = unsafePerformIO $ fmap fst (parseStaticFlags [])

-- | Map a compilation function over each 'ModSummary' in the dependency graph
--   of a list of targets.
genCode :: (GhcMonad m, Binary b)
        => CompConfig
        -> (a -> CompiledModule b -> IO a)
        -> IORef a
        -> IORef [Target]
        -> IORef [Module]
        -> [String]
        -> m (Either [Error] (DynFlags, a))
genCode cfg f accref tgtref recompref files = do
    dfs <- getSessionDynFlags
    merrs <- handleSourceError (maybeErrors dfs) $ do
      ts <- mapM (flip guessTarget Nothing) files
      liftIO $ writeIORef tgtref ts
      setTargets ts
      loads <- load LoadAllTargets
      mss <- depanal [] False
      acc <- liftIO $ readIORef accref
      recompiled <- liftIO $ readIORef recompref
      let cachedmods = sortBy onModId mss \\ sort recompiled
      -- TODO: if some cached mod failed to load, recompile it
      acc' <- liftIO $ foldM (loadCachedMod ts) acc cachedmods
      liftIO $ writeIORef accref acc'
      return $ if succeeded loads then Nothing else Just []
    case merrs of
      Just errs -> return $ Left errs
      _         -> do
        code <- liftIO $ readIORef accref
        return $ Right (dfs, code)
  where
    onModId m n = ms_mod m `compare` ms_mod n

    loadCachedMod tgts acc ms =
      readModCache cfg (toModMetadata cfg True tgts ms) >>= f acc

    -- remove all mod summaries from msmss whose mod identities exist in mmods.
    -- both lists must be sorted.
    (\\) :: [ModSummary] -> [Module] -> [ModSummary]
    msmss@(ms:mss) \\ mmods@(m:mods)
      | ms_mod ms == m = mss \\ mmods        -- ms found in mods; remove it
      | ms_mod ms <  m = msmss \\ mods       -- ms < m; ms may still be in mods
      | otherwise      = ms : (mss \\ mmods) -- ms > m; ms is not in mods
    mss \\ []          = mss                 -- mods empty, keep all remaining
    []  \\ _           = []                  -- no mss to keep!

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
