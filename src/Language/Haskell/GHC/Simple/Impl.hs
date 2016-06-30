{-# LANGUAGE FlexibleInstances, CPP, PatternGuards #-}
-- | Lower level building blocks for custom code generation.
module Language.Haskell.GHC.Simple.Impl (
    Ghc, PkgKey,
    liftIO,
    toSimplifiedStg,
    toModMetadata,
    modulePkgKey, pkgKeyString
  ) where

-- GHC scaffolding
import BinIface
import GHC hiding (Warning)
import GhcMonad (liftIO)
import HscMain
import HscTypes
import TidyPgm
import CorePrep
import StgSyn
import CoreSyn
import CoreToStg
import SimplStg
import DriverPipeline
#if __GLASGOW_HASKELL__ >= 800
import qualified Module as M (moduleUnitId, unitIdString, UnitId)
#elif __GLASGOW_HASKELL__ >= 710
import qualified Module as M (modulePackageKey, packageKeyString, PackageKey)
#else
import qualified Module as M (modulePackageId, packageIdString, PackageId)
#endif

import Control.Monad
import Data.IORef
import System.FilePath (takeDirectory)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Language.Haskell.GHC.Simple.Types

instance Intermediate [StgBinding] where
  prepare = toSimplifiedStg

instance Intermediate CgGuts where
  prepare _ = return

instance Intermediate CoreProgram where
  prepare ms cgguts = do
    env <- hsc_env `fmap` getPipeState
    liftIO $ prepareCore env (hsc_dflags env) ms cgguts

-- | Package ID/key of a module.
modulePkgKey :: Module -> PkgKey

-- | String representation of a package ID/key.
pkgKeyString :: PkgKey -> String

#if __GLASGOW_HASKELL__ >= 800
-- | Synonym for 'M.UnitId', to bridge a slight incompatibility between
--   GHC 7.8/7.10/8.0.
type PkgKey = M.UnitId
modulePkgKey = M.moduleUnitId
pkgKeyString = M.unitIdString
#elif __GLASGOW_HASKELL__ >= 710
-- | Synonym for 'M.PackageKey', to bridge a slight incompatibility between
--   GHC 7.8 and 7.10.
type PkgKey = M.PackageKey
modulePkgKey = M.modulePackageKey
pkgKeyString = M.packageKeyString
#else
-- | Synonym for 'M.PackageId', to bridge a slight incompatibility between
--   GHC 7.8 and 7.10.
type PkgKey = M.PackageId
modulePkgKey = M.modulePackageId
pkgKeyString = M.packageIdString
#endif

-- | Build a 'ModMetadata' out of a 'ModSummary'.
toModMetadata :: CompConfig
              -> ModSummary
              -> ModMetadata
toModMetadata cfg ms = ModMetadata {
    mmSummary        = ms,
    mmName           = moduleNameString $ ms_mod_name ms,
    mmPackageKey     = pkgKeyString . modulePkgKey $ ms_mod ms,
    mmSourceIsHsBoot = ms_hsc_src ms == HsBootFile,
    mmSourceFile     = ml_hs_file $ ms_location ms,
    mmInterfaceFile  = ml_hi_file $ ms_location ms
  }

-- | Compile a 'ModSummary' into a list of simplified 'StgBinding's.
--   See <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/StgSynType>
--   for more information about STG and how it relates to core and Haskell.
toSimplifiedStg :: ModSummary -> CgGuts -> CompPipeline [StgBinding]
toSimplifiedStg ms cgguts = do
  env <- hsc_env `fmap` getPipeState
  let dfs = hsc_dflags env
  liftIO $ do
    prog <- prepareCore env dfs ms cgguts
    stg <- coreToStg dfs (ms_mod ms) prog
    fst `fmap` stg2stg dfs (ms_mod ms) stg

-- | Prepare a core module for code generation.
prepareCore :: HscEnv -> DynFlags -> ModSummary -> CgGuts -> IO CoreProgram
prepareCore env dfs _ms p = do
#if __GLASGOW_HASKELL__ >= 800
  liftIO $ corePrepPgm env (ms_mod _ms) (ms_location _ms) (cg_binds p) (cg_tycons p)
#elif __GLASGOW_HASKELL__ >= 710
  liftIO $ corePrepPgm env (ms_location _ms) (cg_binds p) (cg_tycons p)
#else
  liftIO $ corePrepPgm dfs env (cg_binds p) (cg_tycons p)
#endif
