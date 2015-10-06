-- | Config, input and output types for the simplified GHC API.
module Language.Haskell.GHC.Simple.Types (
    -- * GHC pipeline configuration
    Compile (..),
    CompConfig,
    defaultConfig,
    cfgGhcFlags, cfgUseTargetsFromFlags, cfgUpdateDynFlags, cfgGhcLibDir,
    cfgUseGhcErrorLogger, cfgCustomPrimIface, cfgGhcPipeline,
    cfgAlwaysCreateHiFiles,

    -- * Compilation results and errors
    CompiledModule (..),
    CompResult (..),
    Error (..),
    Warning (..),
    compSuccess
  ) where

import GHC
import Language.Haskell.GHC.Simple.PrimIface

-- | Any type we can generate intermediate code for.
class Compile a where
  -- | Generate some sort of code (or other output) from a Haskell module.
  toCode :: ModSummary -> Ghc a

-- | GHC pipeline configuration, parameterized over the intermediate code
--   produced by the pipeline.
data CompConfig a = CompConfig {
    -- | GHC command line dynamic flags to control the Haskell to STG
    --   compilation pipeline.
    --   For instance, passing @["-O2", "-DHELLO"]@ here is equivalent to
    --   passing @-O2 -DHELLO@ to the @ghc@ binary.
    --
    --   Note that flags set here are overridden by any changes to 'DynFlags'
    --   performed by 'cfgUpdateDynFlags', and that '--make' mode is always
    --   in effect.
    --
    --   Default: @[]@
    cfgGhcFlags :: [String],

    -- | If file or module names are found among the 'cfgGhcFlags',
    --   should they be used as targets, in addition to any targets given by
    --   other arguments to 'withStg' et al?
    --
    --   Default: @True@
    cfgUseTargetsFromFlags :: Bool,

    -- | Modify the dynamic flags governing the compilation process.
    --   Changes made here take precedence over any flags passed through
    --   'cfgGhcFlags'.
    --
    --   Default: @id@
    cfgUpdateDynFlags :: DynFlags -> DynFlags,

    -- | Use GHC's standard logger to log errors and warnings to the command
    --   line? Errors and warnings are always collected and returned,
    --   regardless of the value of this setting.
    --
    --   Output other than errors and warnings (dumps, etc.) are logged using
    --   the standard logger by default. For finer control over logging
    --   behavior, you should override 'log_action' in 'cfgUpdateDynFlags'.
    --
    --   Default: @False@
    cfgUseGhcErrorLogger :: Bool,

    -- | Path to GHC's library directory. If 'Nothing', the library directory
    --   of the system's default GHC compiler will be used.
    --
    --   Default: @Nothing@
    cfgGhcLibDir :: Maybe FilePath,

    -- | Use a custom interface for @GHC.Prim@.
    --   This is useful if you want to, for instance, compile to a 32 bit
    --   target architecture on a 64 bit host.
    --
    --   For more information, see "Language.Haskell.GHC.Simple.PrimIface".
    --
    --   Default: @Nothing@
    cfgCustomPrimIface :: Maybe (PrimOp -> PrimOpInfo,
                                 PrimOp -> Arity -> StrictSig),

    -- | Use a custom GHC pipeline to generate intermediate code. Useful if
    --   the provided instances for @[StgBinding]@ etc. don't quite do what you
    --   want them to. See "Language.Haskell.GHC.Simple.Impl" for more
    --   information about custom pipelines.
    --
    --   Default: @toCode@
    cfgGhcPipeline :: ModSummary -> Ghc a,

    -- | Always ensure that the interface file indicated by the
    --   'modInterfaceFile' field of 'CompiledModule' exists?
    --   When compiling with @hscTarget = HscNothing@, GHC will not
    --   automatically create module interface files.
    --   This is undesirable if, for instance, one wants the compiler to
    --   generate interface files as well as custom generated code, but not
    --   invoke any standard GHC code generator such as the LLVM or NCG
    --   generators.
    --
    --   Default: @True@
    cfgAlwaysCreateHiFiles :: Bool
  }

-- | Default configuration.
defaultConfig :: Compile a => CompConfig a
defaultConfig = CompConfig {
    cfgGhcFlags            = [],
    cfgUseTargetsFromFlags = True,
    cfgUpdateDynFlags      = id,
    cfgUseGhcErrorLogger   = False,
    cfgGhcLibDir           = Nothing,
    cfgCustomPrimIface     = Nothing,
    cfgGhcPipeline         = toCode,
    cfgAlwaysCreateHiFiles = True
  }

-- | Compiler output and metadata for a given module.
data CompiledModule a = CompiledModule {
    -- | 'ModSummary' for the module, as given by GHC.
    modSummary :: ModSummary,

    -- | String representation of the module's name, not qualified with a
    --   package key.
    --   'ModuleName' representation can be obtained from the module's
    --   'stgModSummary'.
    modName :: String,

    -- | String representation of the module's package key.
    --   'PackageKey' representation can be obtained from the module's
    --   'stgModSummary'.
    modPackageKey :: String,

    -- | Is this module a compilation target (as opposed to a dependency of
    --   one)?
    modIsTarget :: Bool,

    -- | Was the module compiler from a @hs-boot@ file?
    modSourceIsHsBoot :: Bool,

    -- | The Haskell source the module was compiled from, if any.
    modSourceFile :: Maybe FilePath,

    -- | 'ModIface' structure corresponding to this module.
    --   If 'modInterfaceFile' exists, it contains this structure.
    modInterface :: ModIface,

    -- | Interface file corresponding to this module.
    modInterfaceFile :: FilePath,

    -- | Module data generated by compilation; usually bindings of some kind.
    modCompiledModule :: a
  }

-- | A GHC error message.
data Error = Error {
    -- | Where did the error occur?
    errorSpan      :: SrcSpan,

    -- | Description of the error.
    errorMessage   :: String,

    -- | More verbose description of the error.
    errorExtraInfo :: String
  }

-- | A GHC warning.
data Warning = Warning {
    -- | Where did the warning occur?
    warnSpan :: SrcSpan,

    -- | What was the warning about?
    warnMessage :: String
  }

-- | Result of a compilation.
data CompResult a
  = Success {
      -- | Result of the compilation.
      compResult :: a,

      -- | Warnings that occurred during compilation.
      compWarnings :: [Warning],

      -- | Initial 'DynFlags' used by this compilation, collected from 'Config'
      --   data.
      compDynFlags :: DynFlags
    }
  | Failure {
      -- | Errors that occurred during compilation.
      compErrors :: [Error],

      -- | Warnings that occurred during compilation.
      compWarnings :: [Warning]
    }

-- | Does the given 'CompResult' represent a successful compilation?
compSuccess :: CompResult a -> Bool
compSuccess (Success {}) = True
compSuccess _            = False
