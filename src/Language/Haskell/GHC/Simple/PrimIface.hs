{-# LANGUAGE CPP #-}
-- | Facilities for using a custom GHC.Prim interface.
--
--   The simplest(?) way to use this is to generate primop info
--   using the @genprimopcode@ program from GHC, making any desired changes
--   to those files, and passing the @primOpInfo@ and @primOpStrictness@
--   functions defined therein as the @cfgCustomPrimIface@ member of
--   your config.
--
--   Your strictness and info functions need to support all the
--   primops exported by the GHC version in use, making code written for this
--   interface rather less portable than code using the rest of @ghc-simple@.
--
--   This functionality is probably what you want if you are making a cross
--   compiler, to prevent the types of GHC primops from changing depending on
--   the compiler host platform.
--
--   If you are *not* making a cross compiler, chances are you will not want to
--   touch this with a ten foot pole.
module Language.Haskell.GHC.Simple.PrimIface (
    module Demand, module TysWiredIn, module FastString, module CmmType,
    module BasicTypes,
    PrimOp (..), PrimOpInfo (..),
    mkGenPrimOp, mkDyadic, mkMonadic, mkCompare,
    primIface, fixPrimopTypes
  ) where
import IfaceEnv (initNameCache)
import PrelInfo (primOpRules, ghcPrimIds)
#if __GLASGOW_HASKELL__ < 800
import PrelInfo (wiredInThings)
#else
import PrelInfo (wiredInIds, primOpId)
import TcTypeNats (typeNatTyCons)
#endif
import PrimOp hiding (primOpSig)
import IdInfo
import Rules
import PrelNames
import Name
import BasicTypes
import Type
import Unique
import Id
import TysWiredIn
import TysPrim
import FastString
import Demand
import HscTypes
import Avail
import MkId (seqId)
import Data.IORef (modifyIORef')
import TyCon
import CmmType

#if __GLASGOW_HASKELL__ < 710
setCallArityInfo :: IdInfo -> Arity -> IdInfo
setCallArityInfo i _ = i
#endif

-- | Module interface for @GHC.Prim@, with the given function applied to each
--   primop.
primIface :: (PrimOp -> PrimOpInfo)
          -> (PrimOp -> Arity -> StrictSig)
          -> ModIface
primIface nfo str = (emptyModIface gHC_PRIM) {
        mi_exports = exports nfo str,
        mi_decls = [],
        mi_fixities = fixies,
        mi_fix_fn = mkIfaceFixCache fixies
    }
  where
    fixies = (getOccName seqId, fixity "seq" 0 InfixR) :
             [(primOpOcc op, f)
             | op <- allThePrimOps
             , Just f <- [primOpFixity op]]
#if __GLASGOW_HASKELL__ >= 800
    fixity = Fixity
#else
    fixity _ = Fixity
#endif

exports :: (PrimOp -> PrimOpInfo)
        -> (PrimOp -> Arity -> StrictSig)
        -> [IfaceExport]
exports nfo str = concat [
    map avail ghcPrimIds,
    map (avail . (fixPrimOp nfo str)) allThePrimOps,
    [ availTC n
    | tc <- funTyCon : coercibleTyCon : primTyCons, let n = tyConName tc]
  ]
  where
#if __GLASGOW_HASKELL__ >= 800
    avail = Avail NotPatSyn . idName
    availTC n = AvailTC n [n] []
#else
    avail = Avail . idName
    availTC n = AvailTC n [n]
#endif
          
-- | Fix primop types in the name cache.
fixPrimopTypes :: (PrimOp -> PrimOpInfo)
               -> (PrimOp -> Arity -> StrictSig)
               -> HscEnv
               -> IO ()
fixPrimopTypes nfo str env = do
    modifyIORef' (hsc_NC env) fixNC
  where
    isPrim (AnId v) = isPrimOpId v
    isPrim _        = False

    fixNC (NameCache us _) = initNameCache us $ concat [
        [getName thing | thing <- wiredInThings, not (isPrim thing)],
        basicKnownKeyNames,
        map (getName . AnId . fixPrimOp nfo str) allThePrimOps
      ]

#if __GLASGOW_HASKELL__ >= 800
-- This list is used only to initialise HscMain.knownKeyNames
-- to ensure that when you say "Prelude.map" in your source code, you
-- get a Name with the correct known key (See Note [Known-key names])
wiredInThings
  = concat
    [           -- Wired in TyCons and their implicit Ids
          tycon_things
        , concatMap implicitTyThings tycon_things

                -- Wired in Ids
        , map AnId wiredInIds

                -- PrimOps
        , map (AnId . primOpId) allThePrimOps
    ]
  where
    tycon_things = map ATyCon ([funTyCon] ++ primTyCons ++ wiredInTyCons
                                    ++ typeNatTyCons)
#endif

-- | Primitive operation signature: constists of the op's type, arity and
--   strictness annotations.
data PrimOpSig = PrimOpSig {
    opType       :: !Type,
    opArity      :: !Arity,
    opStrictness :: !StrictSig
  }

-- | Get the signature of a primitive operation.
primOpSig :: (PrimOp -> PrimOpInfo)
          -> (PrimOp -> Arity -> StrictSig)
          -> PrimOp
          -> PrimOpSig
primOpSig nfo str op = PrimOpSig {
    opType       = typ,
    opArity      = arity,
    opStrictness = str op arity
  }
  where
    (typ, arity) =
      case nfo op of
        Monadic _ t          -> (mkForAllTys [] $ mkFunTys [t] t, 1)
        Dyadic _ t           -> (mkForAllTys [] $ mkFunTys [t,t] t, 2)
        Compare _ t          -> (mkForAllTys [] $ mkFunTys [t,t] intPrimTy, 2)
        GenPrimOp _ tvs ts t -> (mkForAllTys tvs $ mkFunTys ts t, length ts)

data PrimOpInfo
  = Dyadic      OccName         -- string :: T -> T -> T
                Type
  | Monadic     OccName         -- string :: T -> T
                Type
  | Compare     OccName         -- string :: T -> T -> Bool
                Type

  | GenPrimOp   OccName         -- string :: \/a1..an . T1 -> .. -> Tk -> T
#if __GLASGOW_HASKELL__ >= 800
                [TyBinder]
#else
                [TyVar]
#endif
                [Type]
                Type

fixPrimOp :: (PrimOp -> PrimOpInfo)
          -> (PrimOp -> Arity -> StrictSig)
          -> PrimOp
          -> Id
fixPrimOp opnfo str op =
    var
  where
    sig    = primOpSig opnfo str op
    var    = mkGlobalId (PrimOpId op) name (opType sig) nfo
    name   = mkWiredInName gHC_PRIM (primOpOcc op) unique (AnId var) UserSyntax
    unique = mkPrimOpIdUnique $ primOpTag op
    nfo    = flip setCallArityInfo (opArity sig) $
             noCafIdInfo `setStrictnessInfo` opStrictness sig
                         `setRuleInfo` ri
                         `setArityInfo` opArity sig
                         `setInlinePragInfo` neverInlinePragma
    ri     = mkRuleInfo $ case primOpRules name op of
                            Just r -> [r]
                            _      -> []
#if __GLASGOW_HASKELL__ < 800
    mkRuleInfo = mkSpecInfo
    infixl 1 `setRuleInfo`
    setRuleInfo = setSpecInfo
#endif

-- | Create a 'PrimOpInfo' for dyadic, monadic and compare primops.
--   Needed by GHC-generated primop info includes.
mkDyadic, mkMonadic, mkCompare :: FastString -> Type -> PrimOpInfo
mkDyadic str  ty = Dyadic  (mkVarOccFS str) ty
mkMonadic str ty = Monadic (mkVarOccFS str) ty
mkCompare str ty = Compare (mkVarOccFS str) ty

-- | Create a general 'PrimOpInfo'. Needed by GHC-generated primop info
--   includes.
#if __GLASGOW_HASKELL__ >= 800
mkGenPrimOp :: FastString -> [TyBinder] -> [Type] -> Type -> PrimOpInfo
#else
mkGenPrimOp :: FastString -> [TyVar] -> [Type] -> Type -> PrimOpInfo
#endif
mkGenPrimOp str tvs tys ty = GenPrimOp (mkVarOccFS str) tvs tys ty
