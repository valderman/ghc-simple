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
import PrelInfo (wiredInThings, primOpRules, ghcPrimIds)
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
    fixies = (getOccName seqId, Fixity 0 InfixR) :
             [(primOpOcc op, f)
             | op <- allThePrimOps
             , Just f <- [primOpFixity op]]

exports :: (PrimOp -> PrimOpInfo)
        -> (PrimOp -> Arity -> StrictSig)
        -> [IfaceExport]
exports nfo str = concat [
    map (Avail . idName) ghcPrimIds,
    map (Avail . idName . (fixPrimOp nfo str)) allThePrimOps,
    [ AvailTC n [n]
    | tc <- funTyCon : coercibleTyCon : primTyCons, let n = tyConName tc]
  ]

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
                [TyVar]
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
                         `setSpecInfo` si
                         `setArityInfo` opArity sig
                         `setInlinePragInfo` neverInlinePragma
    si     = mkSpecInfo $ case primOpRules name op of
                            Just r -> [r]
                            _      -> []

-- | Create a 'PrimOpInfo' for dyadic, monadic and compare primops.
--   Needed by GHC-generated primop info includes.
mkDyadic, mkMonadic, mkCompare :: FastString -> Type -> PrimOpInfo
mkDyadic str  ty = Dyadic  (mkVarOccFS str) ty
mkMonadic str ty = Monadic (mkVarOccFS str) ty
mkCompare str ty = Compare (mkVarOccFS str) ty

-- | Create a general 'PrimOpInfo'. Needed by GHC-generated primop info
--   includes.
mkGenPrimOp :: FastString -> [TyVar] -> [Type] -> Type -> PrimOpInfo
mkGenPrimOp str tvs tys ty = GenPrimOp (mkVarOccFS str) tvs tys ty
