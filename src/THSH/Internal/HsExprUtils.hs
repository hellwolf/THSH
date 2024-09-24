{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}

module THSH.Internal.HsExprUtils
  ( RdrName
  , findFreeVariables
  ) where

import           GHC                         (GenLocated (..), Located, SrcSpan, locA, unLoc)
import qualified GHC.Hs.Expr                 as HsExpr (GRHS (..), GRHSs (..), HsExpr (..), Match (..), MatchGroup (..))
import qualified GHC.Hs.Extension
import           GHC.Types.Name.Reader       (RdrName (..))
import qualified Language.Haskell.Syntax.Pat as Pat
--
import           Data.Data                   (Data, gmapQ)
import           Data.Typeable               (Typeable, cast)


findFreeVariables :: Data a => a -> [(SrcSpan, RdrName)]
findFreeVariables item = allNames
  where
    -- Find all free Variables in an HsExpr
    f :: forall a. (Data a, Typeable a) => a -> [Located RdrName]
    f expr = case cast @_ @(HsExpr.HsExpr GHC.Hs.Extension.GhcPs) expr of
#if MIN_VERSION_ghc(9,2,0)
      Just (HsExpr.HsVar _ l@(L a _)) -> [L (locA a) (unLoc l)]
#else
      Just (HsExpr.HsVar _ l) -> [l]
#endif

#if MIN_VERSION_ghc(9,10,0)
      Just (HsExpr.HsLam _ _ (HsExpr.MG _ (unLoc -> (map unLoc -> [HsExpr.Match _ _ (map unLoc -> ps) (HsExpr.GRHSs _ [unLoc -> HsExpr.GRHS _ _ (unLoc -> e)] _)])))) -> filter keepVar subVars
#elif MIN_VERSION_ghc(9,6,0)
      Just (HsExpr.HsLam _ (HsExpr.MG _ (unLoc -> (map unLoc -> [HsExpr.Match _ _ (map unLoc -> ps) (HsExpr.GRHSs _ [unLoc -> HsExpr.GRHS _ _ (unLoc -> e)] _)])))) -> filter keepVar subVars
#else
      Just (HsExpr.HsLam _ (HsExpr.MG _ (unLoc -> (map unLoc -> [HsExpr.Match _ _ (map unLoc -> ps) (HsExpr.GRHSs _ [unLoc -> HsExpr.GRHS _ _ (unLoc -> e)] _)])) _)) -> filter keepVar subVars
#endif
        where
          keepVar (L _ n) = n `notElem` subPats
          subVars = concat $ gmapQ f [e]
          subPats = concat $ gmapQ findPats ps
      _ -> concat $ gmapQ f expr

    -- Find all Variables bindings (i.e. patterns) in an HsExpr
    findPats :: forall a. (Data a, Typeable a) => a -> [RdrName]
    findPats p = case cast @_ @(Pat.Pat GHC.Hs.Extension.GhcPs) p of
      Just (Pat.VarPat _ (unLoc -> name)) -> [name]
      _                                   -> concat $ gmapQ findPats p
    -- Be careful, we wrap hsExpr in a list, so the toplevel hsExpr will be
    -- seen by gmapQ. Otherwise it will miss variables if they are the top
    -- level expression: gmapQ only checks sub constructors.
    allVars = concat $ gmapQ f [item]
    allNames = map (\(L l e) -> (l, e)) allVars
