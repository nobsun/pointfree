module PointFree where

import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree ( headF, tailF )
import Data.Functor.Foldable
import Data.Set ( Set )
import qualified Data.Set as Set
import Expr
import GHC.RTS.Flags (RTSFlags(debugFlags))

type AnnExpr = Cofree ExprF
type Vars = Set Name

annVars :: Expr -> AnnExpr Vars
annVars = cata phi where
    phi e = case e of
        EVarF v  -> Set.singleton v :< e
        ENumF _  -> Set.empty :< e
        EApF s@(us :< _) t@(ws :< _)
                 -> Set.union us ws :< EApF s t

elimVar :: Name -> AnnExpr Vars -> AnnExpr Vars
elimVar v ae = case ae of
    vs :< e
        | Set.notMember v vs
            -> vs :< EApF (Set.empty :< EVarF "const")
                          (vs        :< e            )
        | otherwise
            -> case e of
                EVarF _ -> Set.empty :< EVarF "id"
                ENumF _ -> error "Impossible"
                EApF s@(svs :< _) t
                    | Set.notMember v svs
                        -> Set.delete v vs :< EApF (svs :< EApF (Set.empty :< EVarF ".") s)
                                                   (elimVar v t)
                    | otherwise
                        -> Set.delete v vs
                            :< EApF (Set.delete v svs :< EApF (Set.empty :< EVarF "_S") (elimVar v s))
                                    (elimVar v t)

deAnn :: AnnExpr a -> Expr
deAnn = ana psi where
    psi = \ case
        _ :< e -> e

pointfree :: ScDefn -> ScDefn
pointfree (f, args, body) = (f, [], body') where
    annbody = annVars body
    body' = deAnn (cata phi args)
    phi = \ case
        Nil -> annbody
        Cons v ann -> elimVar v ann

normalize :: ScDefn -> ScDefn
normalize (f,args,body) = (f,args,norm body)
norm :: Expr -> Expr
norm = \ case
        EAp (EVar "id") e -> norm e
        EAp (EAp (EVar "const") x) _ -> norm x
        EAp (EAp (EVar ".") s) t -> case s of
            EVar "id" -> norm t
            _         -> case t of
                EVar "id" -> norm s
                _         -> EAp (EAp (EVar ".") (norm s)) (norm t)
        EAp (EAp (EVar "_S") f) (EAp (EVar "const") x)
            -> EAp (EAp (EVar "flip") (norm f)) (norm x)
        EAp s t -> EAp (norm s) (norm t)
        e -> e
            