module Expr where

import Data.Bool
import Data.Char
import Data.List (nub)
import Data.Functor.Foldable
import Text.ParserCombinators.ReadP
import Text.PrettyPrint (Doc, (<+>))
import qualified Text.PrettyPrint as PP

type Name = String

data Expr
    = EVar Name
    | ENum Int
    | EAp Expr Expr
    deriving (Eq)

isAtom :: Expr -> Bool
isAtom = \ case
    EAp _ _ -> False
    _       -> True

data ExprF r
    = EVarF Name
    | ENumF Int
    | EApF r r
    deriving (Functor)

type instance Base Expr = ExprF

instance Recursive Expr where
    project = \ case
        EVar v -> EVarF v
        ENum n -> ENumF n
        EAp s t -> EApF s t

instance Corecursive Expr where
    embed = \ case
        EVarF v  -> EVar v
        ENumF n  -> ENum n
        EApF s t -> EAp s t

type ScDefn = (Name, [Name], Expr)

instance {-# Overlapping #-} Read ScDefn where
    readsPrec _ = \ case 
        

isInfix :: String -> Bool
isInfix = (&&) <$> not . ("." ==) <*> all (`elem` symbols) 

symbols, symbols' :: String
symbols = ".<>=+-*/#$%^&:"
symbols' = tail symbols

instance Show Expr where
    showsPrec p = \ case
        EVar v -> showString' v
        ENum n -> showString' (show n)
        EAp s t -> case s of
            EAp (EVar o) s'
                | isInfix o
                    -> showParen (p > q) 
                        ( showsPrec q' s' 
                        . showSpace
                        . showString' o
                        . showSpace
                        . showsPrec q'' t
                        )
                where
                    q = prec o
                    d = adir o
                    (q', q'') = case d of
                        L -> (q, succ q)
                        I -> (succ q, succ q)
                        R -> (succ q, q)
            _ -> showsPrec p s . showSpace . showsPrec (succ p) t

showSpace :: ShowS
showSpace = showChar ' '

showString' :: String -> ShowS
showString' = (++)

data Fixity
    = L
    | I
    | R

prec :: String -> Int
prec o = maybe 9 snd (lookup o tbl)

adir :: String -> Fixity
adir o = maybe L fst (lookup o tbl)

tbl :: [(String, (Fixity, Int))]
tbl = [ ("$" , (R, 0))
          , ("||", (R, 1))
          , ("&&", (R, 2))
          , ("<" , (I, 3))
          , ("<=", (I, 3))
          , (">" , (I, 3))
          , (">=", (I, 3))
          , ("==", (I, 3))
          , ("/=", (I, 3))
          , ("+" , (L, 4))
          , ("-" , (L, 4))
          , ("*" , (L, 5))
          , ("/" , (L, 5))
          , ("^" , (R, 6))
          , ("." , (R, 7))
          ]

-- Parser

parse :: ReadP a -> ReadS a
parse = readP_to_S

token :: ReadP a -> ReadP a
token = (skipSpaces *>)

pMunch :: ReadP a -> ReadP [a]
pMunch p = pMunch1 p <++ pure []

pMunch1 :: ReadP a -> ReadP [a]
pMunch1 p = (:) <$> p <*> pMunch p

--

pBop :: Name -> ReadP (Expr -> Expr -> Expr)
pBop op = (EAp .) . (EAp . EVar) <$> (token (string op))

pExpr :: ReadP Expr
pExpr = chainr1 pExpr1 (pBop "||")

pExpr1 :: ReadP Expr
pExpr1 = chainr1 pExpr2 (pBop "&&")

mkBop :: Expr -> Name -> Expr -> Expr
mkBop e1 o e2 = EAp (EAp (EVar o) e1) e2

pExpr2 :: ReadP Expr
pExpr2 = foldl1 EAp <$> pMunch1 pAExpr

pAExpr :: ReadP Expr
pAExpr = pENum +++ pEVar +++ between (char '(') (char ')') pExpr

pENum :: ReadP Expr
pENum = token (ENum . read <$> munch1 isDigit)

pEVar :: ReadP Expr
pEVar = token (EVar <$> ((:) <$> satisfy isAlpha <*> munch isAlphaNum))

test :: String
test = "True && False && True"

relops :: [String]
relops = [ "<"
         , "<="
         , ">="
         , ">"
         , "=="
         , "/="
         ]

instance Read Expr where
    readsPrec _ = readP_to_S pExpr
