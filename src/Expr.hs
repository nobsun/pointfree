module Expr where

import Data.Bool
import Data.Char
import Data.Functor.Foldable
import Text.ParserCombinators.ReadP
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

scDefnR :: ReadP ScDefn
scDefnR = (,,) <$> varR' <*> munchR varR'
      <*  skipSpaces <* string "="
      <*> exprR
      <*  skipSpaces <* eof

exprR :: ReadP Expr
exprR = aexprR +++ expr1R

aexprR :: ReadP Expr
aexprR = varR +++ numR +++ paren exprR

varR :: ReadP Expr
varR = EVar <$> varR'

varR' :: ReadP Name
varR' = (:) <$> (skipSpaces *> satisfy isAlpha) <*> munch isAlphaNum

numR :: ReadP Expr
numR = ENum <$> number

number :: ReadP Int
number = read <$> (skipSpaces *> munch1 isDigit)

paren :: ReadP Expr -> ReadP Expr
paren = between (skipSpaces *> char '(') (skipSpaces *> char ')')

expr1R :: ReadP Expr
expr1R = assembleOp <$> expr2R <*> expr1cR

data PartialExpr
    = NoOp
    | FoundOp Name Expr

assembleOp :: Expr -> PartialExpr -> Expr
assembleOp e = \ case
    NoOp -> e
    FoundOp op e' -> EAp (EAp (EVar op) e) e'

expr1cR :: ReadP PartialExpr
expr1cR = (FoundOp <$> (skipSpaces *> string "||") <*> expr1R)
        +++ emptyR NoOp

emptyR :: a -> ReadP a
emptyR a = a <$ look

expr2R :: ReadP Expr
expr2R = assembleOp <$> expr3R <*> expr2cR

expr2cR :: ReadP PartialExpr
expr2cR = (FoundOp <$> (skipSpaces *> string "&&") <*> expr2R)
        +++ emptyR NoOp

expr3R :: ReadP Expr
expr3R = skipSpaces *> (assembleOp <$> expr4R <*> expr3cR)

expr3cR :: ReadP PartialExpr
expr3cR = (FoundOp <$> relopR <*> expr4R) +++ emptyR NoOp

relopR :: ReadP Name
relopR = skipSpaces *> foldr1 (+++) (map string relops)

relops :: [Name]
relops = ["<", "<=", "==", "/=", ">=", ">"]

expr4R :: ReadP Expr
expr4R = assembleOp <$> expr5R <*> expr4cR

expr4cR :: ReadP PartialExpr
expr4cR = (FoundOp <$> (skipSpaces *> string "+") <*> expr4R)
      +++ (FoundOp <$> (skipSpaces *> string "-") <*> expr5R)
      +++ emptyR NoOp

expr5R :: ReadP Expr
expr5R = assembleOp <$> expr6R <*> expr5cR

expr5cR :: ReadP PartialExpr
expr5cR = (FoundOp <$> (skipSpaces *> string "*") <*> expr5R)
      +++ (FoundOp <$> (skipSpaces *> string "/") <*> expr6R)
      +++ emptyR NoOp

expr6R :: ReadP Expr
expr6R = assembleOp <$> expr7R <*> expr6cR 

expr6cR :: ReadP PartialExpr
expr6cR = (FoundOp <$> (skipSpaces *> string ".") <*> expr7R)
      +++ emptyR NoOp

expr7R :: ReadP Expr
expr7R = foldl1 EAp <$> munch1R aexprR

munchR :: ReadP a -> ReadP [a]
munchR r = munch1R r +++ emptyR []

munch1R :: ReadP a -> ReadP [a]
munch1R r = (:) <$> r <*> munchR r

instance Read Expr where
    readsPrec _ = readP_to_S exprR

instance {-# Overlapping #-} Read ScDefn where
    readsPrec _ = readP_to_S scDefnR

pprExpr :: Expr -> PP.Doc
pprExpr = \ case
    EVar v -> PP.text v
    ENum n -> PP.int n
    EAp s t -> case s of
        EAp (EAp (EVar o) s1) s2
            | isInfix o -> PP.parens (pprExpr s1 PP.<+> PP.text o PP.<+> pprExpr s2)
                           PP.<+> bool (PP.parens (pprExpr t)) (pprExpr t) (isAtom t)
        EAp (EVar o) s1
            | isInfix o -> pprExpr s1 PP.<+> PP.text o 
                           PP.<+> bool (PP.parens (pprExpr t)) (pprExpr t) (isAtom t)
        _ -> pprExpr s PP.<+> bool (PP.parens (pprExpr t)) (pprExpr t) (isAtom t)

isInfix :: String -> Bool
isInfix = all (`elem` ("<>=/*+-$." :: String))

instance Show Expr where
    show = PP.render . pprExpr

instance {-# Overlapping #-} Show ScDefn where
    show = PP.render . pprScDefn

pprScDefn :: ScDefn -> PP.Doc
pprScDefn (f, args, body) =
    PP.text f PP.<+> PP.hsep (map PP.text args) PP.<+> PP.text "=" PP.<+> pprExpr body
