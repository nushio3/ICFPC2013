import Text.Trifecta
import Control.Applicative
import Control.Lens

identifier = token $ some lower

type Idfr = String

data Program = Program Idfr Expr deriving (Show, Eq, Ord)

data Reducer = Reducer Idfr Idfr Expr deriving (Show, Eq, Ord)

data Expr = C0 | C1 | Var Idfr 
    | If0 Expr Expr Expr
    | Fold Expr Expr Reducer
    | Op1 Op1 Expr
    | Op2 Op2 Expr Expr deriving (Show, Eq, Ord)

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16 deriving (Show, Eq, Ord)

data Op2 = And | Or | Xor | Plus deriving (Show, Eq, Ord)

parseReducer :: Parser Reducer
parseReducer = parens $ do
    symbol "lambda"
    symbol "("
    a <- identifier
    b <- identifier
    symbol ")"
    e <- parseExpr
    return $ Reducer a b e

parseExpr = choice [try $ parens $ symbol "if0" >> If0 <$> parseExpr <*> parseExpr <*> parseExpr
    , try $ parens $ symbol "fold" >> Fold <$> parseExpr <*> parseExpr <*> parseReducer
    , parens $ Op2 <$> parseOp2 <*> parseExpr <*> parseExpr
    , parens $ Op1 <$> parseOp1 <*> parseExpr
    , token $ C0 <$ symbol "0"
    , token $ C1 <$ symbol "1"
    , Var <$> identifier
    ]

parseOp1 = token $ choice [Not <$ symbol "not"
    , Shl1 <$ symbol "shl1"
    , Shr1 <$ symbol "shr1"
    , Shr4 <$ symbol "shr4"
    , Shr16 <$ symbol "shr16" ]

parseOp2 = token $ choice [And <$ symbol "and", Or <$ symbol "or", Xor <$ symbol "xor", Plus <$ symbol "plus"]

parseProgram :: Parser Program
parseProgram = parens $ do
    symbol "lambda"
    b <- parens identifier
    e <- parseExpr
    return $ Program b e

exprSize :: Expr -> Int
exprSize (If0 a b c) = 1 + exprSize a + exprSize b + exprSize c
exprSize (Fold a b (Reducer _ _ c)) = 1 + exprSize a + exprSize b + exprSize c
exprSize (Op1 _ e0) = 1 + exprSize e0
exprSize (Op2 _ e0 e1) = 1 + exprSize e0 + exprSize e1
exprSize C0 = 1
exprSize C1 = 1

programSize :: Program -> Int
programSize (Program _ e) = 1 + exprSize e
