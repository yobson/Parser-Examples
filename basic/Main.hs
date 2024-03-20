{-# LANGUAGE GADTs, OverloadedStrings, RankNTypes #-}

module Main where

import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Except
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (liftA2)

import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import Data.Void
import Data.Functor

import System.Environment
import System.Exit
import System.IO

type Parser = Parsec Void Text

comment :: Parser ()
comment = L.skipLineComment "Dim"

spaceConsumer :: Parser ()
spaceConsumer = L.space hspace1 comment empty

lexme :: Parser a -> Parser a
lexme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

printL, ifL, inputL, letL, whileL, thenL, multL, divL, plusL, subL, commaL, assignL, endL :: Parser Text
printL = symbol "PRINT"
ifL    = symbol "IF"
inputL = symbol "INPUT"
letL   = symbol "LET"
whileL = symbol "WHILE"
thenL  = symbol "THEN"
multL  = symbol "*"
divL   = symbol "/"
plusL  = symbol "+"
subL   = symbol "-"
commaL = symbol ","
assignL = symbol "="
endL = symbol "END"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

program :: Parser [Stmt]
program = many (statement <* newline)

statement :: Parser Stmt
statement =  try (Print <$> (printL *> exprList))
         <|> try (liftA2 IF (comp <* thenL) statement)
         <|> try (Input <$> (inputL *> varList))
         <|> try (liftA2 LET (letL *> lexme upperChar <* assignL) expression)
         <|>     (liftA2 While (comp <* thenL) (statement <* endL))

exprList :: Parser ExprList
exprList = ExprList <$> printExpr `sepBy1` commaL

printExpr :: Parser (Either String (Expr Int))
printExpr =  Left . unpack <$> try stringL
         <|> Right <$> expression

varList :: Parser [Char]
varList = lexme upperChar `sepBy1` commaL

comp :: Parser (Expr Bool)
comp = do
  e1 <- expression
  op <- relop
  e2 <- expression
  return $ Comp e1 e2 op

expression :: Parser (Expr Int)
expression =  try (liftA2 Add (term <* plusL) term)
          <|> try (liftA2 Sub (term <* subL)  term)
          <|> term

term :: Parser (Expr Int)
term =  try (liftA2 Mult (factor <* multL) factor)
    <|> try (liftA2 Div  (factor <* divL)  factor)
    <|> factor

factor :: Parser (Expr Int)
factor = try var <|> try number <|> parens expression

var :: Parser (Expr Int)
var = Var <$> lexme upperChar

number :: Parser (Expr Int)
number = Number <$> lexme L.decimal

relop :: Parser RelOP
relop =  try (symbol "="  $> Equ)
     <|> try (symbol "<>" $> NEQ)
     <|> try (symbol ">=" $> GEQ)
     <|> try (symbol "<=" $> LEQ)
     <|> try (symbol ">"  $> G)
     <|> try (symbol "<"  $> L)

stringL :: Parser Text
stringL = lexme $ between (symbol "\"") (symbol "\"") (pack <$> many (try alphaNumChar <|> allowedChars))

allowedChars :: Parser Char
allowedChars = try (char ' ') <|> try (char ',') <|> try (char ':') <|> try (char ';') <|> try (char '!')


data RelOP = Equ | GEQ | LEQ | G | L | NEQ
  deriving (Eq, Show)

data Expr a where
  Var :: Char -> Expr Int
  Number :: Int -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int
  Sub :: Expr Int -> Expr Int -> Expr Int
  Mult :: Expr Int -> Expr Int -> Expr Int
  Div :: Expr Int -> Expr Int -> Expr Int
  Comp :: (Ord a) => Expr a -> Expr a -> RelOP -> Expr Bool
  
instance Show (Expr a) where
  show (Var c)    = [c]
  show (Number i) = show i
  show (Add e1 e2) = concat ["(", show e1, " + ", show e2, ")"]
  show (Sub e1 e2) = concat ["(", show e1, " - ", show e2, ")"]
  show (Div e1 e2) = concat ["(", show e1, " / ", show e2, ")"]
  show (Mult e1 e2) = concat ["(", show e1, " * ", show e2, ")"]
  show (Comp e1 e2 c) = concat ["(", show e1, show c, show e2, ")"]

newtype ExprList = ExprList [Either String (Expr Int)]
  deriving Show


data Stmt = Print ExprList
          | IF (Expr Bool) Stmt
          | Input [Char]
          | LET Char (Expr Int)
          | While (Expr Bool) Stmt
          deriving Show

type Eval = ExceptT String (StateT [(Char, Int)] IO)

runEval :: Eval a -> IO a
runEval m = evalStateT (runExceptT m) [] >>= either fail return

evalExpr :: Expr a -> Eval a
evalExpr (Var c) = do
  var' <- gets $ lookup c
  maybe (throwError $ "Can't find " <> [c]) return var'
evalExpr (Number i) = return i
evalExpr (Add e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  return $ v1 + v2
evalExpr (Sub e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  return $ v1 - v2
evalExpr (Comp e1 e2 cmp) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  return $ cmp `elem` case compare v1 v2 of
    LT -> [NEQ, L, LEQ]
    EQ -> [Equ, GEQ, LEQ]
    GT -> [GEQ, G, NEQ]
evalExpr (Mult e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  return $ v1 * v2
evalExpr (Div e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  return $ v1 `div` v2

showExprList :: ExprList -> Eval String
showExprList (ExprList lst) = concat <$> mapM showExpr lst
  where showExpr (Left s) = return s
        showExpr (Right e) = show <$> evalExpr e

update :: Char -> Int -> Eval ()
update c i = do
  sts <- get
  case lookup c sts of
      Nothing -> put $ (c,i):sts
      Just _  -> modify (map (\(k,v) -> if k == c then (k,i) else (k,v)))

evalStmt :: Stmt -> Eval ()
evalStmt (Print lst) = showExprList lst >>= (liftIO . putStrLn)
evalStmt (IF cond s) = do
  c <- evalExpr cond
  when c $ evalStmt s
evalStmt (Input lst) = do
  forM_ lst $ \c -> do
    liftIO $ putStr $ c:"? "
    i <- liftIO $ read <$> getLine
    update c i
evalStmt (LET c e) = do
  v <- evalExpr e
  update c v
evalStmt (While e s) = do
  cond <- evalExpr e
  when cond $ do
    evalStmt s
    evalStmt (While e s)



main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  when (null args) $ do
    putStrLn "Expected basic file"
    exitWith (ExitFailure 1)
  let file = head args
  source <- T.readFile file
  case parse program file source of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right stmt  -> runEval $ mapM_ evalStmt stmt
