{-# LANGUAGE
      OverloadedStrings
    , MultiParamTypeClasses 
    , FlexibleInstances
    , LambdaCase #-}

{- Author: Devin Hill (dhill45@jhu.edu) -}
      
module Cue.Parser
  ( Symbols
  , parse
  ) where

  
import Parser.Combinators
import Parser.Err
import Parser.Pos

import Cue.Scanner
import Cue.Abstract


import Control.Exception

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe (fromMaybe)
import Data.Functor(($>))

import Control.Applicative
import Control.Monad

import Control.Monad.IO.Class (liftIO)

-------------------------------------------------------------------
--    Primary Parser Type                                        --
------------------------------------------------------------------- 

-- | The type of (combinator) parser we'll be using at this stage
type P a = Parser [Token] (PErr [ParseError] IO) a


type Symbols = Map ByteString [AST]
  

-- | Give a preexisting parser a more specific error message
(<!>) :: P a -> String -> P a
p <!> s = p <??> const [ParseError s]
  

-- | Give a preexisting parser a more specific error message
(<!!>) :: P a -> String -> P a
p <!!> s = p <|> (Parser $ \_ -> 
  liftIO $ throwIO $ ParseError s)
  

-- | Produce a WEAK error during parsing with the provided message
parseError' :: String -> P a
parseError' s = empty <!> s

    
-- | Produce a STRONG error during parsing with the provided message
parseError :: String -> P a
parseError s = empty <!!> s





parse :: P Symbols
parse = foldl Map.union Map.empty <$> many decl


-- | Match a token from the input by its token type
token :: Token -> P Token
token kind = do
  n <- next
  unless (n == kind) $
    parseError' $ "unexpected token `" ++ show n ++ "'"
  return n


decl :: P Symbols
decl = do
  name <- ident
  code <- block
  return $ Map.fromList [ (name, code) ]
    

-- | Match specifically identifier tokens,
--   since this is a pretty common case
ident' :: P Token
ident' = satisfy
  (\case
    (Ident _) -> True
    _         -> False)
  next


-- | Match an identifier.
ident :: P ByteString
ident = do
  Ident b <- ident'
  return b
    
    
    
-- | Match specifically number tokens,
--   since this is a useful case
number' :: P Token
number' = satisfy 
  (\case
    (Number _) -> True
    _          -> False)
  next
  

-- | Match a number token.
number :: P Integer
number = do
  Number n <- number'
  return n
  
  
block :: P [AST]
block = do
  token Sym'LBRACE
  body <- many stmt
  token Sym'RBRACE
  return body


stmt' :: Token -> (RegExpr -> AST) -> P AST
stmt' tok cstr = do
  token tok
  ast <- cstr <$> regexpr
  token Sym'SEMICOLON
  return ast
  
  
stmt'' :: Token -> AST -> P AST
stmt'' tok cstr = do
  token tok
  token Sym'SEMICOLON
  return cstr
  

stmt :: P AST
stmt =
  stmt' Key'GET AST'Get <|>
  stmt' Key'PUT AST'Put <|>
  stmt' Key'POP AST'Pop <|>
  stmt' Key'ADD AST'Add <|> 
  stmt' Key'SUB AST'Sub <|> 
  stmt' Key'MUL AST'Mul <|>
  stmt' Key'DIV AST'Div <|>
  stmt' Key'MOD AST'Mod <|>
  tststmt <|>
  stmt'' Key'INC AST'Inc <|>
  stmt'' Key'DEC AST'Dec <|>
  stmt'' Key'DIE AST'Die <|>
  cuestmt
  
  
tststmt :: P AST
tststmt = do
  token Key'TST
  lhs <- fromMaybe Accumulator <$> optional regexpr
  op  <- comp
  rhs <- regexpr
  body <- block
  return $ AST'Tst lhs op rhs body
  

cuestmt :: P AST
cuestmt = do
  token Key'CUE
  name <- ident
  token Sym'SEMICOLON
  return $ AST'Cue name
  
  
regexpr :: P RegExpr
regexpr = token Sym'PERCENT *> (Queue <$> regexpr')


regexpr' :: P RegExpr
regexpr' = (Value <$> number) <|> regexpr


comp :: P Comp
comp =
  (token Sym'LTSIGN   $> Comp'LT  ) <|>
  (token Sym'GTSIGN   $> Comp'GT  ) <|>
  (token Sym'LTEQUALS $> Comp'LTE ) <|>
  (token Sym'GTEQUALS $> Comp'GTE ) <|>
  (token Sym'EQUALS   $> Comp'EQ  ) <|>
  (token Sym'NOT      $> Comp'NEQ )

