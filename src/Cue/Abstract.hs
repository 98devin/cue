{-# LANGUAGE
      LambdaCase #-}

{- Author: Devin Hill (98devin@gmail.com) -}

module Cue.Abstract
  ( AST(..)
  , RegExpr(..)
  , Comp(..)
  , Combine(..)
  , combination
  , comparison
  ) where
  
  
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

  


data AST
  = AST'Put RegExpr
  | AST'Com Combine RegExpr
  | AST'Tst RegExpr Comp RegExpr [AST]
  | AST'Die
  | AST'Inc
  | AST'Dec
  | AST'Cue ByteString
  deriving (Eq, Ord, Show)
  
data RegExpr
  = Queue RegExpr
  | Value Integer
  | Accumulator
  deriving (Eq, Ord, Show)

  

data Combine
  = Comb'GET
  | Comb'POP
  | Comb'ADD
  | Comb'MUL
  | Comb'SUB
  | Comb'DIV
  | Comb'MOD
  deriving (Eq, Ord, Show)
  
combination :: Combine -> Integer -> Integer -> Integer
combination = \case
  Comb'GET -> flip const
  Comb'POP -> const
  Comb'ADD -> (+)
  Comb'MUL -> (*)
  Comb'SUB -> (-)
  Comb'DIV -> div
  Comb'MOD -> mod
  
  
  
data Comp
  = Comp'LT
  | Comp'GT
  | Comp'EQ
  | Comp'LTE
  | Comp'GTE
  | Comp'NEQ
  deriving (Eq, Ord, Show)

comparison :: Comp -> Integer -> Integer -> Bool
comparison = \case
  Comp'LT  -> (<)
  Comp'GT  -> (>)
  Comp'EQ  -> (==)
  Comp'LTE -> (<=)
  Comp'GTE -> (>=)
  Comp'NEQ -> (/=)
  