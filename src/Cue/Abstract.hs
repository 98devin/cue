{-# LANGUAGE
      LambdaCase
    , OverloadedStrings
    , RecordWildCards #-}

{- Author: Devin Hill (98devin@gmail.com) -}

module Cue.Abstract
  ( AST(..)
  , RegExpr(..)
  , ProcDef(..)
  , ProcInst(..)
  , Comp(..)
  , Combine(..)
  , Symbols
  , makeInst
  , emptyProc
  , combination
  , comparison
  ) where
  
  
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

  

data ProcDef
  = Proc
  { procName :: ByteString
  , procBody :: [AST]
  , procArgs :: [ByteString]
  }
  deriving (Eq, Show)


data ProcInst
  = Inst
  { instName :: ByteString
  , instBody :: [AST]
  , instArgs :: Map ByteString Integer
  }
  deriving (Eq, Show)
  
  
emptyProc :: ByteString -> ProcDef
emptyProc name = Proc
  { procName = B.concat ["missing proc: {", name, "}"]
  , procBody = []
  , procArgs = []
  }
  
  
makeInst :: ProcDef -> [Integer] -> ProcInst
makeInst Proc{..} args = Inst
  { instName = procName
  , instBody = procBody
  , instArgs = Map.fromList $ zip procArgs args
  }

  
type Symbols = Map ByteString ProcDef


data AST
  = AST'Put RegExpr
  | AST'Com Combine RegExpr
  | AST'Tst RegExpr Comp RegExpr [AST]
  | AST'Die
  | AST'End
  | AST'Inc
  | AST'Dec
  | AST'Cue ByteString [RegExpr]
  | AST'Cbl [AST]
  deriving (Eq, Ord, Show)
  
data RegExpr
  = Queue RegExpr
  | Value Integer
  | Var   ByteString
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
  