
{- Author: Devin Hill (dhill45@jhu.edu) -}

module Cue.Abstract where
  
  
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

  


data AST
  = AST'Get RegExpr
  | AST'Put RegExpr
  | AST'Pop RegExpr
  | AST'Add RegExpr
  | AST'Sub RegExpr
  | AST'Mul RegExpr
  | AST'Div RegExpr
  | AST'Mod RegExpr
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
  
  
  
data Comp
  = Comp'LT
  | Comp'GT
  | Comp'EQ
  | Comp'LTE
  | Comp'GTE
  | Comp'NEQ
  deriving (Eq, Ord, Show)