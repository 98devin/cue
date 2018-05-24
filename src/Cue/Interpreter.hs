{-# LANGUAGE
      OverloadedStrings
    , RankNTypes
    , ViewPatterns
    , PatternSynonyms
    , TemplateHaskell #-}

{- Author: Devin Hill (dhill45@jhu.edu) -}

module Cue.Interpreter where
  

import Control.Monad
import Control.Monad.State
import Control.Monad.Cont

import qualified Control.Lens as L
import Control.Lens.Operators

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq


import Cue.Abstract



type Symbols = Map ByteString [AST]


data InterpState = IState
  { callQueue :: Seq ByteString
  , dataQueue :: Map Integer (Seq Integer)
  , symbols   :: Symbols
  , acc       :: Integer
  }
  deriving (Eq, Show)

  
makeLenses ''InterpState
  
  
initialState :: Symbols -> InterpState
iniitalState syms = IState
  { callQueue = Seq.fromList ["main"]
  , dataQueue = Map.empty
  , symbols   = syms
  , acc       = 0
  }
  

  
type I a = forall r. StateT InterpState (ContT r IO) a

  
interpret :: Symbols -> IO ()
interpret syms = runContT return . evalStateT interp $ initialState syms




interp :: I ()
interp = do
  calls <- gets callQueue
  unless (Seq.null calls) $ do
      let c :<| cs = calls
      callQueue %= const cs
      syms <- gets symbols
      let body = Map.findWithDefault [] c syms
      callCC $ \exit ->
        forM_ body $ \stmt -> do
          interpStmt stmt exit 