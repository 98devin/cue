{-# LANGUAGE
      OverloadedStrings
    , RankNTypes
    , PatternSynonyms
    , LambdaCase #-}

{- Author: Devin Hill (dhill45@jhu.edu) -}

module Cue.Interpreter where
  

import Control.Monad
import Control.Monad.State
import Control.Monad.Cont

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

import Data.Maybe (fromMaybe)

import Cue.Abstract



type Symbols = Map ByteString [AST]


data InterpState = IState
  { callQueue :: Seq ByteString
  , dataQueue :: Map Integer (Seq Integer)
  , symbols   :: Symbols
  , acc       :: Integer
  }
  deriving (Eq, Show)

  
  
initialState :: Symbols -> [Integer] -> InterpState
initialState syms input = IState
  { callQueue = Seq.fromList ["main"]
  , dataQueue = Map.fromList [ (0, Seq.fromList input) ]
  , symbols   = syms
  , acc       = 0
  }
  

type I' r a = StateT InterpState (ContT r IO) a
  
type I a = forall r. I' r a

  
interpret :: Symbols -> [Integer] -> IO InterpState
interpret syms input = 
  flip runContT return . 
  execStateT interp $ initialState syms input


interp :: I ()
interp = do
  calls <- gets callQueue
  unless (Seq.null calls) $ do
    let c :<| cs = calls
    
    modify' $ \s -> 
      s { callQueue = cs, acc = 0 }
    
    syms <- gets symbols
    let body = Map.findWithDefault [] c syms
    callCC $ \exit ->
      forM_ body $ \stmt -> do
        interpStmt exit stmt
    interp
          
          
          
getNthQueue :: Integer -> I (Seq Integer)
getNthQueue n = fromMaybe Empty <$> gets (Map.lookup n . dataQueue)
          

getRExp :: RegExpr -> I Integer
getRExp = \case
  Value n -> return n
  Queue q -> getRExp' q
  

getRExp' :: RegExpr -> I Integer
getRExp' = \case
  Value n  -> return n
  Queue re -> do
    q <- getRExp' re
    e <- getNthQueue q

    let (val, e') = case e of
          Empty    -> (0, Empty)
          v :<| vs -> (v, vs)
    
    modify' $ \s ->
      s { dataQueue = Map.insert q e' $ dataQueue s }
      
    return val
    

interpStmt :: (() -> I' r ()) -> AST -> I' r ()
interpStmt exit = \case
      
  AST'Put re -> do
    
    q <- getRExp re
    e <- getNthQueue q
    
    a <- gets acc
    let e' = e :|> a
    
    -- Update modified queue
    modify' $ \s ->
      s { dataQueue = Map.insert q e' $ dataQueue s }
    
    
  AST'Com com re -> do
    
    q <- getRExp re
    e <- getNthQueue q
    
    let (val, e') = case e of
          Empty    -> (0, Empty)
          v :<| vs -> (v, vs)
    
    -- Update accumulator and modified queue
    modify' $ \s ->
      s { acc = combination com (acc s) val, dataQueue = Map.insert q e' $ dataQueue s }
    
    
  AST'Tst Accumulator comp rhs body -> do
        
      rq <- getRExp rhs
      re <- getNthQueue rq
      
      let (val, re') = case re of
            Empty    -> (0, Empty)
            v :<| vs -> (v, vs)
            
      -- Update modified queue
      modify' $ \s ->
        s { dataQueue = Map.insert rq re' $ dataQueue s }
            
      a <- gets acc
      
      when (comparison comp a val) $
        forM_ body (interpStmt exit)
            
        
        
  AST'Tst lhs comp rhs body -> do
    
    lq <- getRExp lhs
    le <- getNthQueue lq
    
    let (vall, le') = case le of
          Empty    -> (0, Empty)
          v :<| vs -> (v, vs)
          
    rq <- getRExp rhs
    re <- getNthQueue rq
    
    let (valr, re') = case re of
          Empty    -> (0, Empty)
          v :<| vs -> (v, vs)
          
    -- Update modified queues
    modify' $ \s ->
      s { dataQueue = Map.insert rq re' .
                      Map.insert lq le' $ dataQueue s }
          
    when (comparison comp vall valr) $
      forM_ body (interpStmt exit)
        
    
  AST'Die -> exit ()
  
  
  AST'Inc -> modify' $ \s -> s { acc = succ (acc s) }
  
  AST'Dec -> modify' $ \s -> s { acc = pred (acc s) }
  
  AST'Cue bs -> modify' $ \s -> s { callQueue = callQueue s :|> bs }