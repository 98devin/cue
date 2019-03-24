{-# LANGUAGE
      OverloadedStrings
    , RankNTypes
    , PatternSynonyms
    , LambdaCase
    , TemplateHaskell
    , GADTs
    , MultiParamTypeClasses
    , FlexibleInstances
    , FunctionalDependencies #-}

{- Author: Devin Hill (98devin@gmail.com) -}

module Cue.Interpreter where
  
import Control.Lens

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


data Call
  = NamedCall ByteString [Integer]
  | Closure [AST] Integer ProcInst
  deriving (Eq, Show)

  
data InterpState = IState
  { _callQueue :: Seq Call
  , _dataQueue :: Map Integer (Seq Integer)
  , _symbols   :: Symbols
  , _acc       :: Integer
  }
  deriving (Eq, Show)

makeFieldsNoPrefix ''InterpState
  
  
initialState :: Symbols -> [Integer] -> InterpState
initialState syms input = IState
  { _callQueue = Seq.fromList [ NamedCall "main" [] ]
  , _dataQueue = Map.fromList [ (0, Seq.fromList input) ]
  , _symbols   = syms
  , _acc       = 0
  }
  

type I' r a = StateT InterpState (ContT r IO) a
  
type I a = forall r. I' r a


interpret :: Symbols -> [Integer] -> IO InterpState
interpret syms input =
  (`runContT` return) $
    execStateT interp $
      initialState syms input


interp :: I ()
interp = use callQueue >>= \case
  Seq.Empty -> return ()
  
  call :<| cs -> do
    
    callQueue .= cs
    acc       .= 0
    syms <- use symbols
    
    case call of
      NamedCall procName args -> do
        let proc = Map.findWithDefault
                    (emptyProc procName)
                    procName
                    syms      
        let inst = makeInst proc args
        loop inst          
        
      Closure body accVal inst -> do
        acc .= accVal
        loop $ inst {instBody = body}
  
  where
    loop inst = 
      callCC $ \exit -> do
      callCC $ \next -> do
        interpCall next exit inst
      interp
      
        
interpCall :: (() -> I' r ()) -- next continuation, end procedure
           -> (() -> I' r ()) -- exit continuation, end program
           -> ProcInst
           -> I' r ()
interpCall next exit inst = do
  let interp' = interpStmt next exit inst
  forM_ (instBody inst) interp'
           
    
getNthQueue :: Integer -> I (Seq Integer)
getNthQueue n = 
  use $ dataQueue
      . to (Map.lookup n)
      . to (fromMaybe Seq.Empty)
  
      
getQueueVal :: Seq Integer -> (Integer, Seq Integer)
getQueueVal = \case
  Seq.Empty -> (0, Seq.Empty)
  v :<| vs  -> (v, vs)   

  
getRExp :: ProcInst -> RegExpr -> I Integer
getRExp inst = \case 
  Accumulator -> use acc
  Value n -> return n
  Var bs ->
    return $ Map.findWithDefault 0 bs (instArgs inst)
  Queue re ->
    getRExp' inst re

    
getRExp' :: ProcInst -> RegExpr -> I Integer
getRExp' inst = \case
  Accumulator -> use acc
  Value n -> return n
  Var bs ->
    return $ Map.findWithDefault 0 bs (instArgs inst)
  Queue re -> do
    q <- getRExp' inst re
    e <- getNthQueue q

    let (val, e') = getQueueVal e
    
    dataQueue %= Map.insert q e'
          
    return val
    

interpStmt :: (() -> I' r ()) -- next continuation, end procedure
           -> (() -> I' r ()) -- exit continuation, end program
           -> ProcInst        -- the proc within which we are working
           -> AST -> I' r ()
interpStmt next exit inst = 
  let rExp  = getRExp  inst
      rExp' = getRExp' inst 
  in \case    
  AST'Put re -> do
    q <- rExp re
    e <- getNthQueue q
    a <- use acc
    
    dataQueue %= Map.insert q (e :|> a)
    
    
  AST'Com com re -> do
    val <- rExp' re
    acc %= flip (combination com) val
     
    
  AST'Tst Accumulator comp rhs body -> do
        
      rq <- rExp rhs
      re <- getNthQueue rq
      
      let (val, re') = getQueueVal re
            
      dataQueue %= Map.insert rq re'
            
      a <- use acc
      
      when (comparison comp a val) $
        forM_ body (interpStmt next exit inst)
        
        
  AST'Tst lhs comp rhs body -> do
    
    lq <- rExp lhs
    le <- getNthQueue lq
    
    let (vall, le') = getQueueVal le
    
    rq <- rExp rhs
    re <- getNthQueue rq
    
    let (valr, re') = getQueueVal re      
    
    dataQueue %=
        Map.insert rq re'
      . Map.insert lq le'
        
    when (comparison comp vall valr) $
      forM_ body (interpStmt next exit inst)
      
  AST'Die -> next ()
  
  AST'End -> exit ()
  
  AST'Inc -> acc %= succ
    
  AST'Dec -> acc %= pred
    
  AST'Cue bs args -> do
    args' <- mapM rExp args 
    callQueue %= (|> NamedCall bs args')
  
  AST'Cbl bl -> do
    ac <- use acc
    callQueue %= (|> Closure bl ac inst)
  