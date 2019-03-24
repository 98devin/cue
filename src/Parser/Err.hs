{-# LANGUAGE
      FlexibleInstances
    , FlexibleContexts
    , MultiParamTypeClasses #-}

{- Author: Devin Hill (98devin@gmail.com) -}
    
module Parser.Err 
  ( ParseError(..)
  
  , PErr(..)
  , PWarn(..)
  
  , PErrorT(failWithContext)
  , failWith
  , (<?>)
  , (<??>)
  ) where
  
    
import Parser.Combinators
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Fail as Fail

import Control.Arrow (first, second)

import Data.Monoid

import Control.Exception


-------------------------------------------------------------------
--    Error Handling                                             --
-------------------------------------------------------------------

-- | Type representing a parser error
newtype ParseError = ParseError { errorMsg :: String }

instance Exception ParseError where
  -- no methods needed

-- | Implementation of displaying a parse error
instance Show ParseError where
  show (ParseError msg) =
    "error: Parse error: " ++ msg

    
-- | Errors which are unrecoverable and cannot produce output
newtype PErr e m a  = PErr  { runPErr  :: m (Either e a) }
  
-- | Errors which are merely warnings, and just accompany output
newtype PWarn e m a = PWarn { runPWarn :: m (e, a) }



-------------------------------------------------------------------
--    Instance Declarations (PErr)                               --
-------------------------------------------------------------------

instance Functor f => Functor (PErr e f) where
  fmap f = PErr . (fmap.fmap) f . runPErr
  
instance Applicative m => Applicative (PErr e m) where
  pure = PErr . pure . Right
  
  (PErr p1) <*> (PErr p2) = PErr $ liftA2 (<*>) p1 p2
  
instance Monad m => Monad (PErr e m) where
  return = pure
  
  p >>= f = PErr $ do
    r <- runPErr p
    case r of
      Left  e -> return (Left e)
      Right a -> runPErr (f a)
    
      
-- | And an accompanying alternative instance we need in order
--   to use it in a parser; Either by default doesn't supply one.
instance (Monoid e, Monad m) => Alternative (PErr e m) where
  empty = PErr $ return (Left mempty)
  
  p1 <|> p2 = PErr $ do
    r1 <- runPErr p1
    r2 <- runPErr p2
    return $ case (r1, r2) of
      (Left f1, Left f2) -> Left (f1 <> f2)
      (Right a, _      ) -> Right a
      (_      , Right a) -> Right a 
      

instance (Monoid e, Monad m, Alternative m) => MonadPlus (PErr e m) where
  -- use defaults

  
instance MonadIO m => MonadIO (PErr e m) where
  liftIO = PErr . fmap Right . liftIO
  
  
instance Fail.MonadFail m => Fail.MonadFail (PErr e m) where
  fail = PErr . fmap Left . Fail.fail
  
  
  
-------------------------------------------------------------------
--    Instance Declarations (PWarn)                              --
-------------------------------------------------------------------
  
instance Functor f => Functor (PWarn e f) where
  fmap f = PWarn . (fmap.second) f . runPWarn
  
  
instance (Applicative m, Monoid e) => Applicative (PWarn e m) where
  pure = PWarn . pure . (,) mempty
  
  (PWarn p1) <*> (PWarn p2) = PWarn $ 
    (\(e, f) (e2, a) -> (e <> e2, f a)) <$> p1 <*> p2
  
  
instance (Monoid e, Monad m) => Monad (PWarn e m) where
  return = pure
  
  p >>= f = PWarn $ do
    (e , a ) <- runPWarn p
    (e', a') <- runPWarn (f a) 
    return (e <> e', a')

    
instance (Monoid e, Alternative m) => Alternative (PWarn e m) where
  empty = PWarn empty

  p1 <|> p2 = PWarn $ runPWarn p1 <|> runPWarn p2
  
  
instance (Monoid e, Monad m, Alternative m) => MonadPlus (PWarn e m) where
  -- use defaults
  

instance (Monoid e, MonadIO m) => MonadIO (PWarn e m) where
  liftIO = PWarn . fmap ((,) mempty) . liftIO
   
  
instance (Monoid e, Fail.MonadFail m) => Fail.MonadFail (PWarn e m) where
  fail = PWarn . fmap ((,) mempty) . Fail.fail
  
  
-------------------------------------------------------------------
--    Provided Combinators/Helpers                               --
-------------------------------------------------------------------    
  
-- | A class representing a type `p` which, when parametrized
--   over `e` and `m`, forms a valid backing monad for a parser
--   which allows errors to be produced. 
class (Monad (p e m)) => PErrorT p e m where
  -- | Fail, and create the error message from the current state of the parser,
  --   allowing more detailed errors ideally.
  failWithContext :: (Monad m, Monoid e) => (s -> e) -> Parser s (p e m) a -> Parser s (p e m) a
  
  
instance (Monad m, Alternative m) => PErrorT PErr e m where
  failWithContext fe p = Parser $ \s -> runParser p s <|> (PErr . return . Left $ fe s)
    
  
instance (Monoid e, Monad m) => PErrorT PWarn e m where
  failWithContext fe p = Parser $ \s -> PWarn $ do
    (e, a) <- runPWarn $ runParser p s
    return (fe s <> e, a) -- this seems backwards, but is actually correct I think
  
-- | Reversed `failWithContext` synonym
(<??>) :: (Monoid e, Monad m, PErrorT p e m) => Parser s (p e m) a -> (s -> e) -> Parser s (p e m) a
(<??>) = flip failWithContext

-- | Attach a more enlightening error message to some parser from outside,
--   for example as a default fail state description. This value is not affected
--   by the context of the parser state.
failWith :: (Monoid e, Monad m, PErrorT p e m) => e -> Parser s (p e m) a -> Parser s (p e m) a
failWith = failWithContext . const

-- | Reversed synonym
(<?>) :: (Monoid e, Monad m, PErrorT p e m) => Parser s (p e m) a -> e -> Parser s (p e m) a
(<?>) = flip failWith 
