{-# LANGUAGE
      RecordWildCards
    , ScopedTypeVariables
    , LambdaCase #-}

{- Author: Devin Hill (dhill45@jhu.edu) -}
      
module Main where
  
import System.Environment
import System.Console.GetOpt
import System.Exit

import System.IO

import Control.Exception
import Control.Monad (unless, when)

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import Data.List (foldl')
import Data.Char (ord)

import Parser.Pos

  
-------------------------------------------------------------------
--    Primary command-line interface to SIMPLE compiler          --
-------------------------------------------------------------------

-- | Main application entry point
main :: IO ()
main = do
  -- Parse CLI args into options and get a bytestring
  -- representing the actual input as well
  (CLIOptions{..}, input) <- handleArgs =<< getArgs 
  
  print input
  
  exitSuccess
    
  
-- | A useful combinator for slightly better syntax when
--   using on monadic computation results; works well with LambdaCase.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
    
  
-- | A datatype with a field for each option we might want to consider
--   as a command line argument or flag.
data CLIOptions = CLIOptions
  { optStringInput :: Bool
  , optListInput   :: Bool
  }

  
-- | The default state of each option the CLIOptions object specifies.
defaultOptions :: CLIOptions
defaultOptions = CLIOptions
  { optStringInput = False
  , optListInput   = True
  }
  
  
-- | A specification of what command line options are available,
--   and how each option modifies the CLIOptions object if it is present.
--   Many or all of these options will probably just end up being toggles, here.
cliFlags :: [OptDescr (CLIOptions -> CLIOptions)]
cliFlags =
  [ Option ['s'] []
      (NoArg $ \o -> o { optStringInput = True
                       , optListInput   = False }) 
      "use the cli args as string contents of the input queue"
  ]
  
  
-- | Process the command line arguments received,
--   and produce an object representing the options provided
--   as well as the bytes which the intended file contains (be it stdin or some other)
handleArgs :: [String] -> IO (CLIOptions, [Integer])
handleArgs argv = case getOpt RequireOrder cliFlags argv of
  (o, otherOpts, []) -> do
    
    -- modify options according to each command line flag
    let opts@CLIOptions{..} = foldl' (flip id) defaultOptions o
    
    -- load a bytestring from either stdin or a file,
    -- checking for potential errors (no such file, etc.)
    otherOpts <- case otherOpts of
        
        -- If there is no command line input, read from stdin
        [] -> words . B.unpack <$> B.getContents       
        
        -- Otherwise use what's given
        params -> return params
        
        
    if optStringInput then
        return (opts, map (toInteger . ord) . unwords $ otherOpts)
    else if optListInput then
        return (opts, map read otherOpts)
    else
        die "what in tarnation"
      
  (_, _, errors) -> do
    -- end program and print out each error
    die $ concatMap ("error: " ++) errors