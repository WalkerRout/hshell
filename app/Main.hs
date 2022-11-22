module Main where

import System.Directory
import Data.Char
import System.Process
import Control.Concurrent
import System.IO
import Control.Exception

data ExitCode = SUCCESS | FAILURE deriving (Show, Eq, Ord)
data CmdStatus = CUSTOM | INBUILT deriving (Show, Eq, Ord)

eval :: String -> IO ()
eval input = do
  flag <- checkCustom input
  case flag of
    CUSTOM -> callCustom input
    INBUILT -> do
      m <- newEmptyMVar
      forkIO $ forkAction m
      res <- takeMVar m
      return ()
  where
    forkAction :: MVar String -> IO ()
    forkAction m = do
        try $ callCommand input :: IO (Either SomeException ())
        putMVar m "Done" -- add exception handling for atomic value ^

    checkCustom :: String -> IO CmdStatus
    checkCustom = return . custom
  
    custom :: String -> CmdStatus
    custom str = case (map toUpper . head . words $ str) of
      "TEST" -> CUSTOM
      "CD" -> CUSTOM
      "CLEAR" -> CUSTOM
      _ -> INBUILT
    
    callCustom :: String -> IO ()
    callCustom cmd = case (map toUpper . head . words $ cmd) of
      "TEST" -> testAction
      "CD" -> cdAction (unwords . tail . words $ cmd)
      "CLEAR" -> clearAction

testAction :: IO ()
testAction = putStrLn "Test Action..."

cdAction :: String -> IO ()
cdAction newDir = do
  dir <- getCurrentDirectory
  res <- try $ setCurrentDirectory (dir ++ "/" ++ newDir) :: IO (Either SomeException ())
  case res of
    Left a -> putStrLn $ "Error: " ++ show a
    Right b -> return ()

clearAction :: IO ()
clearAction = putStr "\ESC[H\ESC[2J"

repl :: IO (Either String ExitCode)
repl = do
  dir <- getCurrentDirectory
  putStr $ dir ++ "/> "
  hFlush stdout
  input <- getLine -- withEcho False getLine
  case (map toUpper input) of
    "EXIT" -> return $ Right SUCCESS
    "" -> repl
    _ -> do
      eval input
      repl
  where
    withEcho :: Bool -> IO a -> IO a
    withEcho echo action = do
      old <- hGetEcho stdin
      bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

main :: IO ()
main = do
  res <- repl
  case res of
    Left a -> putStrLn $ "Error: " ++ a
    Right b -> putStrLn $ "Thank you!\n"
