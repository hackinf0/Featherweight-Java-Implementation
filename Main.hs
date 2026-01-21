module Main where

import System.IO (hFlush, stdout)
import Data.List (isSuffixOf)
import Text.Parsec (parse)
import Text.Parsec.String (parseFromFile)

import Syntax (ClassTable, Expr, Name, className)
import Parser (classTableParser, exprParser)
import Typechecker (checkClassTable, typeOf, objectClass)
import Evaluator (step)
import qualified Data.Map as Map


type Context = Map.Map Name Name
type State = Maybe (ClassTable, Bool, Context)

main :: IO ()
main = do
  printWelcome
  replLoop Nothing

printWelcome :: IO ()
printWelcome = do
  putStrLn "Welcome to Featherweight Java!"
  printHelp

printHelp :: IO ()
printHelp = do
  putStrLn "Commands:"
  putStrLn "  :parse FILENAME.fj  -- parse a file"
  putStrLn "  :typecheck          -- typecheck the loaded class table"
  putStrLn "  :var NAME TYPE      -- declare a variable manually"
  putStrLn "  :ct                 -- show current class table"
  putStrLn "  :help               -- show this help"
  putStrLn "  :q                  -- quit"

replLoop :: State -> IO ()
replLoop state = do
  putStr "FJ> "
  hFlush stdout
  input <- getLine
  case words input of
    [":q"] -> putStrLn "Goodbye!"

    [":help"] -> do
      printHelp
      replLoop state

    [":ct"] -> do
      case state of
        Nothing -> putStrLn "No class table loaded."
        Just (ct, _, _) -> mapM_ print ct
      replLoop state

    [":typecheck"] -> do
      case state of
        Nothing -> putStrLn "No class table loaded."
        Just (ct, _, ctx) -> do
          case checkClassTable ct of
            Left err -> putStrLn ("Typecheck failed: " ++ err)
            Right () -> putStrLn "Typecheck succeeded!"
          replLoop (Just (ct, True, ctx))

    [":parse", filePath] | ".fj" `isSuffixOf` filePath -> do
      parsed <- parseFromFile classTableParser filePath
      case parsed of
        Left perr -> print perr >> replLoop state
        Right ct -> do
          putStrLn "Parsed! (but not typechecked yet)"
          replLoop (Just (objectClass:ct, False, Map.empty))

    [":var", varName, varType] ->
      case state of
        Nothing -> putStrLn "Error: Load a class table first (:parse)" >> replLoop state
        Just (ct, typed, ctx) -> do
          if varType `elem` map className ct
            then do
              putStrLn $ "Added variable " ++ varName ++ " :: " ++ varType
              let ctx' = Map.insert varName varType ctx
              replLoop (Just (ct, typed, ctx'))
            else do
              putStrLn $ "Error: Type " ++ varType ++ " not found in Class Table."
              replLoop state

    _ -> case state of
      Nothing -> putStrLn "Error: No class table loaded. Use :parse first." >> replLoop state
      Just (ct, isTyped, ctx) ->
        if not isTyped
          then putStrLn "Error: Class table not typechecked yet. Use :typecheck first." >> replLoop state
          else do
            case parse exprParser "<stdin>" input of
              Left perr -> print perr
              Right expr -> do
                putStrLn "Starting expression:"
                print expr
                let fullCtx = Map.insert "this" (className (head ct)) ctx
                case typeOf fullCtx ct expr of
                  Left terr -> putStrLn ("Type error: " ++ show terr)
                  Right ty -> do
                    putStrLn ("Type: " ++ ty)
                    evalLoop ct expr
            replLoop state

-- Evaluate the expression step by step
evalLoop :: ClassTable -> Expr -> IO ()
evalLoop ct expr = case step ct expr of
  Just expr' -> do
    putStrLn " => "
    print expr'
    evalLoop ct expr'
  Nothing -> do
    putStrLn "Result:"
    print expr

