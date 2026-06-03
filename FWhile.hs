{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- PROGRAMA PRINCIPAL DEL INTÉRPRETE DE FWHILE -}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)

import Control.Arrow (ArrowLoop(loop))
import Data.List (isSuffixOf)

import Parser
import AST
import PP
import Eval
import TypeChecker

-- Posibles flags
data Flag
  = FlagRepl
  | FlagPrettyPrint
  | FlagEval String
  | FlagCheck
  | FlagHelp

data Options = Options
  { optFile :: Maybe FilePath
  , optFlag :: Maybe Flag
  }

-- Programa principal
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      putStrLn err
      putStrLn usage
      exitFailure
    Right opts ->
      runWith opts


runWith :: Options -> IO ()
runWith opts =
  case optFlag opts of
    Just FlagHelp -> putStrLn usage
    Just flag ->
      case optFile opts of
        Nothing ->
          ejecutarFlag flag []
        Just file' -> do
          let file = if ".fw" `isSuffixOf` file' then file' else file' ++ ".fw"
          src <- readFile file
          case parseProg src of
            Left err -> print err
            Right prog ->
              case checkProg prog of
                Ok -> ejecutarFlag flag prog
                checkRes -> print checkRes

    Nothing ->
      repl []
  where
    ejecutarFlag :: Flag -> Prog -> IO ()
    ejecutarFlag flag prog =
      case flag of
        FlagPrettyPrint ->
          putStrLn (ppProg prog)

        FlagCheck ->
          print Ok

        FlagEval input ->
          case parseExp input of
            Left err -> print err
            Right e ->
              case checkExp prog e of
                Ok -> either print print $ evalExp prog [] e
                err -> print err

        FlagRepl ->
          repl prog

        FlagHelp ->
          putStrLn usage


repl :: Prog -> IO ()
repl prog = do
  putStrLn logo
  putStrLn "REPL. Use :q or :quit para salir."
  loop
  where
    loop = do
      putStr "> "
      hFlush stdout
      line <- getLine
      case line of
        ":q"    -> pure ()
        ":quit" -> pure ()
        _ ->
          case parseExp line of
            Left err -> print err >> loop
            Right e ->
              case checkExp prog e of
                Ok -> either print print (evalExp prog [] e) >> loop
                err -> print err >> loop
                

parseArgs :: [String] -> Either String Options
parseArgs args =
  case args of
    ["-h"] -> Right (Options Nothing (Just FlagHelp))
    ["--help"] -> Right (Options Nothing (Just FlagHelp))

    [] -> Right (Options Nothing (Just FlagRepl))

    ["-r"] -> Right (Options Nothing (Just FlagRepl))
    ["--repl"] -> Right (Options Nothing (Just FlagRepl))

    [file] -> Right (Options (Just file) (Just FlagRepl))

    [file, "-r"] -> Right (Options (Just file) (Just FlagRepl))
    [file, "--repl"] -> Right (Options (Just file) (Just FlagRepl))

    [file, "-p"] -> Right (Options (Just file) (Just FlagPrettyPrint))
    [file, "--prettyprint"] -> Right (Options (Just file) (Just FlagPrettyPrint))

    [file, "-c"] -> Right (Options (Just file) (Just FlagCheck))
    [file, "--check"] -> Right (Options (Just file) (Just FlagCheck))

    [file, "-e", expr] -> Right (Options (Just file) (Just (FlagEval expr)))
    [file, "--eval", expr] -> Right (Options (Just file) (Just (FlagEval expr)))

    _ -> Left "Argumentos inválidos."

usage :: String
usage =
  unlines
    [ "Uso:"
    , "  prog FILE -r"
    , "  prog FILE --repl"
    , "  prog FILE -p"
    , "  prog FILE --prettyprint"
    , "  prog FILE -c"
    , "  prog FILE --check"
    , "  prog FILE -e EXPR"
    , "  prog FILE --eval EXPR"
    , "  prog -h"
    , "  prog --help"
    ]

logo :: String
logo = unlines
  [ "_____________      __.__    .__.__          "
  , "\\_   _____/  \\    /  \\  |__ |__|  |   ____  "
  , " |    __) \\   \\/\\/   /  |  \\|  |  | _/ __ \\ "
  , " |     \\   \\        /|   Y  \\  |  |_\\  ___/ "
  , " \\___  /    \\__/\\  / |___|  /__|____/\\___  >"
  , "     \\/          \\/       \\/             \\/"
  ]
