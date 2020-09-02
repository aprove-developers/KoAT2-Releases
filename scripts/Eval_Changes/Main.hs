#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ megaparsec terminal-progress-bar extra async async-pool cpuinfo ])" -i runhaskell

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Process
import System.Directory.Extra
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.Tuple.Extra (both)
import Control.Concurrent.Async.Pool
import Control.Monad
import Control.Monad.Extra (ifM)
import System.Exit
import Data.List (isSuffixOf, sort, intercalate, delete, partition)
import Data.Functor ((<&>))
import Text.Megaparsec
import Text.Megaparsec.Char
import System.FilePath ((</>), (<.>), takeFileName)
import System.Directory (listDirectory, doesFileExist)
import qualified Text.Megaparsec.Char.Lexer as L
import System.Environment
import System.CPU

import System.ProgressBar

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

data Complexity = Timeout
                | Polynomial Int
                | Exp
                | Inf deriving (Show, Read)

prettyPrintComplexity :: Complexity -> String
prettyPrintComplexity Timeout = "TO"
prettyPrintComplexity Inf = "∞"
prettyPrintComplexity Exp = "EXP"
prettyPrintComplexity (Polynomial i) = "n^" ++ show i

type Parser = Parsec String String

type EvalDB = [(FilePath, Complexity)]

parseTilChar :: Char -> Parser String
parseTilChar c = takeWhileP (Just "character") (/= c)

parseOutput :: String -> Complexity
parseOutput out = fromRight $ parse parser "stdin" out
  where
    fromRight :: Either (ParseErrorBundle String String) Complexity -> Complexity
    fromRight (Left e) = error $ out ++ ": " ++ show e
    fromRight (Right r) = r

    parseAsympt = Exp <$ string "EXP"
              <|> Polynomial <$> (string "n^" *> L.decimal)
              <|> Polynomial 0 <$ L.decimal

    parser :: Parser Complexity
    parser =
      Inf <$ string "MAYBE"
      <|> string "WORST_CASE(" *> parseTilChar ',' *> char ',' *> space *> string "O(" *> parseAsympt <* string "))"


evalExample :: FilePath -> FilePath -> IO Complexity
evalExample koat ex =
  readProcessWithExitCode "timeout" ["300s", koat, "analyse", "-i", ex] ""
  <&> \(err,stdout,_) -> if err ==  ExitSuccess then parseOutput stdout else Timeout


-- all KoatFiles in the specified directory with relative paths
allKoATFiles :: FilePath -> IO [FilePath]
allKoATFiles dir = traverseFiles dir ""
  where
    traverseFiles dir prefix = do
      contents <- listDirectory dir
      concat <$> forM contents
        (\name ->
          let complname = dir </> name in
          ifM (doesFileExist complname)
            (pure $ [prefix </> name | ".koat" `isSuffixOf` name])
            (traverseFiles complname (prefix </> name)))

eval :: FilePath -> FilePath -> IO EvalDB
eval koat dir = do
  files <- sort <$> allKoATFiles dir
  let numExamples = length files

  cores <- logicalCores <$> getCPUs -- number of cores
  let jobs = ceiling $ fromIntegral cores / (2 :: Double)

  putStrLn $ show numExamples ++ " Examples using " ++ show jobs ++ " threads"

  let style = defStyle { stylePrefix = remainingTime renderDuration "Working"
                       , stylePostfix = exact <> msg " (" <> percentage <> msg ") "<> elapsedTime renderDuration }

  pb <- newProgressBar style 10 (Progress 0 numExamples ()) -- progressbar

  withTaskGroup jobs $ \tg -> forConcurrently tg files $ \name -> do
    !compl <- evalExample koat (dir </> name)
    incProgress pb 1
    pure (name, compl)

  where
    forConcurrently tg = flip (mapConcurrently tg)

dumpEval :: EvalDB -> FilePath -> IO ()
dumpEval db outputfile = writeFile outputfile $ intercalate "\n" ((\(ex, compl) -> show ex ++ "; " ++ show compl) <$> db) ++ "\n"

parseEval :: String -> EvalDB
parseEval = map parserLine . lines
  where
    parserLine s =
      -- skip "; "
      let (filepath, _:_:rest) = head $ reads s in
      let compl = read rest in
      (filepath, compl)

compResult :: Complexity -> Complexity -> Ordering
compResult a b = compare (compValue a) (compValue b)
  where
    compValue Inf = (3,0)
    compValue Timeout = (3,0)
    compValue Exp = (2,0)
    compValue (Polynomial i) = (1,i)

main :: IO ()
main = do
  (cmd:args) <- getArgs
  case cmd of
    "bench" ->
      case args of
        [koat, bench] -> do
          results <- eval koat bench
          dumpEval results (takeFileName koat <.> "csv")
          putStrLn "Done!"

        _ -> error "Syntax: bench koat_binary problem_database"

    "diff" ->
      case args of
        [oldfile, newfile] -> do
          old_results <- M.fromList . parseEval <$> readFile oldfile
          new_results <- M.fromList . parseEval <$> readFile newfile

          -- create a map of all examples from both DBs
          let joint_examples = S.union (M.keysSet old_results) (M.keysSet new_results)
          let combined_map = M.fromSet (\ex -> both (fromJust . M.lookup ex) (old_results, new_results)) joint_examples

          let regressions = [e | e@(ex,(o,n)) <- M.assocs combined_map, compResult o n == LT]
          let progressions = [e | e@(ex,(o,n)) <- M.assocs combined_map, compResult o n == GT]

          let regressions_str = foldl (\r_str (ex,(o,n)) -> r_str ++ concat ["\n",ex," from ",prettyPrintComplexity o," to ",prettyPrintComplexity n]) "" regressions
          let progressions_str = foldl (\r_str (ex,(o,n)) -> r_str ++ concat ["\n",ex," from ",prettyPrintComplexity o," to ",prettyPrintComplexity n]) "" progressions

          putStrLn $
            show (length $ M.assocs combined_map) ++ " combined examples\n"
            ++ "Regressions (" ++ show (length regressions) ++ "):" ++ regressions_str ++ "\n\n"
            ++ "Progressions (" ++ show (length progressions) ++ "):" ++ progressions_str



        _ -> error "Syntax diff oldfile newfile"


    _  -> error "Syntax: cmd cmdargs where cmd is either bench or diff"
