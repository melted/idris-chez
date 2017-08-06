module Main where

import Idris.Core.TT
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL
import Idris.Options
import Idris.Main

import IRTS.CodegenCommon
import IRTS.Compiler
import IRTS.CodegenChez

import System.Environment
import System.Exit

import Paths_idris_chez

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath,
                   outTy :: OutputType }

showUsage = do putStrLn "Usage: idris-codegen-chez <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "a.ss" Executable) xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts ("-S":xs) = process (opts { outTy = Raw }) xs
    process opts ("-c":xs) = process (opts { outTy = Object }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

build :: Opts -> Idris ()
build opts = do elabPrims
                loadInputs (inputs opts) Nothing
                mainProg <- elabMain
                ir <- compile (Via IBCFormat "chez") (output opts) (Just mainProg)
                runIO $ codegenChez (ir { outputType = outTy opts })

main :: IO ()
main = do opts <- getOpts
          if null (inputs opts)
             then showUsage
             else runMain (build opts)
