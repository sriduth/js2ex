module Main where

import System.Process
import Lib
import Data.Text as T
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser

main :: IO String
main = do
  exFileText <- generateExFromJs "./test2.js"
  writeFile "test.ex" exFileText
  readProcess "mix" ["format", "test.ex"] ""
  
generateExFromJs :: String -> IO String
generateExFromJs filePath = do
  fileData <- parseFile filePath
  parsed <- return
    $ case fileData of
        JSAstProgram items _ -> ModuleIntroduction (T.pack "Module") (Block (js2ex <$> items))
        _ -> ModuleIntroduction (T.pack "ModEmpty") (Block [])
  return $ (astToEx 0 parsed)
