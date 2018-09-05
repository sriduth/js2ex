{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Prelude
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Debug.Trace 
import qualified Data.Text as T
import Data.List (intercalate)

data ExAST
  = ModuleIntroduction T.Text ExAST
  | NamedFunction T.Text [T.Text] ExAST
  | AnonymousFunction [T.Text] ExAST
  | Block [ExAST]
  | IfElse ExAST ExAST ExAST
  deriving (Eq, Show) 



fromCommaList :: JSCommaList a -> [a]
fromCommaList (JSLCons l _ i) = fromCommaList l ++ [i]
fromCommaList (JSLOne i)      = [i]
fromCommaList JSLNil = []

jsIdentToName ident =
  case ident of
    JSIdentName _ name -> Just $ T.pack name
    _ -> Nothing

getIdentName ident =
  case ident of
    JSIdentName _ name -> T.pack name

noa = JSNoAnnot

jsBlockIfElseFlow statements =
  case statements of
    (x:xs) -> case x of
                JSIf a b exp c statement -> [JSIfElse noa noa exp noa statement c (JSStatementBlock a (jsBlockIfElseFlow xs) b JSSemiAuto)]
                _ -> statements
    _ -> statements
                
getJsStatementsFromBlock (JSBlock _ statements _) = statements

-- | Transform expressions of javascript to Elixir Syntax
jse2ex expr = Block []

-- | Transform js statements to elixir  syntax.
js2ex jsAst =
  case jsAst of
    JSStatementBlock _ stmts _ _ -> Block (js2ex <$> stmts)
    JSBreak _ _ _ -> Block []
    JSConstant _ list _ -> Block []
    JSIfElse _ _ expr _  ifcond _ elseCond-> IfElse (jse2ex expr) (js2ex ifcond) (js2ex elseCond)
    --fn,name, lb,parameter list,rb,block,autosemi
--    call@(JSMethodCall expr _ args ann _) -> 
    fun@(JSFunction _ name _ parameterlist _ block autosemi) ->
      case jsIdentToName name of
        Just name -> NamedFunction (name) (getIdentName <$> fromCommaList parameterlist)
          (Block (js2ex <$> (jsBlockIfElseFlow $ getJsStatementsFromBlock block)))
    --for,lb,var,vardecl,semi,expr,semi,expr,rb,stmt
    for@(JSForVar _ _ start cond _ mod _ expr _ loopBody) ->
      let inits = loopBody in
      Block ([js2ex inits])
    z@(_) ->  (Block [])
    
someFunc :: IO String
someFunc = do
  fileData <- parseFile "./test.js"
  parsed <- return $ case fileData of
                       JSAstProgram items _ -> ModuleIntroduction (T.pack "Module") (Block (js2ex <$> items))
                       _ -> ModuleIntroduction (T.pack "ModEmpty") (Block [])
  return $ astToEx parsed 

makeIndent x = intercalate "" ([" " | _ <- [1..x]])

astToEx ast =
  case ast of
    ModuleIntroduction name rest ->
      "defmodule" <> (T.unpack name) <> "\n" <> (astToEx rest)
    NamedFunction name args body ->
      "def" <> (T.unpack name) <> "\n" <> astToEx body
    AnonymousFunction args body ->
      "fn -> \n" <> astToEx body
    Block binds -> intercalate "\n" $ astToEx <$> binds
    IfElse ifCond thens elses ->
      "if" <> (astToEx ifCond) <> "\n"
      <> astToEx thens
      <> "\nelse\n"
      <> astToEx elses
  
