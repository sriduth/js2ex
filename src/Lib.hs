{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Prelude
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import qualified Debug.Trace as Tr
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)

trace = Tr.trace
--trace x y = y


data ExAST
  = ModuleIntroduction T.Text ExAST
  | NamedFunction T.Text [T.Text] ExAST
  | AnonymousFunction [T.Text] ExAST
  | Block [ExAST]
  | IfElse ExAST ExAST ExAST
  | FunctionCall T.Text [ExAST]
  | Number T.Text
  | Identifier T.Text
  | Literal T.Text
  | StringLiteral T.Text
  | ArrayDecl [ExAST]
  | Accessor ExAST ExAST
  | Indexer ExAST ExAST
  | IndexerExpr ExAST [ExAST]
  | BinaryOp ExAST JSBinOp ExAST
  | Parens ExAST
  | Assignment ExAST ExAST
  | VarIntro ExAST (Maybe ExAST)
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
                JSIf a b exp c statement ->
                  [JSIfElse noa noa exp noa statement c (JSStatementBlock a (jsBlockIfElseFlow xs) b JSSemiAuto)]
                _ -> statements
    _ -> statements
                
getJsStatementsFromBlock (JSBlock _ statements _) = statements

getFnArguments argsList = (\x -> case x of
                                   JSIdentName _ name -> T.pack name) <$> fromCommaList argsList

-- | Transform expressions of javascript to Elixir Syntax
jse2ex expr =
  case expr of
    JSDecimal _ str -> Number (T.pack str)
    JSIdentifier _ ident -> Identifier (T.pack ident)
    JSLiteral _ literal -> Literal (T.pack literal)
    JSStringLiteral _ literal -> StringLiteral (T.pack literal)
    JSArrayLiteral _ arrayContents _ -> ArrayDecl (foldl (\agg item ->
                                                      case item of
                                                        JSArrayElement expr -> agg ++ [(jse2ex expr)]
                                                        _ -> agg
                                                   ) [] arrayContents)
    JSMemberDot first _ key -> Accessor (jse2ex first) (jse2ex key)
    JSMemberExpression expr _ members _ -> IndexerExpr (jse2ex expr) (jse2ex <$> (fromCommaList members))
    JSExpressionBinary lhs binOp rhs -> BinaryOp (jse2ex lhs) binOp (jse2ex rhs)
    JSExpressionParen _ expr _ -> Parens (jse2ex expr)
    JSFunctionExpression _ name' _ args _ body ->
      let name = case jsIdentToName name' of
                   Just name -> name
                   _ -> ""
      in 
      NamedFunction (name) (getFnArguments args) (Block (js2ex <$> (jsBlockIfElseFlow $ getJsStatementsFromBlock body)))
    JSVarInitExpression expression init' ->
      let rhs = case init' of
            JSVarInit _ expr' -> Just (jse2ex expr')
            _ -> Nothing
      in
        VarIntro (jse2ex expression) rhs
    _ -> Block []



isAssignmentExport js =
  case js of
    JSAssignStatement (JSMemberDot (JSIdentifier _ "exports") _  (JSIdentifier _ fnName)) _ rhs _ -> Just (fnName, rhs)
    _ -> Nothing
    
-- | Transform js statements to elixir  syntax.
js2ex jsAst =
  case jsAst of
    JSVariable _ decls _ -> Block $ jse2ex <$> (fromCommaList decls )
    assign@(JSAssignStatement lhs assignOp rhs _) ->
      case isAssignmentExport assign of
        Just (fnName, body) ->
          let (JSFunctionExpression f' name lb' params rb' block) = body in
            jse2ex (JSFunctionExpression f' (JSIdentName f' fnName) lb' params rb' block)
        _ -> Block []
      --trace ("Is assignmen export :: " <> show (isAssignmentExport assign)) (Block [])
          
    JSStatementBlock _ stmts _ _ -> Block (js2ex <$> stmts)
    JSBreak _ _ _ -> Block []
    JSConstant _ list _ -> Block []
    JSIfElse _ _ expr' _  ifcond _ elseCond ->
      let expr = expr' in 
      IfElse (jse2ex expr) (js2ex ifcond) (js2ex elseCond)
    call@(JSMethodCall (JSIdentifier _ fnName) a args' b c) ->
      let args = jse2ex <$> (fromCommaList args')
      in
      FunctionCall (T.pack fnName) (trace ("Function arguments" <> show args) args)
      --trace ("dala :: \n" <> (show $ fr omCommaList args)) (Block [])
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
    JSReturn _ returnValue _ ->
      case returnValue of
        Just val -> jse2ex val
        _ -> Block []
    z@(_) ->  trace ("Nothing was matched :: \n" <> show z <> "\n") (Block [])
    
someFunc :: IO String
someFunc = do
  fileData <- parseFile "./test2.js"
  parsed <- return $ case fileData of
                       JSAstProgram items _ -> ModuleIntroduction (T.pack "Module") (Block (js2ex <$> items))
                       _ -> ModuleIntroduction (T.pack "ModEmpty") (Block [])
  return $ astToEx parsed 

makeIndent x = intercalate "" ([" " | _ <- [1..x]])

astToEx ast =
  case ast of
    ModuleIntroduction name rest ->
      "defmodule " <> (T.unpack name) <> " " <> (astToEx rest)
    NamedFunction name args body ->
      "def " <> (T.unpack name) <> " " <> astToEx body
    AnonymousFunction args body ->
      "fn -> \n" <> astToEx body
    Block binds -> "do\n" <> (intercalate "\n" $ astToEx <$> binds) <> "\nend"
    IfElse ifCond thens elses ->
      "if " <> (astToEx ifCond) <> " "
      <> astToEx thens
      <> "\nelse\n"
      <> astToEx elses
    FunctionCall fnName args ->
      (T.unpack fnName) <> "(" <> (intercalate ", " (astToEx <$> args)) <> ")"
    Number number -> T.unpack number
    Identifier ident -> T.unpack ident
    Literal literal -> T.unpack literal
    StringLiteral literal -> "\"" <> (T.unpack literal) <> "\""
    ArrayDecl elements -> intercalate ", " (astToEx <$> elements)
    Accessor lhs rhs -> (astToEx lhs) <> "." <> (astToEx rhs)
    Indexer lhs rhs -> (astToEx lhs) <> "[" <> (astToEx rhs) <> "]"
    IndexerExpr lhs arrRhs -> (astToEx lhs) <> "(" <> (intercalate ", " (astToEx <$> arrRhs)) <> ")"
    BinaryOp lhs op rhs ->
      intercalate " " [(astToEx lhs), (jsBinOpMap op), (astToEx rhs)]
    Parens ast -> "(" <> astToEx ast <> ")"
    Assignment lhs rhs -> (astToEx lhs) <> " = " <> (astToEx rhs)
    VarIntro var rhs -> (astToEx var) <> (case rhs of
                                            Just init -> " = " <> astToEx init
                                            _ -> "")
jsBinOpMap binOp =
  case binOp of
    JSBinOpAnd _ -> "and"
    JSBinOpBitAnd _ -> "&&&" 
    JSBinOpBitOr _ -> "|||"
    JSBinOpBitXor _ -> "^^^"
    JSBinOpDivide _ -> "/"
    JSBinOpEq _ -> "=="
    JSBinOpGe _ -> ">="
    JSBinOpGt _ -> ">"
    JSBinOpIn _ -> "in"
    JSBinOpInstanceOf _ -> ".__struct__ ="
    JSBinOpLe _ -> "<="
    JSBinOpLsh _ -> ""
    JSBinOpLt _ -> "<"
    JSBinOpMinus _ -> "-"
    JSBinOpMod _ ->  "%"
    JSBinOpNeq _ -> "!="
    JSBinOpOr _ -> "||"
    JSBinOpPlus _ -> "+"
    JSBinOpRsh _ ->  ""
    JSBinOpStrictEq _ -> "=="
    JSBinOpStrictNeq _ -> "!="
    JSBinOpTimes _ ->  "*"
    JSBinOpUrsh _ -> ""
  
