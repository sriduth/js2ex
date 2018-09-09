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
  | UnaryOp JSUnaryOp ExAST
  | UnaryExpression JSUnaryOp ExAST
  | BinaryExpression ExAST JSBinOp ExAST
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
    JSExpressionTernary cond _ trueExpr _ falseExpr -> IfElse (jse2ex cond) (jse2ex trueExpr) (jse2ex falseExpr)
    JSUnaryExpression operator expression -> UnaryExpression operator (jse2ex expression)
    JSExpressionBinary exp1 op exp2 -> BinaryExpression (jse2ex exp1) op (jse2ex exp2)
    JSExpressionParen _ expr _ -> Parens (jse2ex expr)
    JSFunctionExpression _ name' _ args _ body ->
      case jsIdentToName name' of
        Just name ->
          NamedFunction (name) (getFnArguments args) (Block (js2ex <$> (jsBlockIfElseFlow $ getJsStatementsFromBlock body)))
        _ -> AnonymousFunction (getFnArguments args) (Block (js2ex <$> (jsBlockIfElseFlow $ getJsStatementsFromBlock body)))

    JSVarInitExpression expression init' ->
      let rhs = case init' of
            JSVarInit _ expr' -> Just (jse2ex expr')
            _ -> Nothing
      in
        VarIntro (jse2ex expression) rhs
    z@(_) -> trace ("Expr not matched :: " <> show z) (Block [])



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
  return $ (astToEx 0 parsed)

makeIndent x = intercalate "" ([" " | _ <- [1..x]])

newlinesIndent statements ilvl =
  let indent = makeIndent ilvl in
  intercalate "\n" ((indent <> ) <$> statements)

squashNestedBlocks items =
  foldl (\squashed item ->
           case item of
             Block items' -> squashed ++ items'
             _ -> squashed ++ [item]
           ) [] items
    
astToEx indent ast =
  case ast of
    ModuleIntroduction name rest ->
      "defmodule " <> (T.unpack name) <> " " <> (astToEx indent rest)
    NamedFunction name args body ->
      let
        arguments = "(" <> (intercalate ", " (T.unpack <$> args)) <> ")"
      in
      "def " <> (T.unpack name) <> arguments  <> astToEx indent body
    AnonymousFunction args body ->
      let renderedBody =
            case body of
              Block statements -> newlinesIndent ((astToEx indent) <$> squashNestedBlocks statements) (indent + 1)
              _ -> astToEx indent body
          arguments = "(" <> (intercalate ", " (T.unpack <$> args)) <> ")"
      in
        "fn" <> arguments <> " -> \n" <> renderedBody <> "\nend"
    block@(Block binds') ->
      let binds = squashNestedBlocks binds' in
      "do\n" <> (newlinesIndent ((astToEx indent) <$> binds) (indent + 1)) <> "\nend"
    IfElse ifCond thens elses ->
      "if " <> (astToEx indent ifCond) <> " do\n"
      <> (case thens of
            Block stmts -> newlinesIndent ((astToEx indent) <$> stmts) (indent + 1)
            _ -> astToEx indent thens)
      <> "\nelse\n"
      <> (case elses of
            Block stmts -> newlinesIndent ((astToEx indent) <$> squashNestedBlocks stmts) (indent + 1)
            _ -> (astToEx indent) elses)
      <> "\nend"
    FunctionCall fnName args ->
      (T.unpack fnName) <> "(" <> (intercalate ", " (astToEx indent <$> args)) <> ")"
    Number number -> T.unpack number
    Identifier ident -> T.unpack ident
    Literal literal -> T.unpack literal
    StringLiteral literal -> "\"" <> (T.unpack literal) <> "\""
    ArrayDecl elements -> intercalate ", " (astToEx indent <$> elements)
    Accessor lhs rhs -> (astToEx indent lhs) <> "." <> (astToEx indent rhs)
    Indexer lhs rhs -> (astToEx indent lhs) <> "[" <> (astToEx indent rhs) <> "]"
    IndexerExpr lhs arrRhs -> (astToEx indent lhs) <> "(" <> (intercalate ", " (astToEx indent <$> arrRhs)) <> ")"
    BinaryOp lhs op rhs ->
      if infixBinOpToFn op
      then
        intercalate "" [(jsBinOpMap op), "(", (astToEx indent lhs), ", ", (astToEx indent rhs), ")"]
      else
        intercalate " " [(astToEx indent lhs), (jsBinOpMap op), (astToEx indent rhs)]
    UnaryExpression op expr -> (jsUnaryOpMap op) <> astToEx indent expr
    Parens ast -> "(" <> astToEx indent ast <> ")"
    Assignment lhs rhs -> (astToEx indent lhs) <> " = " <> (astToEx indent rhs)
    VarIntro var rhs -> (astToEx indent  var)
                        <> (case rhs of
                               Just init -> " = " <> astToEx indent init
                               _ -> "")

infixBinOpToFn op =
  case op of
    JSBinOpMod _ -> True
    _ -> False

jsUnaryOpMap op =
  case op of
    JSUnaryOpDecr _ -> "-"
    JSUnaryOpDelete _ -> "delete"
    JSUnaryOpIncr _ -> "+"
    JSUnaryOpMinus _ -> "-"
    JSUnaryOpNot _ -> "!"
    JSUnaryOpPlus _ -> "+"
    JSUnaryOpTilde _ -> "~"
    JSUnaryOpTypeof _ -> "typeof"
    JSUnaryOpVoid _ -> "void"
    
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
    JSBinOpMod _ ->  "rem"
    JSBinOpNeq _ -> "!="
    JSBinOpOr _ -> "||"
    JSBinOpPlus _ -> "+"
    JSBinOpRsh _ ->  ""
    JSBinOpStrictEq _ -> "=="
    JSBinOpStrictNeq _ -> "!="
    JSBinOpTimes _ ->  "*"
    JSBinOpUrsh _ -> ""
  
