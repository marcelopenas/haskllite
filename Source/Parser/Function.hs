module Source.Parser.Function (parseFunction) where

import Source.Parser.Block (parseBlock)
import Source.Parser.Parser (Parser)
import Source.CompilerError (compilerParserError)
import Source.Lexer (LexerState, getNext)
import Source.Node (Node (FuncDec))
import Source.Token (Token (ARROW, CLOSE_PAR, COMMA, FN, IDENTIFIER, OPEN_PAR, TYPE, TYPE_ASSIGN), VarType (UnityT))

parseFunction :: Parser Node
parseFunction lexState@(lex, token) = case token of
  FN ->
    let afterFn = getNext lex
        (afterName, name) = expectIdentifier afterFn
        (afterOpenPar, _) = expectOpenPar afterName
        (afterArgs, args) = parseArgs afterOpenPar
        (afterClosePar, _) = expectClosePar afterArgs
        (afterReturn, returnType) = parseOptionalReturnType afterClosePar
        (afterBlock, block) = parseBlock afterReturn
     in (afterBlock, FuncDec name returnType args block)
  _ -> compilerParserError lexState "Expected function declaration"

expectIdentifier :: Parser String
expectIdentifier lexState@(lex, token) = case token of
  IDENTIFIER name -> (getNext lex, name)
  _ -> compilerParserError lexState "Expected identifier"

expectOpenPar :: Parser ()
expectOpenPar lexState@(lex, token) = case token of
  OPEN_PAR -> (getNext lex, ())
  _ -> compilerParserError lexState "Expected open paren"

expectClosePar :: Parser ()
expectClosePar lexState@(lex, token) = case token of
  CLOSE_PAR -> (getNext lex, ())
  _ -> compilerParserError lexState "Expected close paren"

parseArgs :: Parser [(String, VarType)]
parseArgs lexState@(lex, token) = case token of
  CLOSE_PAR -> (lexState, [])
  _ ->
    let (afterFirstArg, firstArg) = parseSingleArg lexState
        (afterRest, restArgs) = parseArgsRest afterFirstArg
     in (afterRest, firstArg : restArgs)

parseSingleArg :: Parser (String, VarType)
parseSingleArg lexState@(lex, token) = case token of
  IDENTIFIER argName ->
    let (afterTypeAssign, _) = expectTypeAssign (getNext lex)
        (afterType, TYPE typ) = expectType afterTypeAssign
     in (afterType, (argName, typ))
  _ -> compilerParserError lexState "Expected identifier for arg"

expectTypeAssign :: Parser ()
expectTypeAssign lexState@(lex, token) = case token of
  TYPE_ASSIGN -> (getNext lex, ())
  _ -> compilerParserError lexState "Expected ':'"

parseArgsRest :: Parser [(String, VarType)]
parseArgsRest lexState@(lex, token) = case token of
  COMMA ->
    let afterComma = getNext lex
        (afterArg, arg) = parseSingleArg afterComma
        (afterRest, rest) = parseArgsRest afterArg
     in (afterRest, arg : rest)
  _ -> (lexState, [])

parseOptionalReturnType :: Parser VarType
parseOptionalReturnType lexState@(lex, token) = case token of
  ARROW ->
    let (afterArrow, typ) = parseReturnType (getNext lex)
     in (afterArrow, typ)
  _ -> (lexState, UnityT)

parseReturnType :: Parser VarType
parseReturnType lexState@(lex, token) = case token of
  OPEN_PAR ->
    let (afterOpen, _) = expectClosePar (getNext lex)
     in (afterOpen, UnityT)
  _ ->
    let (afterType, TYPE typ) = expectType lexState
     in (afterType, typ)

expectType :: Parser Token
expectType lexState@(lex, token) = case token of
  TYPE typ -> (getNext lex, TYPE typ)
  _ -> compilerParserError lexState "Expected type"