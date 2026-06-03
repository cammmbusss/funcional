{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- PARSER DE PROGRAMAS -}
module Parser where

import AST
import Text.Parsec

import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)


-- Parser de programas
parseProg :: String -> Either ParseError Prog
parseProg s = do
  parse (funP `sepEndBy1` (semiP *> pure ()) <* eof) "" s

-- Parser de expresiones
parseExp :: String -> Either ParseError Exp
parseExp = parse (expP <* eof) ""


-- Definición del lenguaje
langDef :: Tok.LanguageDef ()
langDef = emptyDef
  { Tok.commentLine = "#"
  , Tok.identStart = letter
  , Tok.identLetter = alphaNum <|> char '_'
  , Tok.reservedNames =
      [ "case","of","while","if","then","else"
      , "True","False","nil","Nil","_", "Cons", "fun"
      , "head", "tail"
      ]
  , Tok.caseSensitive = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

identP :: Parser Id
identP = Tok.identifier lexer

reservedP :: String -> Parser ()
reservedP = Tok.reserved lexer

parensP :: Parser a -> Parser a
parensP = Tok.parens lexer

symbolP :: String -> Parser String
symbolP = Tok.symbol lexer

bracesP :: Parser a -> Parser a
bracesP = Tok.braces lexer

bracketsP :: Parser a -> Parser a
bracketsP = Tok.brackets lexer

semiP :: Parser String
semiP = Tok.semi lexer

commaP :: Parser String
commaP = Tok.comma lexer

-- Parser de funciones
funP :: Parser Fun
funP = do
  _ <- reservedP "fun" 
  name <- identP
  inVar <- identP
  stmts <- bracesP stmtsP
  out <- expP
  pure (Fun name inVar stmts out)

stmtsBracesP :: Parser [Stmt]
stmtsBracesP = bracesP stmtsP

stmtsP :: Parser Stmts
stmtsP = do
  ms <- cmdP `sepEndBy` semiP
  _ <- optionMaybe semiP
  pure ms

-- Parser de instrucción
cmdP :: Parser Stmt
cmdP =try caseP
  <|> try assignP
  <|> whileP
  <|> ifP

assignP :: Parser Stmt
assignP = do
  v <- identP
  _ <- symbolP ":="
  e <- expP
  pure (Assign v e)

whileP :: Parser Stmt
whileP = do
  reservedP "while"
  cond <- expP
  body <- bracesP stmtsP
  pure (While cond body)

ifP :: Parser Stmt
ifP = do
  reservedP "if"
  cond <- expP
  reservedP "then"
  thenBody <- bracesP stmtsP
  elseBody <- reservedP "else" *> bracesP stmtsP
  pure (If cond thenBody elseBody)

caseP :: Parser Stmt
caseP = do
  reservedP "case"
  e <- expP
  reservedP "of"
  clauses <- bracesP (many1 clauseP)
  pure (Case e clauses)


clauseP :: Parser Clause
clauseP = do
  p <- patternP
  _ <- symbolP "->"
  body <-  (stmtsBracesP <* semiP) <|>
    (do c <- cmdP
        _ <- semiP
        pure [c])
  pure (Clause p body)

-- patterns
patternP :: Parser Pattern
patternP =
      parensP patternP
  <|> (PVar <$> Tok.identifier lexer)
  <|> (PLitN <$> Tok.natural lexer)
  <|> (PLitB <$> (reservedP "True"  *> pure True))
  <|> (PLitB <$> (reservedP "False" *> pure False))
  <|> (PNil <$ reservedP "Nil")
  <|> (PCons <$> (reservedP "Cons" *> patternP) <*> patternP)


-- Parser de expresiones
expP :: Parser Exp
expP = parseOr

-- ||
parseOr :: Parser Exp
parseOr = chainl1 parseAnd (bin "||" (BinOp Or))

-- &&
parseAnd :: Parser Exp
parseAnd = chainl1 parseRel (bin "&&" (BinOp And))

-- == , <
parseRel :: Parser Exp
parseRel = chainl1 parseAdd relOp
  where
    relOp =  (bin "==" (BinOp Equ))
         <|> (bin "<"  (BinOp Lt))

-- + -
parseAdd :: Parser Exp
parseAdd = chainl1 parseMul
  (bin "+" (BinOp Add) <|> bin "-" (BinOp Sub))

-- * / %
parseMul :: Parser Exp
parseMul = chainl1 parseUnary (  bin "*" (BinOp Times)
                             <|> bin "/" (BinOp Div)
                             <|> bin "%" (BinOp Mod)
                             )

-- unary: - !
parseUnary :: Parser Exp
parseUnary =
      (do _ <- symbolP "-"
          e <- parseUnary
          pure (UnOp Minus e))
  <|> (do _ <- symbolP "!"
          e <- parseUnary
          pure (UnOp Not e))
  <|> try parseApp
  <|> parseCons
  <|> parseAtom

-- llamada de función
parseApp :: Parser Exp
parseApp = do
  f <- identP
  x <- parseAtom
  pure (Call f x)

-- Cons
parseCons :: Parser Exp
parseCons = do
  _ <- reservedP "Cons"
  a <- parseSimpleAtom
  b <- parseSimpleAtom
  pure (Cons a b)

-- literales, variables y paréntesis
parseSimpleAtom :: Parser Exp
parseSimpleAtom =
      (LitN <$> Tok.natural lexer)
  <|> (reservedP "True"  *> pure (LitB True))
  <|> (reservedP "False" *> pure (LitB False))
  <|> (reservedP "Nil"   *> pure Nil)
  <|> (Var <$> identP)
  <|> parensP expP
  
parseAtom :: Parser Exp
parseAtom =
      (LitN <$> Tok.natural lexer)
  <|> (reservedP "True"  *> pure (LitB True))
  <|> (reservedP "False" *> pure (LitB False))
  <|> (reservedP "Nil"   *> pure Nil)
  <|> hdP
  <|> tlP
  <|> (Var <$> identP)
  <|> parensP expP
  where
    hdP = do
      reservedP "head"
      e <- parseAtomOrExp
      pure (Head e)

    tlP = do
      reservedP "tail"
      e <- parseAtomOrExp
      pure (Tail e)

    callP = do
      f <- identP
      x <- expP
      pure (Call f x)
    parseAtomOrExp = expP

bin :: String -> (Exp -> Exp -> Exp) -> Parser (Exp -> Exp -> Exp)
bin op f = f <$ symbolP op

  
