module Parser (classTableParser, exprParser) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Expr as E

import Syntax

-- Token settings
lexer :: P.TokenParser ()
lexer = P.makeTokenParser fjDef

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
parens     = P.parens lexer
semi       = P.semi lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
braces     = P.braces lexer
reservedOp = P.reservedOp lexer

-- LanguageDef
-- In FJ we don't have comments but...
fjDef :: LanguageDef st
fjDef = LanguageDef
  { commentStart = "/*"
  , commentEnd   = "*/"
  , commentLine  = "//"
  , nestedComments = False
  , identStart   = letter
  , identLetter  = alphaNum <|> char '_'
  , opStart      = oneOf "."
  , opLetter     = oneOf "."
  , reservedNames = ["class", "extends", "super", "return", "new", "Object", "this"]
  , reservedOpNames = ["."]
  , caseSensitive = True
  }

-- Class Head
classHeadParser =
  do { reserved "class"
     ; className <- identifier
     ; reserved "extends"
     ; classSuperClass <- typeParser
     ; (classFields, classConstructor, classMethods) <- braces classBodyParser
     ; return $ Class className classSuperClass classFields classConstructor classMethods
     }

classBodyParser =
  do { classFields <- many (try fieldParser) <|> return []
     ; classConstructor <- constructorParser
     ; classMethods <- many methodParser
     ; return (classFields, classConstructor, classMethods)
     }

fieldParser =
  do { fieldType <- typeParser
     ; fieldName <- identifier
     ; semi
     ; return $ Field fieldType fieldName
     }

typeParser = identifier
         <|> do { reserved "Object"; return "Object" }

constructorParser =
  do { constructorName <- identifier
     ; constructorArgs <- parens constructorArgumentParser
     ; (superParams, constructorAssignments) <- braces constructorBodyParser
     ; return $ Constructor constructorName constructorArgs superParams constructorAssignments
     }


constructorBodyParser =
  do { reserved "super"
     ; superParams <- parens $ sepBy identifier $ symbol ","
     ; semi
     ; constructorAssignments <- many assignmentParser
     ; return (superParams, constructorAssignments)
     }

assignmentParser =
  do { reserved "this"
     ; symbol "."
     ; classField <- identifier
     ; symbol "="
     ; constructorArgument <- identifier
     ; semi
     ; return (classField, constructorArgument)
     }

methodParser =
  do { methodType <- typeParser
     ; methodName <- identifier
     ; methodArgs <- parens methodArgumentParser
     ; expression <- braces (do { reserved "return"
                                ; expr <- exprParser
                                ; semi
                                ; return expr })
     ; return $ Method methodType methodName methodArgs expression
     }


-- For constructor (returns [Field])
constructorArgumentParser = sepBy p $ symbol ","
  where
    p = do { argumentType <- typeParser
           ; argumentName <- identifier
           ; return $ Field argumentType argumentName
           }

-- For method (returns [(Type, Name)])
methodArgumentParser = sepBy p $ symbol ","
  where
    p = do { argumentType <- typeParser
           ; argumentName <- identifier
           ; return (argumentType, argumentName)
           }


exprParser = E.buildExpressionParser table term

table :: E.OperatorTable Char () Expr
table = [[E.Infix (reservedOp "." >> return makeExpr) E.AssocLeft]]
  where
    makeExpr e1 (MethodInvoke _ e2name e2exprs) = MethodInvoke e1 e2name e2exprs
    makeExpr e1 (Var e2name) = FieldAccess e1 e2name
    makeExpr _ _ = error "Invalid expression at dot operator."

term  = exprNewParser
   <|> try exprCastParser
   <|> parens exprParser
   <|> try exprMethodParser
   <|> exprOtherParser

exprNewParser =
  do { reserved "new"
     ; exprType <- typeParser
     ; exprs <- parens $ sepBy exprParser $ symbol ","
     ; return $ New exprType exprs
     }

exprCastParser =
  do { castType <- parens typeParser
     ; expr <- exprParser
     ; return $ Cast castType expr
     }

exprMethodParser =
  do { method <- identifier
     ; exprs <- parens $ sepBy exprParser $ symbol ","
     ; return $ MethodInvoke (Var "_") method exprs
     }

exprOtherParser =
  do { raw <- identifier <|> do { reserved "this"; return "this" }
     ; return $ Var raw
     }

classTableParser =
  do { whiteSpace
     ; ct <- many classHeadParser
     ; eof
     ; return ct
     }
