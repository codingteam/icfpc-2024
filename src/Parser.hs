module Parser (parseExpression) where

import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

import AST
import Lib
import Strings

type Parser a = Parsec Void T.Text a

pExpression :: Parser AST
pExpression =
        pBoolean
    <|> pNumber
    <|> pString
    <|> pUnary
    <|> pBinary
    <|> pIf
    <|> pVar
    <|> pLambda

pBoolean :: Parser AST
pBoolean = do
    r <- (string "T" >> return (Boolean True)) <|> (string "F" >> return (Boolean False))
    space
    return r

pNumberLit :: Parser Integer
pNumberLit = do
    str <- takeWhileP Nothing (/= ' ')
    space
    case parseNumber str of
        Nothing -> fail "not a number"
        Just n -> return n

pNumber :: Parser AST
pNumber = do
    single 'I'
    Number <$> pNumberLit

pString :: Parser AST
pString = do
    single 'S'
    str <- takeWhileP Nothing (/= ' ')
    space
    return $ Str $ textFromGalaxy str

pUnary :: Parser AST
pUnary = do
        single 'U'
        r <- pNegate <|> pNot <|> pStrToInt <|> pIntToStr
        return r
    where
        pNegate = single '-' >> space >> Negate <$> pExpression
        pNot = single '!' >> space >> Not <$> pExpression
        pStrToInt = single '#' >> space >> StrToInt <$> pExpression
        pIntToStr = single '$' >> space >> IntToStr <$> pExpression

pBinary :: Parser AST
pBinary = do
    single 'B'
    sign <- oneOf ("+-*/%<>=|&.TD$" :: String)
    space
    let op = case sign of
                '+' -> Add
                '-' -> Sub
                '*' -> Mult
                '/' -> Div
                '%' -> Mod
                '<' -> Lt
                '>' -> Gt
                '=' -> Equals
                '|' -> Or
                '&'-> And
                '.' -> Concat
                'T' -> Take
                'D' -> Drop
                '$' -> Apply
    x1 <- pExpression
    space
    x2 <- pExpression
    space
    return $ op x1 x2

pIf :: Parser AST
pIf = do
    single '?'
    space
    cond <- pExpression
    space
    true <- pExpression
    space
    false <- pExpression
    space
    return $ If cond true false

pVar :: Parser AST
pVar = do
    single 'v'
    n <- pNumberLit
    return $ Var n

pLambda :: Parser AST
pLambda = do
    single 'L'
    n <- pNumberLit
    space
    body <- pExpression
    space
    return $ Lambda n body

parseExpression :: T.Text -> Either String AST
parseExpression txt =
    case parse pExpression "<expression>" txt of
        Left err -> Left $ errorBundlePretty err
        Right expr -> Right expr

