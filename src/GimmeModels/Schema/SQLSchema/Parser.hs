{-# LANGUAGE OverloadedStrings #-}

module GimmeModels.Schema.SQLSchema.Parser (Schema(..), Table(..), Field(..), parseSchema)
where

import Data.Char (isSpace, toLower)
import Control.Applicative

import qualified Data.ByteString.Char8 as C 
import qualified Data.Attoparsec.Char8 as P

data Schema = Schema { schemaTables :: [Table] } deriving (Show)

schemaParser :: P.Parser Schema
schemaParser = do
    ts <- P.many1 tableParser
    return $ Schema ts

data Table = Table {
      tableName   :: String 
    , tableFields :: [Field]
    } deriving (Show)

tableParser :: P.Parser Table
tableParser = do
    P.manyTill P.anyChar $ P.stringCI "create table"
    P.skipWhile fieldGarbage
    n <- P.takeWhile (not . fieldGarbage)
    P.skipWhile fieldGarbage 
    P.char '(' 
    fs <- fieldParser `P.sepBy` (P.char ',')
    P.manyTill P.anyChar (P.char ';')
    return $ Table (C.unpack n) fs 

data Field = Field {
      fieldName :: String
    , fieldType :: String
    } deriving (Show)

fieldGarbage = \c -> isSpace c || c == ',' || c == '\"'
fieldEnd = \c -> c == ',' || c == ')' 

fieldParser :: P.Parser Field
fieldParser = do
    P.skipWhile fieldGarbage
    n <- P.takeWhile (not . fieldGarbage)
    P.skipWhile fieldGarbage
    t <- P.takeWhile (not . fieldGarbage) 
    P.takeTill (\c -> c == ',' || c == ';')
    return $ Field (C.unpack n) (C.unpack t)

parseSchema :: String -> Maybe Schema
parseSchema str = 
    case P.parseOnly schemaParser (C.pack str) of 
         Right s -> Just $ cleanSchema s
         Left  e -> Nothing
    where 
        cleanSchema s = Schema $ cleanTables s
        cleanTables s = map cleanTable (schemaTables s)
        cleanTable t  = t { tableFields = filter (\f -> (lower (fieldType f)) /= lower "key") (tableFields t) } 
        lower s       = map toLower s

