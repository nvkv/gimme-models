{-# LANGUAGE OverloadedStrings #-}

module GimmeModels.Schema.SQLSchema.Types
where

import Data.Char (isSpace)
import Control.Applicative

import qualified Data.ByteString.Char8 as C 
import qualified Data.Attoparsec.Char8 as P
import qualified GimmeModels.Types     as BT

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
    P.skipSpace
    n <- P.takeWhile (not . isSpace)
    P.skipSpace
    P.char '(' 
    fs <- P.manyTill fieldParser (P.string ");")
    return $ Table (C.unpack n) fs 

data Field = Field {
      fieldName :: String
    , fieldType :: String
    } deriving (Show)

fieldGarbage = \c -> isSpace c || c == ','
fieldEnd = \c -> c == ',' || c == ')' 

fieldParser :: P.Parser Field
fieldParser = do
    P.skipWhile fieldGarbage 
    n <- P.takeWhile (not . fieldGarbage)
    P.skipSpace
    t <- P.takeWhile (not . fieldGarbage) 
    P.skipWhile (not . fieldEnd)
    return $ Field (C.unpack n) (C.unpack t)

parseSchema :: String -> Schema
parseSchema s = 
    case P.parseOnly schemaParser (C.pack s) of 
         Right s -> s
         Left  e -> error $ show e

