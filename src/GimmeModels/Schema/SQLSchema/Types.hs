{-# LANGUAGE OverloadedStrings #-}

module GimmeModels.Schema.SQLSchema.Types
where

import Control.Applicative
import Data.Char
import Data.ByteString

import qualified Data.ByteString.Char8 as C 
import qualified Data.Attoparsec.Char8 as P
import qualified GimmeModels.Types     as BT

data Table = Table {
      tableName   :: String 
    , tableFields :: [Field]
    } deriving (Show)

tableParser :: P.Parser Table
tableParser = do
    P.string "create table "
    n <- P.takeWhile (not . isSpace)
    P.skipWhile (isSpace)
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
    P.skipWhile (isSpace)
    t <- P.takeWhile (not . fieldGarbage) 
    P.skipWhile (not . fieldEnd)
    return $ Field (C.unpack n) (C.unpack t)

