{-# LANGUAGE OverloadedStrings #-}

module GimmeModels.Schema.JSONSchema.Types
where

import Data.Aeson
import Data.Maybe
import Data.Text           (Text, unpack, pack)
import Data.Map            (Map(..), mapWithKey, toList)
import Control.Monad       (mzero)
import Control.Applicative ((<$>), (<*>), empty)

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Aeson.Types           as T
import qualified GimmeModels.Types          as BT

data Schema = Schema {
      schema      :: Maybe Text
    , title       :: Maybe Text
    , id          :: Maybe Text 
    , _type       :: Maybe Type
    , description :: Maybe Text
    , extends     :: Maybe (Map String String)
    , items       :: Maybe Schema 
    , properties  :: Maybe PropertiesMap
    , required    :: Maybe Required 
    } deriving (Show)

type PropertiesMap = (Map String Schema)

data Required = ReqB Bool | ReqSeq [Text] deriving (Show)

instance FromJSON Required where
    parseJSON v = 
        case v of 
            (Bool  _) -> ReqB   <$> parseJSON v
            (Array _) -> ReqSeq <$> parseJSON v
            otherwise -> empty

data Type = Single Text | Multiple [Text] deriving (Show)

instance FromJSON Type where 
    parseJSON v = 
        case v of 
            (String _) -> Single   <$> parseJSON v
            (Array  _) -> Multiple <$> parseJSON v
            otherwise  -> empty 

instance FromJSON Schema where 
     parseJSON (Object v) = 
        Schema <$> v .:? "$schema"
               <*> v .:? "title"
               <*> v .:? "id"
               <*> v .:? "type"
               <*> v .:? "description"
               <*> v .:? "extends"
               <*> v .:? "items"
               <*> v .:? "properties"
               <*> v .:? "required"
     parseJSON _ = empty

instance BT.FromSchema Schema where
    fromSchema s n sc = [BT.Model { BT.modelName = mname, BT.modelProps = props, BT.modelParent = sc }]
        where 
           props = case properties s of 
                        Just ps -> fromSchemaProps ps 
                        Nothing -> [] 
           mname = case title s of 
                       Just name -> unpack name
                       Nothing   -> fromMaybe (error "Model's name not specified") n
 
fromSchemaProps :: PropertiesMap -> [BT.Property]
fromSchemaProps ps = fromSchemaProp <$> toList ps 

fromSchemaProp :: (String, Schema) -> BT.Property
fromSchemaProp (k, s) = BT.Property { BT.propName = k, BT.propType = t }
    where 
        t = BT.Type $ case _type s of 
                          Just t' -> case t' of 
                                      (Single   x)  -> unpack x
                                      (Multiple xs) -> unpack $ head xs -- TODO: here we take only first type from list, maybe this isn't right thing
                          Nothing -> "any" 
        
parseSchemaFromString :: String -> Maybe Schema
parseSchemaFromString s = decode $ BSL.pack s :: Maybe Schema 
