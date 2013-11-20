module GimmeModels.Schema.SQLSchema.Types
where

import Data.Char             (toUpper, toLower)
import Data.ByteString.Char8 (split, pack, unpack)

import GimmeModels.Schema.SQLSchema.Parser
import qualified GimmeModels.Types as BT

instance BT.FromSchema Schema where
    fromSchema s n sc = models
        where 
            models = map (tableToModel n sc) (schemaTables s) 
 
tableToModel :: Maybe String -> Maybe BT.Type -> Table -> BT.Model
tableToModel modelName superClass t = BT.Model {
      BT.modelName   = camelize Class (tableName t)
    , BT.modelParent = superClass
    , BT.modelProps  = map fieldToProp (tableFields t)
    }
    where
        fieldToProp f = BT.Property { 
              BT.propName = camelize Prop (fieldName f)
            , BT.propType = BT.Type $ map toLower (fieldType f) 
            }   

data CamelizeMode = Class | Prop

camelize :: CamelizeMode -> String -> String
camelize mode s = foldl (capitalizeFirst mode) "" $ splitS str
    where 
        str                          = [toLower c | c <- s]
        splitS s'                    = [unpack  s | s <- (split '_' $ pack s')]
        capitalizeFirst Prop "" s    = s
        capitalizeFirst _ acc (c:cs) = acc ++ (toUpper c : cs)

parseSchemaFromString = parseSchema 
