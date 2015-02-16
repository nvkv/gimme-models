module GimmeModels.Types
(
      File(..)
    , Model(..)
    , Property(..)
    , Type(..)
    , TargetModel(..)
    , TargetProperty(..)
    , FromSchema(..)
    , NamingOptions(..)
)
where

data File = File {
        fileName         :: String
      , fileContent      :: String
      , fileOwerwritable :: Bool
      } deriving (Eq, Show)

data NamingOptions = NamingOptions {
       namePrefix  :: Maybe String
     , namePostfix :: Maybe String
     }

data Model = Model {
      modelName   :: String
    , modelParent :: Maybe Type
    , modelProps  :: [Property]
    } deriving (Eq, Show)

class TargetModel a where
    fromBase :: Model -> NamingOptions -> a
    generate :: a -> [File]

data Property = Property {
      propName :: String
    , propType :: Type
    } deriving (Eq, Show)

class TargetProperty a where
    fromBaseProp :: Property -> a

data Type = Type String deriving (Eq, Show)

class FromSchema a where
    -- | Some Model -> Maybe Model Name -> Maybe Superclass -> [Model]
    fromSchema :: a -> Maybe String -> Maybe Type -> [Model]
