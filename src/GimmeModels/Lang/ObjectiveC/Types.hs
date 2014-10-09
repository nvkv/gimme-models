module GimmeModels.Lang.ObjectiveC.Types (Model(..), Property(..))
where

import qualified GimmeModels.Types as BT
import Data.List
import Data.Maybe

-- | Objective-C Class Model
data Model = Model {
      baseModel  :: BT.Model
    , superclass :: Type
    , modelProps :: [Property]
    } deriving (Eq)

-- | Generate Model's header file content
ocHeader :: Model -> String
ocHeader m = "#import <Foundation/Foundation.h>\n\n" ++
             headers ++ "\n\n" ++
             "@interface " ++ BT.modelName (baseModel m) ++ " : " ++ show (superclass m) ++ "\n\n" ++
             intercalate "\n" (map show $ modelProps m) ++
             "\n\n@end\n"
    where
        headers = case show $ superclass m of
                    "NSObject" -> ""
                    sc         -> "#import \"" ++ sc ++ ".h\"\n"

-- | Generates Model's implementation file content
ocImplementation :: Model -> String
ocImplementation m = "#import \"" ++ mname ++ ".h\"\n\n" ++
                     "@implementation " ++ mname ++ "\n" ++
                     "@end\n"
                     where
                        mname = BT.modelName $ baseModel m

-- For debugging purposes we will show Model
instance Show Model where
    show m = ocHeader m ++ "\n ------\n" ++ ocImplementation m

--
-- Model must be instance of TargetModel typeclass
--
instance BT.TargetModel Model where
    fromBase m opts = Model {
          baseModel  = m { BT.modelName = "_" ++ prefix ++ BT.modelName m ++ postfix }
        , superclass = sc
        , modelProps = map BT.fromBaseProp $ BT.modelProps m
        }
        where sc = case BT.modelParent m of
                        Just (BT.Type t) -> Type t
                        Nothing          -> Type "NSObject"
              prefix  = fromMaybe "" (BT.namePrefix opts)
              postfix = fromMaybe "" (BT.namePostfix opts)

    -- | This function will generate "_" prefixed class for actual model, and normal-named file that subclasses actual model
    generate m = [hdr, imp] ++ generateFinal
            where
                mn  = BT.modelName $ baseModel m
                hdr = BT.File { BT.fileName = mn ++ ".h", BT.fileContent = ocHeader m         , BT.fileOwerwritable = bModel }
                imp = BT.File { BT.fileName = mn ++ ".m", BT.fileContent = ocImplementation m , BT.fileOwerwritable = bModel }

                bModel = "_" `isPrefixOf` mn
                generateFinal =
                     -- If this is not already final model
                    if bModel then BT.generate m' else []
                        where
                            m' = m { baseModel  = finalNamedM $ baseModel m
                                   , superclass = Type mn
                                   , modelProps = [] }
                            finalNamedM mdl = mdl { BT.modelName  = dropWhile (== '_') $ BT.modelName mdl
                                                  , BT.modelProps = [] }

-- | Objective-C property
data Property = Property {
      baseProp  :: BT.Property
    , propType  :: Type
    , propAttrs :: [PropAttribute]
    } deriving (Eq)

-- | 'show' method will be used to present Property as actual Objective-C code
instance Show Property where
    show p = "@property (" ++ attrs ++ ") " ++ ptype ++ " " ++ pname ++ ";"
        where
            attrs = intercalate ", " $ map show $ propAttrs p
            ptype = show $ propType p
            pname = BT.propName $ baseProp p

-- | Objective-C type
data Type = Type String deriving (Eq)

-- | Type will be Show instance to extract actual type string from it
instance Show Type where
    show (Type t) = t

-- | Enumeration of all possible Objective-C property attributes
data PropAttribute =  Strong
                    | Weak
                    | Copy
                    | Retain
                    | Assign
                    | Nonatomic
                    | Readwrite
                    | Readonly
                    deriving (Eq, Bounded)

instance Show PropAttribute where
    show p = case p of
        Strong    -> "strong"
        Weak      -> "weak"
        Copy      -> "copy"
        Retain    -> "retain"
        Assign    -> "assign"
        Nonatomic -> "nonatomic"
        Readwrite -> "readwrite"
        Readonly  -> "readonly"

--
-- Objective-C Property must be instance of TargetProperty
--
instance BT.TargetProperty Property where
    fromBaseProp p = Property {
          baseProp  = p
        , propType  = mapType ptype
        , propAttrs = typeAttrs ptype
        }
        where ptype = BT.propType p

-- | Mapping base types to Objective-C types
mapType :: BT.Type -> Type
mapType t =
    case t of
        BT.Type "string"    -> Type "NSString *"
        BT.Type "integer"   -> Type "NSNumber *"
        BT.Type "boolean"   -> Type "NSNumber *"
        BT.Type "number"    -> Type "NSNumber *"
        BT.Type "array"     -> Type "NSArray *"
        BT.Type "null"      -> Type "NSNull *"
        BT.Type "object"    -> Type "NSDictionary *"
        BT.Type "date-time" -> Type "NSDate *"
        otherwise           -> Type "NSString *"

-- | In future this function will guess Objective-C
-- | property attrubutes by base type. Or will not.
-- | Now it assume all properties should be (nonatomic, strong)
typeAttrs :: BT.Type -> [PropAttribute]
typeAttrs _ = [Strong, Nonatomic]
