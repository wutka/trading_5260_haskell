module Resources where

import qualified Data.Map as Map

data Transform = Transform { country :: String,
                             inputs :: [ResourceAmount],
                             outputs :: [ResourceAmount] }
               deriving (Eq)
                 
data ResourceAmount = ResourceAmount { resource :: String,
                                       amount :: Int }
                    deriving (Eq)

data Transfer = Transfer { fromCountry :: String,
                           toCountry :: String,
                           resources :: [ResourceAmount] }
              deriving (Eq)

data SpecItem = SpecTransform Transform | SpecTransfer Transfer
  deriving (Eq, Show)

newtype Spec = Spec [SpecItem]
  deriving (Eq, Show)

type ResourceMap = Map.Map String Int
type CountryResources = Map.Map String ResourceMap

data CsvItem =
  CsvString String
  | CsvInt Int
  deriving (Eq, Show)

instance Show ResourceAmount where
  show (ResourceAmount res amt) = "(" ++ res ++ " " ++ show amt ++ ")"
  
instance Show Transform where
  show (Transform country inputs outputs) =
    "(TRANSFORM "++country++" (INPUTS "++unwords (map show inputs)++") (OUTPUTS "++
    unwords (map show outputs)++"))"

instance Show Transfer where
  show (Transfer from to res) = "(TRANSFER "++from++" "++to++" ("++unwords (map show res)++"))"

getCsvString :: CsvItem -> String
getCsvString (CsvString s) = s
getCsvString x = error ("Attempted to get Csv string from "++show x)

getCsvInt :: CsvItem -> Int
getCsvInt (CsvInt i) = i
getCsvInt x = error ("Attempted to get Csv int from "++show x)

