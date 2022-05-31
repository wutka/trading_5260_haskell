module Resources where

import qualified Data.Map as Map
import Data.List

data Transform = Transform { country :: String,
                             inputs :: [ResourceAmount],
                             outputs :: [ResourceAmount] }
               deriving (Eq)

data Transfer = Transfer { fromCountry :: String,
                           toCountry :: String,
                           resources :: [ResourceAmount] }
  deriving (Eq)
                 
data ResourceAmount = ResourceAmount { resource :: String,
                                       amount :: Int }
                    deriving (Eq)

data Operation =
    OpTransfer Transfer
  | OpTransform Transform
  deriving (Eq, Show)

type ResourceMap = Map.Map String Int
type CountryResources = Map.Map String ResourceMap

data CsvItem =
  CsvString String
  | CsvInt Int
  | CsvDouble Double
  deriving (Eq, Show)

data ScoreParameter =
  RatioScore String Double Double String
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

getCsvDouble :: CsvItem -> Double
getCsvDouble (CsvDouble d) = d
getCsvDouble (CsvInt i) = fromIntegral i
getCsvDouble x = error ("Attempted to get Csv double from "++show x)

isTransform :: Operation -> Bool
isTransform (OpTransform _) = True
isTransform _ = False

isTransfer :: Operation -> Bool
isTransfer (OpTransfer _) = True
isTransfer _ = False

getTransform :: Operation -> Maybe Transform
getTransform (OpTransform t) = Just t
getTransform _ = Nothing

getTransfer :: Operation -> Maybe Transfer
getTransfer (OpTransfer t) = Just t
getTransfer _ = Nothing

multiplyResource :: Int -> ResourceAmount -> ResourceAmount
multiplyResource n (ResourceAmount resource amount) = ResourceAmount resource (amount * n)

createTransformOp :: String -> Int -> Transform -> Operation
createTransformOp country multiplier (Transform _ inputs outputs) =
  OpTransform $ Transform country (map (multiplyResource multiplier) inputs)
                          (map (multiplyResource multiplier) outputs)

createTransferOp :: String -> String -> Int -> Transfer -> Operation
createTransferOp fromCountry toCountry multiplier (Transfer _ _ resources) =
  OpTransfer $ Transfer fromCountry toCountry (map (multiplyResource multiplier) resources)

greatestResMultiplier :: ResourceMap -> Int -> ResourceAmount -> Int
greatestResMultiplier rm currMax (ResourceAmount name amt) =
  min currMax bestMultiplier
  where
    resourceValue = rm Map.! name
    bestMultiplier = resourceValue `div` amt
  
greatestMultiplier :: ResourceMap -> Operation -> Int
greatestMultiplier rm (OpTransform (Transform _ inputs _)) =
  foldl' (greatestResMultiplier rm) maxBound inputs
greatestMultiplier rm (OpTransfer (Transfer _ _ resources)) =
  foldl' (greatestResMultiplier rm) maxBound resources

computeScore :: ResourceMap -> [ScoreParameter] -> Double
computeScore rm =
  foldl' (applyScore rm) 0.0

applyScore :: ResourceMap -> Double -> ScoreParameter -> Double
applyScore rm currScore (RatioScore field weight constant proportionField) =
  currScore + (fromIntegral $ rm Map.! field) * weight * constant /
              (fromIntegral $ rm Map.! proportionField)
  
