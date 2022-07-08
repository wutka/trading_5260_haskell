module Resources where

import qualified Data.Map as Map
import Data.List
import Debug.Trace

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

data ScheduleItem =
  ScheduleItem Operation Double
  
type ResourceMap = Map.Map String Int
type CountryResources = Map.Map String ResourceMap

data CsvItem =
  CsvString String
  | CsvInt Int
  | CsvDouble Double
  deriving (Eq, Show)

data ScoreParameter =
  RatioScore String Double String
  | TargetedRatioScore String Double Double String
  deriving (Eq, Show)

data PlanItem = PlanItem Int Double [ScheduleItem] CountryResources
  deriving (Show)

instance Show ResourceAmount where
  show (ResourceAmount res amt) = "(" ++ res ++ " " ++ show amt ++ ")"
  
instance Show Transform where
  show (Transform country inputs outputs) =
    "(TRANSFORM "++country++" (INPUTS "++unwords (map show inputs)++") (OUTPUTS "++
    unwords (map show outputs)++"))"

instance Show Transfer where
  show (Transfer from to res) = "(TRANSFER "++from++" "++to++" ("++unwords (map show res)++"))"

instance Show ScheduleItem where
  show (ScheduleItem op eu) =
    show op ++ " EU: " ++ show eu
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


checkResourceAdjustment :: String -> String -> Int -> Int -> Int
checkResourceAdjustment country res adj val =
  if adj+val >= 0 then adj+val
  else
    error $ "Invalid adjustment to resource "++res++" in "++country++" "++show adj++"+"++show val

applyOp :: CountryResources -> Operation -> Int -> CountryResources
applyOp cr (OpTransform (Transform country inputs outputs)) mult =
  Map.insert country afterOutputs cr
  where
    rm = cr Map.! country
    afterInputs = foldl' applyInputs rm inputs
    afterOutputs = foldl' applyOutputs afterInputs outputs
    applyInputs rm (ResourceAmount res amt) =
      Map.adjust (checkResourceAdjustment country res (-amt*mult)) res rm
    applyOutputs rm (ResourceAmount res amt) =
      Map.adjust ((amt*mult) +) res rm
applyOp cr (OpTransfer (Transfer fromCountry toCountry amounts)) mult =
  Map.insert fromCountry transferredFrom $
  Map.insert toCountry transferredTo cr
  where
    fromRm = cr Map.! fromCountry
    toRm = cr Map.! toCountry
    transferredFrom = foldl' transferFrom fromRm amounts
    transferredTo = foldl' transferTo toRm amounts
    transferFrom rm (ResourceAmount res amt) =
      Map.adjust (checkResourceAdjustment fromCountry res (-amt*mult)) res rm
    transferTo rm (ResourceAmount res amt) =
      Map.adjust ((-amt*mult) +) res rm  

multiplyOp :: Operation -> Int -> Operation
multiplyOp (OpTransform (Transform country inputs outputs)) mult =
  OpTransform (Transform country multInputs multOutputs)
  where
    multInputs = map (multiplyResource mult) inputs
    multOutputs = map (multiplyResource mult) outputs
multiplyOp (OpTransfer (Transfer fromCountry toCountry amounts)) mult =
  OpTransfer (Transfer fromCountry toCountry multAmounts)
  where
    multAmounts = map (multiplyResource mult) amounts
  
greatestResMultiplier :: ResourceMap -> Int -> ResourceAmount -> Int
greatestResMultiplier rm currMax (ResourceAmount name amt) =
  min currMax bestMultiplier
  where
    resourceValue = rm Map.! name
    bestMultiplier = resourceValue `div` amt
  
greatestMultiplier :: CountryResources -> Operation -> Int
greatestMultiplier cr (OpTransform (Transform source inputs _)) =
  foldl' (greatestResMultiplier rm) maxBound inputs
  where
    rm = cr Map.! source
    
greatestMultiplier cr (OpTransfer (Transfer from _ resources)) =
  foldl' (greatestResMultiplier rm) maxBound resources
  where
    rm = cr Map.! from

computeScore :: ResourceMap -> [ScoreParameter] -> Double
computeScore rm =
  foldl' (applyScore rm) 0.0

applyScore :: ResourceMap -> Double -> ScoreParameter -> Double
applyScore rm currScore (RatioScore field weight proportionField) =
--  trace ("RatioScore for "++field++": "++show score++"  weight: "++show weight)
  currScore + score
  where
    score = (fromIntegral $ rm Map.! field) * weight /
            (fromIntegral $ rm Map.! proportionField)
applyScore rm currScore sc@(TargetedRatioScore field weight target proportionField) =
--  trace ("TargetedRatioScore for "++field++": "++show score)
  currScore + score
  where
    score = weight - fromIntegral dist
    dist = abs (getTargetedDiff rm sc)
    level = 1.0 / (1.0 + log (1.0 + fromIntegral dist))

getTargetedDiff :: ResourceMap -> ScoreParameter -> Int
getTargetedDiff rm (RatioScore _ _ _) = error "can't getTargetedDiff on RatioScore"
getTargetedDiff rm (TargetedRatioScore field weight target proportionField) =
  actualVal - targetValue
  where
    denom = fromIntegral $ rm Map.! proportionField
    targetValue = floor (target * denom)
    actualVal = fromIntegral $ rm Map.! field

allScores :: CountryResources -> [ScoreParameter] -> Map.Map String Double
allScores cr scoreParams =
  Map.map getScore cr
  where
    getScore rm = computeScore rm scoreParams
    
makeScorePair :: CountryResources -> String -> Operation -> [ScoreParameter] -> Int -> (Operation, Double)
makeScorePair cr self op scoreParams multiplier =
  (multipliedOp, score)
  where
    multipliedOp = multiplyOp op multiplier
    appliedCR = applyOp cr op multiplier
    score = computeScore (appliedCR Map.! self) scoreParams
              
bestOperationQuantities :: CountryResources -> String -> [ScoreParameter] -> Operation -> [ScheduleItem]
bestOperationQuantities cr self scoring op =
  map makeScheduleItem $ sortBy compareScores quantities
  where                     
    compareScores (_,s1) (_,s2) = compare s1 s2
    quantities = map (makeScorePair cr self op scoring) [1..(greatestMultiplier cr op)]
    makeScheduleItem (op, score) = ScheduleItem op score

getTargetedResourceDifference :: ResourceMap -> [ScoreParameter] -> Bool -> [(String,Int)]
getTargetedResourceDifference rm scoring getShortfall =
  map adjust $ filter lessMore $ map makePair $ filter isTargetRatio scoring
  where
    isTargetRatio (RatioScore _ _ _) = False
    isTargetRatio (TargetedRatioScore _ _ _ _) = True
    makePair (RatioScore _ _ _) = error "can't makePair on RatioScore"
    makePair sc@(TargetedRatioScore field _ _ _) = (field, getTargetedDiff rm sc)
    lessMore (f,amt) = (getShortfall && (amt < 0)) || (not getShortfall && (amt > 0))
    adjust (f,amt) = (f,abs amt)

getExcessResources :: CountryResources -> String -> [ScoreParameter] -> [(String,Int)]
getExcessResources cr country scoring =
  getTargetedResourceDifference rm scoring False
  where
    rm = cr Map.! country

getShortfallResources :: CountryResources -> String -> [ScoreParameter] -> [(String,Int)]
getShortfallResources cr country scoring =
  getTargetedResourceDifference rm scoring True
  where
    rm = cr Map.! country

getTransfers :: CountryResources -> String -> String -> String -> [ScoreParameter] -> Maybe ScheduleItem
getTransfers cr self fromCountry toCountry scoring =
  if null transfers then
    Nothing
  else
    Just $ ScheduleItem (makeTransferItem transfers) score
  where
    excessResources = getExcessResources cr fromCountry scoring
    shortfallResources = getShortfallResources cr toCountry scoring
    allResourceMap = Map.unionWith min (Map.fromList excessResources) (Map.fromList shortfallResources)    
    transferResources = map fst excessResources `intersect` map fst shortfallResources
    transfers = map (makeResourceAmount . kvpair) transferResources
    kvpair k = (k, allResourceMap Map.! k)    
    matchResource resources (res,amt) = res `elem` resources
    makeResourceAmount (res,amt) = ResourceAmount res amt
    makeTransferItem transfers =
      OpTransfer (Transfer fromCountry toCountry transfers)
    rm = cr Map.! self
    score = computeScore rm scoring
    
    
    
