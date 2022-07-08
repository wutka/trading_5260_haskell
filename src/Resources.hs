module Resources where

import qualified Data.Map as Map
import Data.List ( foldl', intersect, sortBy )
import Debug.Trace

-- Represents a transformation of resources within a country
data Transform = Transform { country :: String,
                             inputs :: [ResourceAmount],
                             outputs :: [ResourceAmount] }
               deriving (Eq)

-- Represents a transfer of resources from one country to another
data Transfer = Transfer { fromCountry :: String,
                           toCountry :: String,
                           resources :: [ResourceAmount] }
  deriving (Eq)

-- Represents a named resource with a quantity
data ResourceAmount = ResourceAmount { resource :: String,
                                       amount :: Int }
                    deriving (Eq)

-- An operation can either be a transfer or a transform
data Operation =
    OpTransfer Transfer
  | OpTransform Transform
  deriving (Eq, Show)

-- An item in a schedule has an operation and an expected utility
data ScheduleItem =
  ScheduleItem Operation Double

-- A resource map contains quantities for resources  
type ResourceMap = Map.Map String Int

-- Country resources maps countries to resource maps
type CountryResources = Map.Map String ResourceMap

-- An item in a CSV file
data CsvItem =
  CsvString String
  | CsvInt Int
  | CsvDouble Double
  deriving (Eq, Show)

-- Defines the various ways resources can be scored
data ScoreParameter =
    RatioScore String Double String
  | TargetedRatioScore String Double Double Double String
  deriving (Eq, Show)

-- The next step in planning (this is what is stored in the frontier)
data PlanItem = PlanItem Int Double [ScheduleItem] CountryResources
  deriving (Show)

instance Show ResourceAmount where
  show (ResourceAmount res amt) = "(" ++ res ++ " " ++ show amt ++ ")"

-- Represent a transform the way the project specification asks for
instance Show Transform where
  show (Transform country inputs outputs) =
    "(TRANSFORM "++country++" (INPUTS "++unwords (map show inputs)++") (OUTPUTS "++
    unwords (map show outputs)++"))"

-- Represent a transfer the way the project specification asks for
instance Show Transfer where
  show (Transfer from to res) = "(TRANSFER "++from++" "++to++" ("++unwords (map show res)++"))"

-- Represent a schedule item the way the project specification asks for
instance Show ScheduleItem where
  show (ScheduleItem op eu) =
    show op ++ " EU: " ++ show eu

-- Returns the string contained in a CsvString    
getCsvString :: CsvItem -> String
getCsvString (CsvString s) = s
getCsvString x = error ("Attempted to get Csv string from "++show x)

-- Returns the int contained in a CsvInt
getCsvInt :: CsvItem -> Int
getCsvInt (CsvInt i) = i
getCsvInt x = error ("Attempted to get Csv int from "++show x)

-- Returns the double contained in a CsvDouble
getCsvDouble :: CsvItem -> Double
getCsvDouble (CsvDouble d) = d
getCsvDouble (CsvInt i) = fromIntegral i
getCsvDouble x = error ("Attempted to get Csv double from "++show x)

-- Returns true it an operation is a transform
isTransform :: Operation -> Bool
isTransform (OpTransform _) = True
isTransform _ = False

-- Returns true if an operation is a transfer
isTransfer :: Operation -> Bool
isTransfer (OpTransfer _) = True
isTransfer _ = False

-- Returns the transform from an operation
getTransform :: Operation -> Maybe Transform
getTransform (OpTransform t) = Just t
getTransform _ = Nothing

-- Returns the transfer from an operation
getTransfer :: Operation -> Maybe Transfer
getTransfer (OpTransfer t) = Just t
getTransfer _ = Nothing

-- Multiplies a resource by a multipler
multiplyResource :: Int -> ResourceAmount -> ResourceAmount
multiplyResource n (ResourceAmount resource amount) = ResourceAmount resource (amount * n)

-- Creates a transform for a country with a specific multiplier
createTransformOp :: String -> Int -> Transform -> Operation
createTransformOp country multiplier (Transform _ inputs outputs) =
  OpTransform $ Transform country (map (multiplyResource multiplier) inputs)
                          (map (multiplyResource multiplier) outputs)

-- Creates a transfer from a country to a country with a specific multiplier
createTransferOp :: String -> String -> Int -> Transfer -> Operation
createTransferOp fromCountry toCountry multiplier (Transfer _ _ resources) =
  OpTransfer $ Transfer fromCountry toCountry (map (multiplyResource multiplier) resources)

-- Make sure that a resource adjustment doesn't go negative
checkResourceAdjustment :: String -> String -> Int -> Int -> Int
checkResourceAdjustment country res adj val =
  if adj+val >= 0 then adj+val
  else
    error $ "Invalid adjustment to resource "++res++" in "++country++" "++show adj++"+"++show val

-- Applies a transform operation
applyOp :: CountryResources -> Operation -> Int -> CountryResources
applyOp cr (OpTransform (Transform country inputs outputs)) mult =
  -- Update the country map with the transformed country's resources
  Map.insert country afterOutputs cr
  where
    -- Retrieve the country's resources
    rm = cr Map.! country
    -- Subtract the input resources from the country
    afterInputs = foldl' applyInputs rm inputs
    -- Add the output resources to the country
    afterOutputs = foldl' applyOutputs afterInputs outputs
    -- Subtracts some amount of a resource
    applyInputs rm (ResourceAmount res amt) =
      Map.adjust (checkResourceAdjustment country res (-amt*mult)) res rm
    -- adds some amount of a resource
    applyOutputs rm (ResourceAmount res amt) =
      Map.adjust ((amt*mult) +) res rm
-- Applies a transfer operation      
applyOp cr (OpTransfer (Transfer fromCountry toCountry amounts)) mult =
  -- update the from and to countries in the country resources map
  Map.insert fromCountry transferredFrom $
  Map.insert toCountry transferredTo cr
  where
    -- Retrieve the from country's resources
    fromRm = cr Map.! fromCountry
    -- Retrieve the to country's resources
    toRm = cr Map.! toCountry
    -- Remove the resources from the from country
    transferredFrom = foldl' transferFrom fromRm amounts
    -- Add the resrouces to the to country
    transferredTo = foldl' transferTo toRm amounts
    -- Removes a resource
    transferFrom rm (ResourceAmount res amt) =
      Map.adjust (checkResourceAdjustment fromCountry res (-amt*mult)) res rm
    -- Adds a resource
    transferTo rm (ResourceAmount res amt) =
      Map.adjust ((-amt*mult) +) res rm  

-- Multiplies the amounts in an operation by a multiplier
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

-- Find the greatest allowable multipler for a resource
greatestResMultiplier :: ResourceMap -> Int -> ResourceAmount -> Int
greatestResMultiplier rm currMax (ResourceAmount name amt) =
  -- Whichever multipler is the smallest, we use it because it
  -- is the limiting factor
  min currMax bestMultiplier
  where
    resourceValue = rm Map.! name
    -- The most we can multiply an op amount by is
    -- the resource amount divided by the op amount
    bestMultiplier = resourceValue `div` amt

-- Finds the greatest allowable multiplier for an operation
greatestMultiplier :: CountryResources -> Operation -> Int
greatestMultiplier cr (OpTransform (Transform source inputs _)) =
  -- Find the minimum multiplier across all the resources in the op
  foldl' (greatestResMultiplier rm) maxBound inputs
  where
    rm = cr Map.! source
    
greatestMultiplier cr (OpTransfer (Transfer from _ resources)) =
  -- Find the minimum multiplier across all the resources in the op
  foldl' (greatestResMultiplier rm) maxBound resources
  where
    rm = cr Map.! from

-- Computes a score for a resource map
computeScore :: ResourceMap -> [ScoreParameter] -> Double
-- Since the list of scoring parameters is the last parameter for
-- both this function and the foldl' function, we can omit
-- it from the parameter declaration here and the call to foldl'
computeScore rm =
  -- Add up the score by applying each scoring parameter
  foldl' (applyScore rm) 0.0

-- Adds the value from a scoring parameter to the current score
applyScore :: ResourceMap -> Double -> ScoreParameter -> Double

-- Applies a score for a ratio
applyScore rm currScore (RatioScore field weight proportionField) =
--  trace ("RatioScore for "++field++": "++show score++"  weight: "++show weight)
  currScore + score
  where
    score = (fromIntegral $ rm Map.! field) * weight /
            (fromIntegral $ rm Map.! proportionField)
-- Applies a score for a targeted ratio
applyScore rm currScore sc@(TargetedRatioScore field baseValue weight target proportionField) =
--  trace ("TargetedRatioScore for "++field++": "++show score)
  currScore + score
  where
    -- Dist is how far the current is from the idea, subtract weight*dist from the base score
    -- value. If the dist is 0, the score is the base score value.
    score = baseValue - (weight * fromIntegral dist)
    dist = abs (getTargetedDiff rm sc)
    level = 1.0 / (1.0 + log (1.0 + fromIntegral dist))

-- Returns the distance between the current resource value and the targeted resource
-- value, which is computed as a ratio of some particular field (e.g. population)
getTargetedDiff :: ResourceMap -> ScoreParameter -> Int
getTargetedDiff rm (RatioScore _ _ _) = error "can't getTargetedDiff on RatioScore"
getTargetedDiff rm (TargetedRatioScore field baseValue weight target proportionField) =
  actualVal - targetValue
  where
    denom = fromIntegral $ rm Map.! proportionField
    targetValue = floor (target * denom)
    actualVal = fromIntegral $ rm Map.! field

-- Score all the countries
allScores :: CountryResources -> [ScoreParameter] -> Map.Map String Double
allScores cr scoreParams =
  Map.map getScore cr
  where
    getScore rm = computeScore rm scoreParams

-- Returns a pair of an operation and the score the operation for the self country
-- by applying the operation to the resource map
makeScorePair :: CountryResources -> String -> Operation -> [ScoreParameter] -> Int -> (Operation, Double)
makeScorePair cr self op scoreParams multiplier =
  (multipliedOp, score)
  where
    multipliedOp = multiplyOp op multiplier
    appliedCR = applyOp cr op multiplier
    score = computeScore (appliedCR Map.! self) scoreParams

-- Return a list of the best quantities for an operation, sorted from best score
-- to worst
bestOperationQuantities :: CountryResources -> String -> [ScoreParameter] -> Operation -> [ScheduleItem]
bestOperationQuantities cr self scoring op =
  map makeScheduleItem $ sortBy compareScores quantities
  where                     
    compareScores (_,s1) (_,s2) = compare s1 s2
    quantities = map (makeScorePair cr self op scoring) [1..(greatestMultiplier cr op)]
    makeScheduleItem (op, score) = ScheduleItem op score

-- Gets a list of resources that are not at the idea value
-- returning either a list of the ones that are below the target (if getShortFall=True)
-- or above the target
getTargetedResourceDifference :: ResourceMap -> [ScoreParameter] -> Bool -> [(String,Int)]
getTargetedResourceDifference rm scoring getShortfall =
  -- get the absolute value of the differences, filtering by whether we wanted
  -- the negative or positive ones
  map adjust $ filter lessMore $ map makePair $ filter isTargetRatio scoring
  where
    isTargetRatio (RatioScore _ _ _) = False
    isTargetRatio (TargetedRatioScore _ _ _ _ _) = True
    makePair (RatioScore _ _ _) = error "can't makePair on RatioScore"
    makePair sc@(TargetedRatioScore field _ _ _ _) = (field, getTargetedDiff rm sc)
    lessMore (f,amt) = (getShortfall && (amt < 0)) || (not getShortfall && (amt > 0))
    adjust (f,amt) = (f,abs amt)

-- Returns the list of resources that are above their targeted values
getExcessResources :: CountryResources -> String -> [ScoreParameter] -> [(String,Int)]
getExcessResources cr country scoring =
  getTargetedResourceDifference rm scoring False
  where
    rm = cr Map.! country

-- Returns the list of resources that are below their target values
getShortfallResources :: CountryResources -> String -> [ScoreParameter] -> [(String,Int)]
getShortfallResources cr country scoring =
  getTargetedResourceDifference rm scoring True
  where
    rm = cr Map.! country

-- Returns the transfers that should be performed between two countries
getTransfers :: CountryResources -> String -> String -> String -> [ScoreParameter] -> Maybe ScheduleItem
getTransfers cr self fromCountry toCountry scoring =
  if null transfers then
    -- If there are no transfers, return Nothing
    Nothing
  else
    -- Otherwise create a schedule item from the transfers that need to happen
    Just $ ScheduleItem (makeTransferItem transfers) score
  where
    -- Get the resources that are above the targeted ratio in the from country
    excessResources = getExcessResources cr fromCountry scoring
    -- Get the resources that are below the targeted ratio in the to country
    shortfallResources = getShortfallResources cr toCountry scoring
    -- Merge the excess and shortfall resources, using the minimum value
    -- for each one
    allResourceMap = Map.unionWith min (Map.fromList excessResources) (Map.fromList shortfallResources)
    -- Gets a list of the resources that are common between the excess and the shortfall
    transferResources = map fst excessResources `intersect` map fst shortfallResources
    -- Gets a list of the resources to transfer
    transfers = map makeResourceAmount transferResources
    -- Create a pair of a resource name and its value
    makeResourceAmount k = ResourceAmount k (allResourceMap Map.! k)
    -- Create the transfer operation
    makeTransferItem transfers =
      OpTransfer (Transfer fromCountry toCountry transfers)
    rm = cr Map.! self
    -- Compute the score for the self country for this transfer
    score = computeScore rm scoring
    
    
    
