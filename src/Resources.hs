module Resources where

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

instance Show ResourceAmount where
  show (ResourceAmount res amt) = "(" ++ res ++ " " ++ show amt ++ ")"
  
instance Show Transform where
  show (Transform country inputs outputs) =
    "(TRANSFORM "++country++" (INPUTS "++unwords (map show inputs)++") (OUTPUTS "++
    unwords (map show outputs)++"))"

instance Show Transfer where
  show (Transfer from to res) = "(TRANSFER "++from++" "++to++" ("++unwords (map show res)++"))"
