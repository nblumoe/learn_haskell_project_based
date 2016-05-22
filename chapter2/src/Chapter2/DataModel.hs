{-# LANGUAGE ViewPatterns #-}

module DataModel where

import Data.Char

data Client
    = GovOrg String
    | Company String
              Integer
              Person
              String
    | Individual Person
                 Bool
    deriving (Show)

data Person =
    Person String
           String
           Gender
    deriving (Show)

data Gender
    = Male
    | Female
    | Unknown
    deriving (Show, Eq)

data TimeMachine =
    TimeMachine String
                Integer
                Direction
                Float
    deriving (Show)

data Direction
    = Past
    | Future
    | Both
    deriving (Show)

clientName :: Client -> String
clientName (GovOrg name) = name
clientName (Company name _ _ _) = name
clientName (Individual (Person fName lName _) _) = fName ++ " " ++ lName

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "CEO") = True
specialClient _ = False

companyName :: Client -> Maybe String
companyName (Company name _ _ _) = Just name
companyName (_) = Nothing

data GenderFrequencies =
    GenderFrequencies (Int, Int, Int)
    deriving ((Show))

malesCount :: [Client] -> Int
malesCount [] = 0
malesCount ((Individual (Person _ _ Male) _):cs) = 1 + malesCount cs
malesCount (_:cs) = malesCount cs

genderCount :: Gender -> [Client] -> Int
genderCount _ [] = 0
genderCount g ((Individual (Person _ _ g') _):cs)
  | g == g' = 1 + genderCount g cs
  | otherwise = genderCount g cs
genderCount g (_:cs) = genderCount g cs

genderFrequencies :: [Client] -> GenderFrequencies
genderFrequencies clients =
    GenderFrequencies
        ( genderCount Female clients
        , genderCount Male clients
        , genderCount Unknown clients)

machinesOnSale :: Float -> [TimeMachine] -> [TimeMachine]
machinesOnSale _ [] = []
machinesOnSale discount (TimeMachine a b c price:tms) =
  ((TimeMachine a b c ((1-discount) * price)):(machinesOnSale discount tms))

-- Records

data ClientR
    = GovOrgR { clientRName :: String}
    | CompanyR { clientRName :: String
               , companyId :: Integer
               , person :: PersonR
               , duty :: String}
    | IndividualR { person :: PersonR}
    deriving (Show)

data PersonR = PersonR
    { firstName :: String
    , lastName :: String
    } deriving (Show)

greet :: ClientR -> String
greet IndividualR{person = PersonR{firstName = fn}} = "Hi, " ++ fn
greet CompanyR{clientRName = c} = "Hello, " ++ c
greet GovOrgR{} = "Welcome"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@PersonR{firstName = initial:rest} =
    let newName = toUpper initial : rest
    in p
       { firstName = newName
       }
nameInCapitals p@PersonR{firstName = ""} = p

data TimeMachineR = TimeMachineR
    { name :: String
    , model :: Int
    , direction :: Direction
    , price :: Float
    } deriving (Show)

machinesOnSaleR :: Float -> [TimeMachineR] -> [TimeMachineR]
machinesOnSaleR _ [] = []
machinesOnSaleR discount (tm@TimeMachineR{price = p}:tms) =
    let newPrice = (1 - discount) * p
    in tm
       { price = newPrice
       } :
       machinesOnSaleR discount tms
