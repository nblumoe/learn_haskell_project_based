module DataModel where

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
