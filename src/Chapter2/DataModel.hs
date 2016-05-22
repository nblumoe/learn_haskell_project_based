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
    deriving (Show)

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
clientName client =
    case client of
        GovOrg name -> name
        Company name _ _ _ -> name
        Individual (Person fName lName _) _ -> fName ++ " " ++ lName

companyName :: Client -> Maybe String
companyName client =
    case client of
        Company name _ _ _ -> Just name
        _ -> Nothing
