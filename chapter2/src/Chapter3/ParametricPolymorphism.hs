-- |

module Chapter3.ParametricPolymorphism where

maybeString :: Maybe t -> [Char]
maybeString (Just _) = "Just"
maybeString Nothing  = "Nothing"


data Client i = GovOrg { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String,
                          person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
                deriving Show

data Person = Person { firstName :: String, lastName :: String }
              deriving Show


swapTriple :: (t2, t, t1) -> (t, t1, t2)
swapTriple (x,y,z) = (y,z,x)

-- t -> (t,t)
duplicate :: t -> (t, t)
duplicate x = (x,x)

-- t -> Nothing
nothing :: t -> Maybe a
nothing _ = Nothing

-- [t] -> [(t1,t)]
index :: Num t => [t1] -> [(t, t1)]
index [] = []
index [x] = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
               in (n+1,x):indexed

-- [] -> Char
maybeA :: [t] -> Char
maybeA [] = 'a'


apply3f2 :: (Num a, Num a1) => (a1 -> a) -> a1 -> a
apply3f2 f x = 3 * f (x + 2)

maplambda = map ( \x -> x + 2) [1,2,3]


multiplyByNLambda :: Num a => a -> a -> a
multiplyByNLambda n = \x -> x * n

multiplyByN :: Num a => a -> a -> a
multiplyByN n x = x * n

--- filters

---- Exercise 3-2

filterOnes :: [Integer] -> [Integer]
filterOnes = filter (\x -> x == 1)

filterANumber :: Eq a => a -> [a] -> [a]
filterANumber n = filter (\x -> x == n)

-- filterNot :: (a -> Bool) -> ([a] -> [a])
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (\x -> f x == False)

isGovOrg :: Client t -> Bool
isGovOrg (GovOrg _ _) = True
isGovOrg _ = False

filterGovOrgs :: [Client t] -> [Client t]
filterGovOrgs = filter isGovOrg

filterGovOrgsCase :: [Client t] -> [Client t]
filterGovOrgsCase = filter (\c -> case c of
                                    (GovOrg _ _) -> True
                                    _  -> False)
