module Chapter2.SimpleFunctions where


firstOrEmpty     :: [String] -> String
firstOrEmpty lst =
    if not (null lst)
        then head lst
        else "empty"

(+++)         :: [a] -> [a] -> [a]
lst1 +++ lst2 =
    if null lst1
        then lst2
        else head lst1 : (tail lst1 +++ lst2)


reverse' :: [a] -> [a]
reverse' lst =
    if null lst
        then []
        else reverse' (tail lst) +++ [head lst]

maxmin      :: Ord a => [a] -> (a, a)
maxmin list =
    if null (tail list)
        then (head list, head list)
        else ( if head list > fst (maxmin (tail list))
                   then head list
                   else fst (maxmin (tail list))
             , if head list < snd (maxmin (tail list))
                   then head list
                   else snd (maxmin (tail list)))

maxmin'      :: Ord a => [a] -> (a, a)
maxmin' list =
    let h = head list
    in if null (tail list)
           then (h, h)
           else ( if h > t_max
                      then h
                      else t_max
                , if h < t_min
                      then h
                      else t_min)
  where
    t     = maxmin' (tail list)
    t_max = fst t
    t_min = snd t
