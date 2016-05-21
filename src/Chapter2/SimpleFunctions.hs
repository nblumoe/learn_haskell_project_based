module Chapter2.SimpleFunctions where


firstOrEmpty :: [String] -> String
firstOrEmpty lst =
    if not (null lst)
        then head lst
        else "empty"

(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 =
    if null lst1
        then lst2
        else head lst1 : (tail lst1 +++ lst2)


reverse' :: [a] -> [a]
reverse' lst =
    if null lst
        then []
        else reverse' (tail lst) +++ [head lst]
