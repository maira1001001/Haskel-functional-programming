lengthList :: [a] -> Int
lengthList 		= foldr (\_ x -> x+1) 0
sumList			= foldr (+) 0

--avgLength :: [String] -> Float
strLength :: [String] -> Int
strLength = foldr ((+).length) 0
avgLength xs = strLength xs `div` lengthList xs

avgLength2 = ((div) (strLength)  (lengthList))

--adjacents = map (\x->x, \y->y)
--adjacents2 = map (f x y) where f x y = (x,y)


