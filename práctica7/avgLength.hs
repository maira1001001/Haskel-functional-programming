-- avgLength: calcula la longitud promedio de las palabras de una lista
--

--avgLength :: [String] -> Float

avgLength []     = 0
avgLength (s:ss) = totalStr (s:ss) / length (ss+1) 

totalStr = foldr ((+).length) 0



