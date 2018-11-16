

f =  \x->x
--
--r = \x xs -> f.concat x map f.concat xs

a = map f.concat

b = (f.concat) [[1,2,3]]

c = map (f.concat)--  [[1,2,3], [4,5]]

d x = map (map f) . map (x:)

e = concat . concat

g = concat . concat
