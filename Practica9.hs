--Práctica 9.  Temas: PAtrones genéricos de recursión. Funciones sobre árboles.

--Ejercicio 1
data TipTree a = Tip a | Join (TipTree a) (TipTree a) deriving Show

foldTip :: (a->b)->(b -> b -> b) -> TipTree a -> b

foldTip f g (Tip x) = f x
foldTip f g (Join t1 t2) = g (foldTip f g t1) (foldTip f g t2)

leaves = foldTip (const 1) (+)

height = foldTip (const 0) (\h1 h2 -> 1 + max h1 h2)

nodes = foldTip (const 0) (\n1 n2 -> 1 + n1 + n2)

--mirrorTip :: TipTree a -> TipTree a
--mirrorTip = foldTip id (\t1 t2 -> Join t2 t1)

walkover = foldTip (\h -> [h]) (\xs1 xs2 -> xs1 ++ xs2)

mapTip f = foldTip (\x -> Tip (f x)) (\t1 t2 -> Join t1 t2)

tipTreeExample1 = Join (Tip 1) (Join (Tip 2) (Tip 3))
pot = \x->x*x


--Ejercicio 2
data BinTree a = Empty | Bin a (BinTree a) (BinTree a) deriving Show

foldBin :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldBin f z Empty = z
foldBin f z (Bin x t1 t2) = f x (foldBin f z t1) (foldBin f z t2)

nodesBin = foldBin (\x n1 n2 -> 1 + n1 + n2) 0

heightBin = foldBin (\x h1 h2 -> 1 + max h1 h2) 0

--mapBin f = foldBin (\x b1 b2 -> Bin (f x) b1 b2) id

--mirrorBin = foldBin (\x b1 b2 -> Bin x b2 b1) id

binTreeExample = Bin 1 Empty (Bin 2 Empty (Bin 3 Empty Empty))

