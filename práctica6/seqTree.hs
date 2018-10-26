-- #4.

data Seq e = Nil | Unit e | Cat (Seq e) (Seq e) deriving Show

seqExample = Cat (Cat Nil (Unit 1)) (Cat (Unit 2) Nil)

--appSeq: toma dos secuencias y devuelve su concatenación
--appSeq :: Seq e -> Seq e -> Seq e
appSeq Nil         = []
appSeq (Unit x)    = [x]
appSeq (Cat s1 s2) = appSeq s1 ++ appSeq s2

--lenSeq: calcula la cantidad de elementos de una secuencia
--lenSeq :: Seq e -> Int
lenSeq Nil          = 0
lenSeq (Unit x)     = 1
lenSeq (Cat s1 s2)  = lenSeq s1 + lenSeq s2

--revSeq: toma una secuencia e invierte sus elementos
--revSeq :: Seq e -> Seq e
revSeq Nil          = Nil
revSeq (Unit x)     = Unit x
revSeq (Cat s1 s2)  = Cat (revSeq s2) (revSeq s1)

--headSeq: toma una secuencia y devuelve su primer elemento (el de más a la izq)
--headSeq :: Seq e -> e
--headSeq Nil          = 
--headSeq (Unit x)     = x
--headSeq (Cat Nil s2) = headSeq s2
--headSeq (Cat s1 Nil) = headSeq s1
--headSeq (Cat s1 s2)  = 


--conSeq: toma un elemento y una secuencia y devuelve la secuencia que tiene al elemento dado como cabeza y a la secuencia dada como cola.

--normSeq: elimina todos los Nil innecesarios de una secuencia.
--normSeq :: Seq e -> Seq e
normSeq Nil = Nil
normSeq (Unit x) = Unit x
normSeq (Cat Nil Nil) = Nil
normSeq (Cat s1 Nil)  = normSeq s1
normSeq (Cat Nil s2)  = normSeq s2
normseq (Cat s1 s2)   = Cat (normSeq s1) (normSeq s2)

normSeqExample = Cat (Cat Nil (Unit 1)) Nil

--eqSeq: toma dos secuencias y devuelve True sii ambas contienen los mismos valores, en el mismo orden y en la misma cantidad
--eqSeq :: Seq e -> Seq e -> Bool


--seq2List: toma una secuencia y devuelve una lisa con los mismos elementos en el mismo orden
--seqList :: Seq e -> [e]
seq2List Nil          = []
seq2List (Unit x)     = [x]
seq2List (Cat s1 s2)  = seq2List s1 ++ seq2List s2

seq2ListExample = Cat (Cat (Cat (Unit 1) (Unit 2)) (Nil)) (Cat (Nil) (Unit 3))
