
hanoi :: Integer -> a -> a -> a -> [(a, a)]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-- Función recursiva que te indica los movimientos que deves hacer en las torres de hanoi 
-- El primer valor es la cantidad de disco a usar 
-- El segundo es la pila de inicio 
-- El tercero es la pila final o meta
-- El cuarto es la pila auxiliar
-- Ejemplo:
-- *Main> hanoi 3 '1' '3' '2'
-- [('1','3'),('1','2'),('3','2'),('1','3'),('2','1'),('2','3'),('1','3')]