
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


potencia :: Integer -> Integer -> Integer
potencia m 0 = 1
potencia m n = m * (potencia m (n-1))

--potencia 2 3 = 2 * (potencia 2 (3-1))
--potencia 2 2 = 2 * (potencia 2 (2-1))
--potencia 2 1 = 2 * (potencia 2 (1-1))
--Resultado final
--potencia 2 3 = 2 * (2 * (2 * (1)))

potencia2 m n = m*m*(n-1)

-- Serio de fibonaci
fibo:: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo x = (fibo(x-1))+(fibo(x-2))

-- Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

-- Funcion de Gera 
porr :: Int -> Int -> Int
porr m 0 = 0
porr m n = m + (porr m (n-1))

-- Martha 
desde::Int->[Int]
desde n = n: desde (n+1)

-- Cortez
--busSec :: Ord a => [a] -> a->Bool
--busSec []_ = False
--busSec (x:xs) ele 
-- | x==ele=True
-- |True =busSec ele

-- Soria 
replicate' ::(Num i, Ord i) => i -> a -> [a]
replicate' n x | n <=0= [] | otherwise = x: replicate' (n-1)x

-- Bofo
MAXIMUM' ::(Ord a)=>[a]->a
MAXIMUM' []=error "Maximum of emptx list"
MAXIMUM' [x]=x
MAXIMUM' (x;xs)= x `Max` (MAXIMUM' xs)