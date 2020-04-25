-- LP - HASKELL. Quatre en ratlla
-- DANIEL DONATE - 2020

import Data.List
import Data.Ord 
import System.IO
import System.Random

type Col = [Int]
type Board = [Col]

-- USAGE ------------------------------------------------------------------

usage = print "El nombre de files ha de ser >= 6 i el de columnes >= 7"
estrategia_invalida = print "Estrategia no valida"

-- FI USAGE ------------------------------------------------------------------


-- CREEM EL TAULELL I L'INICIALITZEM ------------------------------------------------------------------

-- Creem el taulell inicial com una matriu de N 
-- files i M columnes, tota a 0
taulell :: Int -> Int -> [Int] -> [[Int]] -> Board
taulell n m l t | length t < m = taulell n m l (insertar_fila n l t)
				| otherwise = t

insertar_fila :: Int -> [Int] -> [[Int]] -> Board
insertar_fila n l t = t++[columnes n l]

columnes :: Int -> [Int] -> Col
columnes n l = if length l < n then columnes n (insertar_zero l) else l

insertar_zero :: [Int] -> [Int]
insertar_zero [] = [0]
insertar_zero [x] = [x,0]
insertar_zero (x:xs) = (x:xs)++[0]

-- Pintem el taulell
printTaulell :: Board -> [Char]
printTaulell [x] = printColumna x
printTaulell (x:xs) = printColumna x ++"\n"++ printTaulell xs

printColumna :: Col -> [Char]
printColumna [val] = " | " ++ show val ++ " | "
printColumna (x:xs) = " | " ++ show x ++ printColumna xs

-- FI INICIALITZACIÓ DEL TAULELL ------------------------------------------------------------------

-- FUNCIONS AUXILIARS ------------------------------------------------------------------

-- Tornem el primer element d'una llista
first :: [Int] -> Int
first [] = -1
first [x] = x
first(x:xs) = x

-- Retorna el nombre de la primera fila buida de la columna col
pos_columna t m c = if m < 0 then -1 
                    else if t!!m!!c /= 0 then pos_columna t (m-1) c 
                    else m

resta_elements :: (Eq t, Num t) => t -> [a] -> [a]
resta_elements n [] = []
resta_elements 0 (x:xs) = xs
resta_elements n (x:xs) = if n/=0 then resta_elements (n-1) (tail (x:xs)) else xs

-- Comprova si es pot insertar una fitxa en la columna col
valid :: Board -> Int -> Int -> Int -> Bool
valid t m n col | entre_marges n col = columna_plena t m col
              | otherwise = False 

-- Comprova si la fitxa està entre "els marges" de les columnes del taulell
entre_marges :: Int -> Int -> Bool
entre_marges n x | x > n = False 
				 | x < 0 = False 
				 | otherwise = True

fila_plena :: [Int] -> Bool
fila_plena [] = True
fila_plena [x] = if x==0 then False else True
fila_plena (x:xs) = if x==0 then False else fila_plena xs

columna_plena :: Board -> Int -> Int -> Bool
columna_plena t m col = if t!!0!!col == 0 then True else False

-- Torna la llista formada pel nombre més gran d'elements contigus en una llista donada
majoria :: [Int] -> [Int]
majoria [] = []
majoria [x] = [x]
majoria (x:xs) | (es_fitxa [(first ls)]) && length ls >= length m = ls 
			   | otherwise = m
  where ls = llista_seguida (x:xs)
        m = majoria (resta_elements_seguida (x:xs)) 
        

-- Llista dels primers nombres seguits 
llista_seguida :: [Int] -> [Int]
llista_seguida [] = []
llista_seguida [x] = [x]
llista_seguida (x:xs) | x == q = [x]++(llista_seguida xs) 
					  | otherwise = [x]
  where q = first xs

-- Llista sense els primers nombres seguits
resta_elements_seguida :: [Int] -> [Int]
resta_elements_seguida [] = []
resta_elements_seguida [x] = []
resta_elements_seguida (x:xs) = if x==q then resta_elements_seguida xs else xs 
  where q = first xs
  
es_fitxa :: [Int] -> Bool
es_fitxa l = if (first l) == 1 || (first l) == 2 then True else False

altre_jugador 1 = 2
altre_jugador 2 = 1

-- FI FUNCIONS AUXILIARS ------------------------------------------------------------------


-- INSERCIÓ D'UNA PEÇA ------------------------------------------------------------------

insertar ::  Int -> Board -> Int -> Int -> Board
insertar x t col m = inserlist (inser x col (t!!f)) f t
  where f = pos_columna t m col 
        
-- Inserta x en la posició n de la llista donada
inser :: Int -> Int -> [Int] -> [Int]
inser x n [] = [x]
inser x n (y:ys) = q++[x]++r
  where q = take n (y:ys)
        r = resta_elements n (y:ys)

inserlist :: [Int] -> Int -> [[Int]] -> [[Int]]
inserlist i col [[]] = [i]
inserlist i col (y:ys) = q++[i]++r
  where q = take col (y:ys)
        r = resta_elements col (y:ys)

-- FI DE INSERTAR -------------------------------------------------------------------

-- FUNCIÓ PER COMPROVAR SI HI HA GUANYADOR -------------------------------------------------------------

-- Comprovació HORITZONTAL, VERTICAL i DIAGONAL(s)
comprovacio :: Board -> Int -> Int -> [Int] -> Bool
comprovacio t m n j = if v || h || d1 || d2 then True else False
  where v = comprov_vertical t m n
        h = comprov_horitzontal t m
        d1 = diagonal1 t m n j
        d2 = diagonal2 t m n j
		
-- Comprovació EMPAT
empat :: Board -> Int -> Bool
empat t 0 = fila_plena (t!!0)
empat t m | fila_plena (t!!m) = empat t (m-1) 
		  | otherwise = False

-- HORITZONTAL 
comprov_horitzontal :: Board -> Int -> Bool
comprov_horitzontal t m | m < 0 = False 
						| otherwise = comprov_horitzontal' t m

comprov_horitzontal' t m | horitzontal fila = True 
						 | otherwise = comprov_horitzontal t (m-1)
  where fila = t!!m

horitzontal :: [Int] -> Bool
horitzontal l = if es_fitxa m then
				if length m >= 4 then True 
				else False
               else False
  where m = majoria l

-- VERTICAL --
columnes_to_llista :: Board -> Int -> Int -> [Int]
columnes_to_llista t m n | m > 0 = [t!!m!!n]++columnes_to_llista t (m-1) n 
					 | otherwise = [t!!0!!n]

comprov_vertical :: Board -> Int -> Int -> Bool
comprov_vertical t m n = if n > 0 then 
                if comprov_vertical' t m n == False then comprov_vertical t m (n-1) else True
                else comprov_vertical' t m 0

comprov_vertical' t m n = vertical q
  where q = columnes_to_llista t m n 

vertical :: Col -> Bool
vertical l = if es_fitxa m then 
				  if length m >= 4 then True 
				  else False
             else False
  where m = majoria l

-- DIAGONAL
-- Creem una llista a partir de la primera diagonal
llista_diagonal1 :: Board -> Int -> Int -> [Int] -> [Int]
llista_diagonal1 t m n j | length j < 4 = llista_diagonal1 t (m-1) (n-1) (j++[t!!m!!n]) 
                        | otherwise = j

-- Comprovem si en aquesta diagonal tenim 4 en ratlla
llista_diagonal1Comprov :: [Int] -> Bool
llista_diagonal1Comprov l = if es_fitxa m then 
							if length m >= 4 then True 
							else False
						   else False
  where m = majoria l

{- Diagonal 1
*
  *
    * 
      *   -}
diagonal1 :: [[Int]] -> Int -> Int -> [Int] -> Bool
diagonal1 t m n j = if m > (m-(m-3)) then 
						if rec_horitzontal1 t m n j then True
                        else diagonal1 t (m-1) n j                              
                    else rec_horitzontal1 t m 3 j


-- Recorregut horitzontal de les diagonals de la fila m 
rec_horitzontal1 :: Board -> Int -> Int -> [Int] -> Bool
rec_horitzontal1 t m n j = if n > (n-(n-3)) then
							  if llista_diagonal1Comprov p then True
                              else rec_horitzontal1 t m (n-1) j
                          else llista_diagonal1Comprov q
  where p = llista_diagonal1 t m n j
        q = llista_diagonal1 t m 3 j

{- Diagonal2
        *
      *
    *
  *          -}

-- Creem una llista a partir de la segona diagonal
llista_diagonal2 :: Board -> Int -> Int -> [Int] -> [Int]
llista_diagonal2 t m n j = a++b++c++d
  where a = [t!!(m-3)!!n]
        b = [t!!(m-2)!!(n-1)]
        c = [t!!(m-1)!!(n-2)]
        d = [t!!m!!(n-3)]

-- Comprovem si en aquesta segona diagonal tenim 4 en ratlla
llista_diagonal2Comprov :: [Int] -> Bool
llista_diagonal2Comprov l = if es_fitxa m then 
								if length m >= 4 then True 
								else False
							else False
  where m = majoria l
  
rec_horitzontal2 :: Board -> Int -> Int -> [Int] -> Bool
rec_horitzontal2 t m n j = if n > (n-(n-3)) then 
								if llista_diagonal2Comprov p then True 
                                else rec_horitzontal2 t m (n-1) j
                           else llista_diagonal2Comprov q
  where p = llista_diagonal2 t m n j
        q = llista_diagonal2 t m 3 j

diagonal2 :: [[Int]] -> Int -> Int -> [Int] -> Bool
diagonal2 t m n j = if m > (m-(m-3)) then 
						if rec_horitzontal2 t m n j then True
                        else diagonal2 t (m-1) n j                              
                    else rec_horitzontal2 t m 3 j


-- FI FUNCIÓ COMPROVAR ---------------------------------------------------------------

-- JUGADORS ---------------------------------------------------------------
human :: Board -> Int -> Int -> Int -> IO ()
human t n m jugador = do
  putStrLn (printTaulell t)
  print("---------------------------------------------------------------")
  if comprovacio t m n [] then print ("GUANYA EL JUGADOR", altre_jugador jugador)
     else do
        if empat t m then print "EMPAT"
           else do
               print ("JUGADOR ", jugador, ": Introdueix una columna [1..N]:")
               c <- getLine
               let col = ((read c)-1)
               if valid t m n col then do 
                    let z = (insertar jugador t col m)
                    human z n m (altre_jugador jugador)   
                  else do 
                    print "COLUMNA INVALIDA"
                    human t n m jugador

human' :: Board -> Int -> Int -> (Board -> Int -> Int -> Int -> IO Int) -> IO ()
human' t n m func = do
  putStrLn (printTaulell t)
  print("---------------------------------------------------------------")
  if comprovacio t m n [] then print "MACHINE WINS!"
     else do
       if empat t m then print "EMPAT"
          else do
               putStrLn "JUGADOR 1: Introdueix una columna [1..N]"
               c <- getLine
               let col = ((read c)-1)
               if valid t m n col then do 
                    let z = (insertar 1 t col m)
                    machine z n m func
                  else do 
                    print "COLUMNA INVALIDA"
                    human' t n m func
  

machine :: Board -> Int -> Int -> (Board -> Int -> Int -> Int -> IO Int) -> IO ()
machine t n m func = do
  putStrLn (printTaulell t)
  print("---------------------------------------------------------------")
  if comprovacio t m n [] then print "HAS GUANYAT!"
     else do
          if empat t m then print "EMPAT"
             else do
			   col <- func t n m n
			   if valid t m n col then do
					let z = (insertar 2 t col m)
					human' z n m func
				
				else machine t n m func

-- FI JUGADORS ---------------------------------------------------------------

-- ESTRATEGIES ---------------------------------------------------------------
--RANDOM
randInt :: Int -> Int -> IO Int
randInt high low = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result

-- Nota: t col i m son inutils aqui...
choose_random :: Board -> Int -> Int -> Int -> IO Int
choose_random t col m n = randInt 0 n

--FI DE RANDOM

-- Mira les adjacencies en horitzontal a una 'col' donada
adj_horitzontal :: Board -> Int -> Int -> Int -> (Int -> Int -> Int) -> Int -> Int
adj_horitzontal t m n col (extra) jugador = 
	if col >= 0 && col <= n then 
		if t!!m!!col == jugador then 
			if (extra col 1) >= 0 && (extra col 1) <= n && t!!m!!(extra col 1)== jugador then 2
			else 1
		else 0
	else 0	

-- adjacencia maxima (horitzontal,vertical) que te l'ultim element insertat de 'col'
adjacencies :: Board -> Int -> Int -> Int -> Int
adjacencies t m n col = do
	let pos_col_top = t!!0!!col
	let l_vert | pos_col_top == 0 = llista_seguida (resta_elements_seguida(reverse(columnes_to_llista t m col)))
				| otherwise = llista_seguida (columnes_to_llista t m col)
	let max_vertical = length(l_vert)
	let fila | pos_col_top == 0 = (pos_columna t m col)+1
				| otherwise = 0
	let max_horitzontal = 1 + (adj_horitzontal t fila n (col+1) (+) 2) + (adj_horitzontal t fila n (col-1) (-) 2) 
	max max_vertical max_horitzontal

--ESTRATEGIA GREEDY	
-- Construim una llista que evalua, en cada col del taulell, el nombre de 
-- fitxes que s'acumulen (nomes per a la maquina)
construeix_llista :: Board -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
construeix_llista t m n col l = do
	if col < 0 then l
	else do
		if valid t m n col then do
			let z = (insertar 2 t col m)
			let maxim = adjacencies z m n col 
			let l1 = l ++ [(col, maxim)]
			construeix_llista t m n (col-1) l1
		else construeix_llista t m n (col-1) l

-- retorna la columna (o una d'elles, si hi ha varies) on insertar 
-- un element implica acumular el màxim nombre de fitxes per la maquina
apila :: Board -> Int -> Int -> Int
apila t m n = do
	let l = construeix_llista t m n n []
	fst(maximumBy (comparing snd) l)

choose_greedy :: Board -> Int -> Int -> Int -> IO Int
choose_greedy t col m n = 
	if col < 0 then return (apila t m n)
	else if valid t m n col then do 
		let z1 = (insertar 1 t col m)
		let z2 = (insertar 2 t col m)
		if comprovacio z1 m n [] || comprovacio z2 m n [] then return col
		else choose_greedy t (col-1) m n
	else choose_greedy t (col-1) m n

--ESTRATEGIA "SMART"...
--Aquesta estrategia nomes es un refinament de la versio purament greedy. 
--El que fa es evaluar les possibles tirades del jugador huma, a part 
--de la propia, i intentar minimitzar les seves fitxes acumulades
--Evita tambe posar fitxes en columnes on es impossible fer 4 en ratlla	
adjacencies_smart :: [[Int]] -> Int -> Int -> Int -> Int -> Int
adjacencies_smart t m n col jugador = do
	let pos_col_top = t!!0!!col
	let primera_fila = pos_columna t m col
	let l_vert | primera_fila < 1 = []
				| pos_col_top == 0 = llista_seguida (resta_elements_seguida(reverse(columnes_to_llista t m col)))
				| otherwise = llista_seguida (columnes_to_llista t m col)
	let max_vertical | (primera_fila+1+length(l_vert)) < 4 = 0
					  | otherwise = length(l_vert)
	let fila | pos_col_top == 0 = (pos_columna t m col)+1
				| otherwise = 0
	let max_horitzontal | (col + 1 + (adj_horitzontal t fila n (col+1) (+) jugador) + (adj_horitzontal t fila n (col-1) (-) jugador)) > 4 = (1 + (adj_horitzontal t fila n (col+1) (+) jugador) + (adj_horitzontal t fila n (col-1) (-) jugador))
						| otherwise = 0
	max max_vertical max_horitzontal

-- Construim una llista que evalua "de forma smart" en cada col del taulell, el nombre de 
-- fitxes que s'acumulen per a 'jugador', si a la columna es pot arribar a un 4 en ratlla.
-- Altrament es 'puntuarà' amb valor 0
construeix_llista_smart :: Board -> Int -> Int -> Int -> [(Int, Int)] -> Int -> [(Int, Int)]
construeix_llista_smart t m n col l jugador = do
	if col < 0 then l
	else do
		if valid t m n col then do
			let z = (insertar jugador t col m)
			let maxim = adjacencies_smart z m n col jugador
			let l1 = l ++ [(col, maxim)]
			construeix_llista_smart t m n (col-1) l1 jugador
		else construeix_llista_smart t m n (col-1) l jugador
		
		
-- retorna la columna (o una d'elles, si hi ha varies) 
-- on insertar un element suposa acumular el màxim nombre de fitxes
-- de qualsevol dels dos jugadors
apila_smart :: Board -> Int -> Int -> Int
apila_smart t m n = do
	let l1 = construeix_llista_smart t m n n [] 1
	let l2 = construeix_llista_smart t m n n [] 2
	let max_1 = snd(maximumBy (comparing snd) l1)
	let max_2 = snd(maximumBy (comparing snd) l2)
	if max_2 >= max_1 then fst(maximumBy (comparing snd) l2)
	else fst(maximumBy (comparing snd) l1)
	
choose_smart :: Board -> Int -> Int -> Int -> IO Int
choose_smart t col m n = 
	if col < 0 then return (apila_smart t m n)
	else if valid t m n col then do 
		let z1 = (insertar 1 t col m)
		let z2 = (insertar 2 t col m)
		if comprovacio z1 m n [] || comprovacio z2 m n [] then return col
		else choose_smart t (col-1) m n
	else choose_smart t (col-1) m n
	
-- FI DE LES ESTRATEGIES ---------------------------------------------------------------
	
-- MAIN ---------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Introdueixi M, el nombre de files"
  nombre_files <- getLine
  putStrLn "Introdueixi N, el nombre de columnes"
  nombre_columnes <- getLine
  let m = (read nombre_files)
  let n = (read nombre_columnes)
  putStrLn "Esculli un mode de joc: \n1. Human vs Human\n2. Random\n3. Greedy\n4. Smart"
  estrategia <- getLine
  let e = (read estrategia)
  if m < 6 || n < 7 then usage
  else if e < 1 || e > 4 then estrategia_invalida
     else do
       putStrLn "QUATRE EN RATLLA\n"
       let t = taulell n m [] []
       if e == 1 then human t (n-1) (m-1) 1 
	   else if e == 2 then human' t (n-1) (m-1) choose_random
	   else if e == 3 then human' t (n-1) (m-1) choose_greedy
	   else human' t (n-1) (m-1) choose_smart

-- FI MAIN ---------------------------------------------------------------