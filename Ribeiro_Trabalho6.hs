--1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado. 

divisoresden:: Int ->[Int]
divisoresden inputInt = [x | x <- [1..inputInt-1], (mod inputInt x)==0] 


--2. Usando  List Comprehension  escreva  uma  função,  chamada  contaCaractere,  que  conte  a ocorrência de um caractere específico, em uma string dada.

contaCaractere:: Char -> String -> Int
contaCaractere inputChar inputString = length [x | x <- inputString, inputChar==x]

--3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada. 

dobroNaoNegativo:: [Int]->[Int]
dobroNaoNegativo listaInput = [x*2 | x <- listaInput, (x>=0)]

--4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado.

pitagoras:: Int ->[(Int, Int, Int)]
pitagoras inputInt = [(a,b,c) | a <- [1..inputInt], b <- [1..inputInt], c <- [1..inputInt], ((a^2 + b^2) == c^2)]

--5. Números  perfeitos  são  aqueles  cuja  soma  dos  seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado. 

isNumeroPerfeito :: Int->Bool
isNumeroPerfeito inputInt = (foldr (+) 0 (divisoresden inputInt) ) == inputInt

numerosPerfeitos :: Int -> [Int]
numerosPerfeitos inputInt = [x | x <- [1..inputInt],isNumeroPerfeito x]


--6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções c, snd e zip no prelude que podem ser úteis. 

somarVetor:: [Int]->Int
somarVetor vetor = foldr (+) 0 vetor

produtoEscalar:: [Int]->[Int]->Int
produtoEscalar lista1 lista2 = somarVetor [(fst x) * (snd x) | x<-zip lista1 lista2]

--7. Usando  List Comprehension  escreva  uma  função,  chamada  primeirosPrimos,  que  devolva uma lista contendo os n primeiros números primos a partir do número 2.

primeirosPrimos:: Int -> [Int]
primeirosPrimos intInput = [x | x<-[2..intInput], (divisoresden x)==[1]]

--8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva uma  lista  de  par  ordenados  contendo  uma  potência  de  2  e  uma  potência  de  3  até  um determinado número dado. Observe que estes números podem ser bem grandes
paresOrdenados:: Int->[(Int,Int)]
paresOrdenados intInput = [(x*x, x*x*x) | x <- [0..intInput]]

main = do
  --Q1
  putStrLn $ "Func.1: entrada: 10 resultado:" ++ show (divisoresden 10)
  putStrLn ""
  --Q2
  putStrLn $ "Func.2: entrada: a casa resultado:" ++ show (contaCaractere 'a' "casa")
  putStrLn ""
  --Q3
  putStrLn $ "Func.3: entrada: [-3,-2,-1,0,1,2,3] resultado:" ++ show (dobroNaoNegativo [-3,-2,-1,0,1,2,3])
  putStrLn ""
  --Q4
  putStrLn $ "Func.4: entrada: 15 resultado:" ++ show (pitagoras 15)
  putStrLn ""
  --Q5
  putStrLn $ "Func.5: entrada: 1000 resultado:" ++ show (numerosPerfeitos 1000)
  putStrLn ""
  --Q6
  putStrLn $ "Func.6: entrada: [1,2,3] [1,2,3] resultado:" ++ show (produtoEscalar [1,2,3] [1,2,3])
  putStrLn ""
  --Q7
  putStrLn $ "Func.7: entrada: 20 resultado:" ++ show (primeirosPrimos 20)
  putStrLn ""
  putStrLn ""
  --Q8
  putStrLn $ "Func.8: entrada: 3 resultado:" ++ show (paresOrdenados 3)
  putStrLn ""




