--Q1
-- "x" se refere a um numero qualquer, "n" o valor de termos
valor_Da_Serie x n = sum[(x ** e) / (product [1..e]) | e <- [1..n]] + 1
valor_Analitico x = exp x
avalie x n | (valor_Analitico x - valor_Da_Serie x n) > 0.001 = "invalido"
           | (valor_Analitico x - valor_Da_Serie x n) < 0.001 = "valido"
--Q2
mod1 :: Int -> Int -> Int
mod1 a b = a `div` b
mod2 a b = a - (b*(mod1 a b))
--Q3
f :: Int -> Float
f n
      |n > 0 = sqrt (6 + f (n-1))
      |n <= 0 = 0.00
