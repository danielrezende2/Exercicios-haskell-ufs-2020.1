--Q1 Implemente a fórmula que indica de quantas maneiras é possível escolher n objetos de uma coleção de m objetos,
--Q1 onde m ≥ n. Utilizar recursividade em cauda na função fatorial. m!/n! (m-n)!
fatorial n1
 | n1 == 0 = 1
 | n1 > 0 = fatorial(n1-1)*n1

formula m n = fatorial(m)/fatorial(n)*fatorial(m-n)

--Q2 Construa uma função que retorne o MDC entre dois números inteiros, e, caso contrário, retorne 0
mdc n1 n2
 | mod n1 n2 == 0 = n2
 | mod n2 n1 == 0 = n1
 | n1 > n2 = (n1-n2) `mod` n2 
 | n2 > n1 = (n2-n1) `mod` n1

mdc3 n1 n2 n3 
 | n1 > n3 && n2 > n3 = mdc n3 (mdc n1 n2)
 | n1 > n2 && n3 > n2 = mdc n2 (mdc n1 n3) 
 | n2 > n1 && n3 > n1 = mdc n1 (mdc n2 n3)

--Q3 Construa uma função que retorne o MMC entre três números inteiros.
mmc n1 n2 
 | n1 == 0 = 0
 | n2 == 0 = 0
 | n1 == n2 = n1
 | otherwise = div (n1 * n2) (mdc n1 n2)

mmc3 n1 n2 n3
 | n1 > n3 && n2 > n3 = mmc n3 (mmc n1 n2)
 | n1 > n2 && n3 > n2 = mmc n2 (mmc n1 n3) 
 | n2 > n1 && n3 > n1 = mmc n1 (mmc n2 n3)

--Q4 Construa uma função que calcule a raiz quadrada inteira de um número inteiro.
--Q4 Implemente uma função que a partir de um número fornecido pelo usuário calcule o valor inteiro.
raiz x = truncate (sqrt x)

--Q5 Construa a função de ackermann, a qual é definida por:
--Q5 1. a(m,n) = n + 1 se m = 0
--Q5 2. a(m,n) = a(m – 1,1)  se m ≠ 0 e n = 0
--Q5 3. a(m,n) = a(m – 1, a(m,n – 1))  se m ≠ 0 e n ≠ 0
ackermann m n 
 | m == 0 = n+1
 | m /= 0 && n == 0 = ackermann (m-1) 1
 | m /= 0 && n /= 0 = ackermann (m-1) (ackermann m (n-1)) 