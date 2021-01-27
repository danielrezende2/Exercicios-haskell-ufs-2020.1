--Q1
incluindo n1 n2 = sum [n1..n2]
excluindo n1 n2 = sum [n1..n2] - (n1+n2)
--Q2
dado n1 n2 n3 = [x | x <- [n1..n2], x `mod` n3 == 0 ] 
--Q3
replicar n1 n2 | n1 < 0 = replicate n2 n1
               | otherwise = replicate n1 n2
soma n1 n2 = sum (replicar n1 n2)