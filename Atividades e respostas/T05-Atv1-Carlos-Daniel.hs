--Q1
testar x y z | x==y && x==z = 3
             | x==y || x==z || z==y = 2
             | x/=y && x/=z && z/=y = 0
--Q2
media a b c = (a+b+c)/3
avaliar a b c | a > (media a b c) && b > (media a b c) && c > (media a b c) = 3
              | a > (media a b c) && b > (media a b c) = 2
			  | a > (media a b c) && c > (media a b c) = 2
			  | b > (media a b c) && c > (media a b c) = 2
              | a > (media a b c) || b > (media a b c) || c > (media a b c) = 1
			  | otherwise = 0
--Q3
potencia_2 x = x*x
--Q4
potencia_4 x = potencia_2 x * potencia_2 x
--Q5
xor a b = (a||b)&&not(a&&b)
--Q6
delta a b c = sqrt(b^2 - 4 * a *c)
x_maior a b c = (- b + (delta a b c)) / 2*a
x_menor a b c = (- b - (delta a b c)) / 2*a
