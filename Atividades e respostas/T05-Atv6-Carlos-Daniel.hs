--Q1 Dadas duas datas (d1,m1,a1) e (d2,m2,a2), tal que data1<=data2, 
--Q1 construa uma função que retorne quantos dias existem entre estas duas datas,
--Q1 onde di define o dia do mês mj no ano ak.
comparador x y
 | x > y = x-y
 | y > x = y-x

soma_data (d,m,a) = (d)+(m*30)+(a*365)

datas (d1,m1,a1) (d2,m2,a2)
 | soma_data (d1,m1,a1) <= soma_data (d2,m2,a2) = (comparador d1 d2) + ((comparador m1 m2)*30) + ((comparador a1 a2)*365)
 | otherwise = error "data1 maior que data2"

--Q2 Crie uma função que receba os coeficientes de uma equação do segundo grau ax2 + bx + c = 0 na forma (a,b,c)
--Q2 e retorne as raízes desta equação. Trate o caso de raízes imaginárias, indicando um erro. 
delta a b c
 | sqrt(b^2 - 4 * a *c) >= 0 = sqrt(b^2 - 4 * a *c)
 | otherwise = error "Nao existe razao pelas coisas feitas pelo coracao"

x_maior a b c = (- b + (delta a b c)) / 2*a
x_menor a b c = (- b - (delta a b c)) / 2*a

raizes (a, b, c) = (x_maior a b c, x_menor a b c)

--Q3 Construa uma função que, dados três valores, verifique se os mesmos podem ser os
--Q3 lados de um triângulo. Se for possível formar o triângulo, retorne uma tupla-2 com o
--Q3 tipo do triângulo formado (com relação às arestas) e o perímetro do mesmo. Exemplo:
--Q3 triangulo (7,7,11) ,Resposta ("Isóceles", 25)
triangulo (a, b, c)
 | a == b && a == c = ("Equilateral", a+b+c)
 | a == b && a /= c = ("Isosceles", a+b+c)
 | b == c && a /= c = ("Isosceles", a+b+c)
 | c == a && a /= b = ("Isosceles", a+b+c)
 | a /= b && a /= c = ("Escaleno", a+b+c) 

--Q4 base :: Int -&gt; (Int, String, String, Char)
--Q4 base x
--Q4 |x==0 = (1793, "Pedro Paulo", "MESTRE", 'M')
--Q4 |x==1 = (1797, "Joana Silva Alencar", "MESTRE", 'F')
--Q4 |x==2 = (1534, "João De Medeiros", "DOUTOR", 'M')
--Q4 |x==3 = (1267, "Cláudio Cédar de Sá", "DOUTOR", 'M')
--Q4 |x==4 = (1737, "Paula de Medeiros", "MESTRE", 'F')
--Q4 |x==5 = (1888, "Rita de Matos", "MESTRE", 'F')
--Q4 ........
--Q4 |x==9 = (1698, "Tereza Cristina Andrade", "MESTRE", 'F')
--Q4 |x==10 = (0, " ", " ", '0')
--Q4 (a) O número de doutores na base.
--Q4 (b) O número de mulheres.
--Q4 (c) O número de mestres do sexo masculino.
--Q4 (d) O nome do professor mais antigo. (número de menor matrícula)

type Meu_tipo = (Int, String, String, Char)

idade :: Meu_tipo -> Int
idade (w,x,y,z) = w

nome :: Meu_tipo -> String
nome (w,x,y,z) = x

tipo :: Meu_tipo -> String
tipo (w,x,y,z) = y

sexo :: Meu_tipo -> Char
sexo (w,x,y,z) = z

comparador_maior :: Meu_tipo -> Meu_tipo -> Meu_tipo
comparador_maior x y
 | x2 <= y2 = x
 | otherwise = y
   where
    x2 = idade x
    y2 = idade y

base x
 |x==1 = (1793, "Pedro Paulo", "MESTRE", 'M')
 |x==2 = (1797, "Joana Silva Alencar", "MESTRE", 'F')
 |x==3 = (1534, "João De Medeiros", "DOUTOR", 'M')
 |x==4 = (1267, "Cláudio Cédar de Sá", "DOUTOR", 'M')
 |x==5 = (1737, "Paula de Medeiros", "MESTRE", 'F')
 |x==6 = (1888, "Carlos", "MESTRE", 'F')
 |x==7 = (1889, "Daniel", "MESTRE", 'F')
 |x==8 = (1890, "Rezende", "MESTRE", 'F')
 |x==9 = (1891, "Euzebio", "MESTRE", 'F')
 |x==10 = (1698, "Tereza Cristina Andrade", "MESTRE", 'F')
 |x==11 = (0, " ", " ", '0')

a :: Float -> Float -> Float
a x conta 
 | x == 0 = conta
 | tipo (base x) /= "DOUTOR" = a (x-1) conta
 | otherwise = a (x-1) (conta+1)

b :: Float -> Float -> Float
b x conta 
 | x == 0 = conta
 | sexo (base x) /= 'F' = b (x-1) conta
 | otherwise = b (x-1) (conta+1)

c :: Float -> Float -> Float
c x conta 
 | x == 0 = conta
 | sexo (base x) /= 'M' = c (x-1) conta
 | otherwise = c (x-1) (conta+1)

d :: Int -> Meu_tipo
d x
 | x == 1 = base 1
 | otherwise = comparador_maior (base x) (d (x-1))