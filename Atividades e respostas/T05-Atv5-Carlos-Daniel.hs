--Q1 Defina uma função que retorne uma tupla-3 contendo o caractere fornecido com entrada, 
--Q1 o mesmo caractere em letras minúsculas ou maiúsculas, e o seu número da tabela ASCII. 
--Q1 Exemplo: converte ‘b’,  resposta: (‘b’,’B’,98) 
--Q1 Antes de achar que o código está errado use "coverte 'Z' para verificação"
import Data.Char

avalie a 
 | isLower a = toUpper a
 | otherwise = toLower a
converte a = (a,avalie(a), ord a)

--Q2 Seja o cadastro de pessoas dado pela função a seguir:
--Q2 
--Q2 Pessoa rg |rg == 1 = (“João Silva”, 12, ‘m’)
--Q2                   |rg == 2 = (“Jonas Souza”, 51, ‘m’)
--Q2                    ......
--Q2                   |rg == 321 = (“Jocileide Strauss”, 21, ‘f’)
--Q2                   |otherwise = (“Não há ninguém mais”, 9999, ‘x’)
--Q2 
--Q2 Construa funções que retornem os seguintes dados:
--Q2 (a)	O nome da pessoa de menor idade até um determinado registro.
--Q2 (b)	A idade média de todas as pessoas até um dado registro.
--Q2 (c)	O número de pessoas do sexo masculino.
--Q2 (d)	O número do registro da pessoa de maior idade. 
type Meu_tipo = (String, Float, Char)

pessoa :: Float -> Meu_tipo
pessoa rg
 | rg == 1 = ("Candido Portinari", 81, 'm')
 | rg == 2 = ("Jonas Souza", 51, 'f')
 | rg == 3 = ("Joice Silva", 22, 'f')
 | rg == 4 = ("Jocileide Strauss", 21, 'f')
 | rg == 5 = ("Joao Silva", 12, 'm')
 | otherwise = ("Nao ha mais ninguem", 0, 'x')

nome :: Meu_tipo -> String
nome (x,y,z) = x

idade :: Meu_Tipo ->Float
idade (x,y,z) = y

sexo :: Meu_tipo -> Char
sexo (x,y,z) = z

conta_masc :: Float -> Float
conta_masc x = conta x 0

nome_maior_idade :: Float-> String
nome_maior_idade x = nome (maior_idade x)

nome_menor_idade :: Float-> String
nome_menor_idade x = nome (menor_idade x)

media_idade :: Float -> Float
media_idade x = (soma_idade x)/x

menor_idade :: Float -> Meu_tipo
menor_idade x
 | x == 1 = pessoa 1
 | otherwise = comparador_menor (pessoa x) (menor_idade(x-1))

maior_idade :: Float -> Meu_tipo
maior_idade x
 | x == 1 = pessoa 1
 | otherwise = comparador_maior (pessoa x) (maior_idade(x-1))

soma_idade x 
 | x == 1 = idade (pessoa 1)
 | otherwise = idade (pessoa x) + (soma_idade(x-1))

conta :: Float -> Float -> Float
conta x cont 
 | x == 0 = cont
 | sexo (pessoa x) /= 'm' = conta (x-1) cont
 | otherwise = conta (x-1) (cont+1)

comparador_menor :: Meu_tipo -> Meu_tipo -> Meu_tipo
comparador_menor x y
 | x1 <= y1 = x
 | otherwise = y
  where
   x1 = idade x
   y1 = idade y

comparador_maior :: Meu_tipo -> Meu_tipo -> Meu_tipo
comparador_maior x y
 | x2 >= y2 = x
 | otherwise = y
   where
    x2 = idade x
    y2 = idade y

--Q3 Construa uma função em que dado um caractere qualquer, retorne uma tupla-3 com o caractere dado
--Q3 , o caractere dado na forma maiúscula/minúscula (o contrário do original) e o número ASCII do original. 
--Q3 Exemplo: analisaLetra ‘h’ , resposta: (‘h’,’H’,104)
analisaLetra a = (a,avalie(a), ord a)

--Q4 Construa uma função em Haskell que recebe 4 inteiros e desenvolve uma tupla-4 com os quatro valores originais, só que ordenados.
ordenados_maior n1 n2
 | n1 >= n2 = n1
 | n2 >= n1 = n2

ordenados_menor n1 n2
 | n1 <= n2 = n1
 | n2 <= n1 = n2
ordenados_media :: Int -> Int -> Int -> Int -> Int
ordenados_media n1 n2 n3 n4 = (n1+n2+n3+n4) `div` 4

ordenados_n1 n1 n2 n3 n4 = ordenados_menor n1 (ordenados_menor n2 (ordenados_menor n3 n4))

ordenados_n2 n1 n2 n3 n4
 | (ordenados_n3 n1 n2 n3 n4) > n1 && (ordenados_n4 n1 n2 n3 n4) > n1 && (ordenados_n1 n1 n2 n3 n4) < n1 = n1
 | (ordenados_n3 n1 n2 n3 n4) > n2 && (ordenados_n4 n1 n2 n3 n4) > n2 && (ordenados_n1 n1 n2 n3 n4) < n2 = n2
 | (ordenados_n3 n1 n2 n3 n4) > n3 && (ordenados_n4 n1 n2 n3 n4) > n3 && (ordenados_n1 n1 n2 n3 n4) < n3 = n3
 | (ordenados_n3 n1 n2 n3 n4) > n4 && (ordenados_n4 n1 n2 n3 n4) > n4 && (ordenados_n1 n1 n2 n3 n4) < n4 = n4

ordenados_n3 n1 n2 n3 n4
 | (ordenados_media n1 n2 n3 n4) < n1 && (ordenados_n4 n1 n2 n3 n4) > n1 = n1
 | (ordenados_media n1 n2 n3 n4) < n2 && (ordenados_n4 n1 n2 n3 n4) > n2 = n2
 | (ordenados_media n1 n2 n3 n4) < n3 && (ordenados_n4 n1 n2 n3 n4) > n3 = n3
 | (ordenados_media n1 n2 n3 n4) < n4 && (ordenados_n4 n1 n2 n3 n4) > n4 = n4

ordenados_n4 n1 n2 n3 n4 = ordenados_maior n1 (ordenados_maior n2 (ordenados_maior n3 n4))

ordenados_crescente n1 n2 n3 n4 = (ordenados_n1 n1 n2 n3 n4, ordenados_n2 n1 n2 n3 n4, ordenados_n3 n1 n2 n3 n4, ordenados_n4 n1 n2 n3 n4)
ordenados_decrescente n1 n2 n3 n4 = (ordenados_n4 n1 n2 n3 n4, ordenados_n3 n1 n2 n3 n4, ordenados_n2 n1 n2 n3 n4, ordenados_n1 n1 n2 n3 n4)