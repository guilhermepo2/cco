# Polinomio de Lagrange (acho que esta errado)
# Calcular o erro para cada ponto e nao o erro medio
# plotar o polinomio interpolado e nao so os pontos dele (plotar os dois)



# Exercicio Simulação

# Exercício 01
# Dados os pontos abaixo, calcular o polinômio interpolador  
#  i   0      1
#  x  0.1    0.6
#  y 1.221 3.3320
#
# Sabendo que f(x) = e^2x, calcule o erro relativo verdadeiro percentual na interpolação de z1 = 0.2 e z2 = 0.3
# Esboçar os gráficos dos polinômios de Newton e Lagrange, juntamente com o gráfico da função

exercicio1 <- function()
{
	xe <- c(.1, .6)
	ye <- c(1.221, 3.3320)	
	ze <= c(.2, .3)
	interpolar(xe, ye, ze, metodo="newton", valores_verdadeiros=exp(2*ze))
}

# Exercicio 02 
# Dada a função f(x) = e^x, determine 8 pontos no intervalo [2.4, 3.8], onde a largura entre os pontos é .2. Em seguida, determine um polinômio de segundo grau e interpole z=3.1. Calcule o erro verdadeiro percentual.

exercicio02 <- function()
{
	xe <- seq(from=2.4, to=3.8, length.out=8)
	ye <- exp(xe)
	ze <- 3.1
	interpolar(xe,ye,ze, nro_pontos=3, metodo="lagrange", valores_verdadeiros=exp(ze))
}

# Exercicio 3
# Sabendo que a função f(x) = x - eˆ^(-x) admite uma raiz no intervalo (0,1),
# determine esta raiz usando interpolação quadrática

exercicio03 <- function()
{
	xe <- seq(from=0,to=1,length.out=3)
	ye <- xe - exp(-xe)
	ze <- seq(from=0, to=1, length.out = 1000)
	interpolados <- interpolar(xe, ye, ze, nro_pontos=3, metodo="newton")
}

# Exercicio 04
# Encontrar uma aproximacao para z = 0.25, utilizando splines cubicas naturais, dados os pontos
# x 0   0.5    1.0      1.5    2.0
# y 3 1.8616 -0.5571 -4.1987 -9.0536
# f(x) possui raiz neste intervalo dos pontos?
exercicio04 <- function()
{
	xe <- c(0, 0.5, 1.0, 1.5, 2.0)
	ye <- c(3.0, 1.8616, -0.5571, 4.1987, -9.0536)
	ze <- seq(from=0.1, to=1.9, length.out = 100)
	interpolar(xe,ye,0.25,metodo="spline_linear")
}

