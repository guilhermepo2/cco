source("cco-interpolacaonewton.r")
source("lagrange.r")
source("spline_linear.r")
source("spline_quadratica.r")


achar_intervalo <- function(X,zs, grau)
{
	menor_z <- min(zs)
	maior_z <- max(zs)
	maior_menor_x <- length(X)
	intervalo <- X
	
	maior_i <- length(X)
	menor_i <- 1
	for(i in 1:length(X))
	{
		if(X[i] <= menor_z)
		{
			menor_i <- i
		}
		
		print(intervalo)
		print(X)
		print(i)
		print(maior_menor_x)
		if(X[i] >= maior_z && X[i] < X[maior_menor_x])
		{
			maior_menor_x <- i
			maior_i <- i
		}	
	}
	intervalo <- X[menor_i:maior_i]
	
	# Fazer o tamanho do intervalo corresponder com o grau
	
	print(intervalo)
	
	
}

interpolar <- function(x,y,zs,n, metodo="t",valor_verdadeiro = NULL)
{
	# X -> conjunto de valores no eixo x
	# Y -> conjunto de valores no eixo y
	# zs -> valores a serem interpolados (eixo x)
	# n -> quantidade de pontos
	# valor_verdadeiro -> valor verdadeiro do ponto interpolado (valido somente se passar apenas um ponto para ser interpolado)
}
