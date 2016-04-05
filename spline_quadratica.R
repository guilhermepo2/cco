spline_quadratica <- function(X,Y,z,n)
{
	# 2 variaveis para cada spline (n-1 splines)
	# 2 * (n-1) = 2n - 2
	# Mas, c0 é definido como 0, então
	# incognitas = 2n - 2 - 1 = 2n - 3
	
	tamanho_incognitas <- 2 * n - 3
	
	# Calculando os indices ai das splines
	# Condição 1
	Ai <- Y
	
	#Condição 2 - Primeiras 3 linhas da Matriz
	ddv <- seq(0,0, length.out = tamanho_incognitas) # diferencas divididas
	Hi <- c() # (xi+1 - xi)

	for(i in 1:(n-1))
	{
		Hi[i] <- (X[i+1] - X[i])
		ddv[i] <- (Y[i+1] - Y[i]) / Hi[i]
	}
	
	matriz_diferenca_divididas <- matrix(ddv, nrow=tamanho_incognitas, 1)
	matriz_coeficientes <- matrix(0, nrow=tamanho_incognitas, ncol = tamanho_incognitas)
	
	# Calcular a matriz (tamanho x tamanho)
	
	# Primeira Linha da Matriz
	matriz_coeficientes[1,1] <- 1 
	j <- 2
	
	for(i in 2:(n-1))
	{
		matriz_coeficientes[i,j] <- 1
		matriz_coeficientes[i,j+1] <- Hi[i]
		j <- j+2
	}
	
	# Condição 3 - Ultimas Linhas da Matriz
	# bi+1 = bi + cihi
	# b1 = b0 + h0c0 => b1 = b0
	matriz_coeficientes[i+1,1] <- 1
	matriz_coeficientes[i+1,2] <- -1
	
	auxiliar_h <- 2
	k <- 4
	for(j in ((i+2):tamanho_incognitas))
	{
		matriz_coeficientes[j,k-2] <- 1
		matriz_coeficientes[j,k-1] <- 2 * Hi[auxiliar_h]
		matriz_coeficientes[j,k] <- -1
		auxiliar_h <- auxiliar_h + 1
		k <- k + 2
	}
	coeficientes <- solve(matriz_coeficientes, matriz_diferenca_divididas)
	
	# pegando os coeficientes
	
	bi <- c(coeficientes[1])
	ci <- c(0)
	for(i in 2:length(coeficientes))
	{
		if(i %% 2==0)
		{
			bi <- c(bi, coeficientes[i])
		}
		else {
			ci <- c(ci, coeficientes[i])
		}
	}

	# Interpolando os pontos desejados
	resultados <- c()
	for(i in z)
	{
		# i é o valor de z que é o valor a ser interpolado
		for(j in 1:n)
		{
			if(i > X[j] && i < X[j+1])
			{
				res <- Ai[j] + bi[j] * (i - X[j]) + ci[j] * ((i - X[j])^2)
				resultados <- c(resultados, res)
			}
		}
	}
	
	# print (resultados)
	return(resultados)
	
}

X <- c(3.0, 4.5, 7.0, 9.0)
Y <- c(2.5, 1.0, 2.5, 0.5)
z <- c(4.0, 5.0, 8.0, 8.5)
n <- length(X)

# spline_quadratica(X,Y,z,n)