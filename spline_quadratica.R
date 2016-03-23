spline_quadratica <- function(X,Y,z,n)
{
	# 2 variaveis para cada spline (n-1 splines)
	# 2 * (n-1) = 2n - 2
	# Mas, c0 é definido como 0, então
	# incognitas = 2n - 2 - 1 = 2n - 3
	
	tamanho_incognitas <- 2 * n - 3
	
	# Calculando os indices ai das splines
	Ai <- Y
	
	# Calculando os indices bi
	# Forma dos indices bi:
	#		f(x(i+1)) - f(xi) / (x(i+1) - xi) * (x - xi)
	Bi <- c() # indices bi
	Hi <- c() # denominador da diferenca dividida
	for(i in 1:(n-1))
	{
		Hi[i] <- (X[i+1] - X[i])
		Bi[i] <- (Y[i+1] - Y[i]) / Hi[i]
	}
		
		
	# Preencho o vetor de Bi com 0 para que fique com o tamanho do numero de incognitas
	for(i in n:tamanho_incognitas)
		Bi[i] <- 0
		
	# Fazendo a matriz transposta do vetor de Bi
	matriz_diferencas_divididas <- matrix(Bi, nrow=tamanho_incognitas, 1)
	
	# Calcular a matriz (tamanho x tamanho)
	
}

X <- c(3.0, 4.5, 7.0, 9.0)
Y <- c(2.5, 1.0, 2.5, 0.5)
z <- c(4.0, 5.0, 8.0, 8.5)
n <- length(X)

spline_quadratica(X,Y,z,n)