spline_linear <- function(X, Y, zs, n)
{
	# X -> coordenadas X dos pontos
	# Y -> coordenadas Y dos pontos
	# z -> ponto(s) a ser(em) interpolado(s)
	# n -> quantidade de pontos em X e Y
	
	
	# Calculando os indices ai das splines
	Ai <- Y
	
	# Calculando os indices bi
	# Forma dos indices bi:
	#		f(x(i+1)) - f(xi) / (x(i+1) - xi) * (x - xi)
	Bi <- c()
	for(i in 1:(n-1))
		Bi[i] <- (Y[i+1] - Y[i]) / (X[i+1] - X[i])
		
	# Calculando o valor f(z) para um z entre x0 e xn
	resultados <- c()
	for(z in zs)
	{
		count <- 1
		while(!(X[count] <= z && X[count+1] > z)) { count <- count + 1}
	
		resultados <- c(resultados, Ai[count] + Bi[count] * (z - X[count]))
	}
	print(resultados)
	return(resultados)
}

X <- c(3.0, 4.5, 7.0, 9.0)
Y <- c(2.5, 1.0, 2.5, 0.5)
z <- c(4.0, 5.0, 8.0, 8.5)
n <- length(X)

spline_linear(X,Y,z,n)