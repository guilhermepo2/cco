spline_cubica_natural <- function(Xs, Ys, Zs, n=length(Xs)-1)
{
	Hi <- c()   # Vetor de Hi
	Ai <- Ys	 	# Vetor Ai = f(xi) primeiras condições
	ddiv <- c() # Vetor de diferenças divididas
	matriz_sistema <- matrix(0, nrow=n-1, ncol=n-1)
	vetor_resultado <- c()
	
	
	# fazendo o cálculo do vetor de Hi e diferenças divididas
	for(i in 1:n)
	{
		Hi[i] <- (Xs[i+1] - Xs[i])
		ddiv[i] <- ((Ys[i+1] - Ys[i]) / Hi[i])
	}
	print(Hi)
	print(ddiv)
	# Calculando a Matriz de Resultados
	for(i in 1:(n-1))
	{
		vetor_resultado[i] <- 6 * (ddiv[i+1] - ddiv[i])
	}
	
	
	# Criando a Matriz Sistema
	
	for(i in 1:(n-1))
	{
		for(j in  1:(n-1))
		{
			if(i==j)
			{
				matriz_sistema[i,j] = 2*(Hi[i] + Hi[1+1])
			}
			else if (i == (j+1))
			{
				matriz_sistema[i,j] = Hi[i]
			}
			else if (i == j-1)
			{
				matriz_sistema[i,j] = Hi[i+1]
			}
		}	
	}
	
	# print(matriz_resultado)
	# print(matriz_sistema)
	
	# resolvendo o sistema para conseguir as segundas derivadas
	segunda_derivada <- solve(matriz_sistema, vetor_resultado)
	segunda_derivada <- c(0,segunda_derivada,0) # primeira e ultima derivada = 0

	# calculando as splines
	interpolados <- c()
	
	for(i in 1:length(Zs))
	{
		for(j in 1:n)
		{
			if(Zs[i] >= Xs[j] && Zs[i] <= Xs[j+1])
			{
				ci <- segunda_derivada[j]/2
				di <- (segunda_derivada[j+1] - segunda_derivada[j]) / (6*Hi[j])
				bi <- ddiv[j] - (((segunda_derivada[j+1] + 2*(segunda_derivada[j]))/6) * Hi[j])
				interpolados[i] <- (Ai[j] + bi * (Zs[i]-Xs[j]) + ci * (Zs[i]-Xs[j])**2) + di * (Zs[i]-Xs[j])**3
				cat(sprintf('S%.0f = %.4f + %.4f * (x - %.4f) + %.4f * (x - %.4f)^2 + %.4f * (x - %.4f)^3 \n', j, Ai[j], bi, Xs[j], ci, Xs[j], di, Xs[j]))
			}
		}
	}
	cat("Segunda Derivada:", segunda_derivada, "\n")
	cat("Ais:", Ai, "\n")
	return(interpolados)
}