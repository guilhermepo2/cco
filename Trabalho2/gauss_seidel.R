# Gauss Seidel

gauss_seidel <- function(A, b, erro=(10^(-5)), iteracoes=50)
{
	x_prox <- c()
	n <- sqrt(length(A))
	x_ant <- c()

	for(i in 1:nrow(A))
	{
		x_ant[i] <- b[i]/A[i,i]
	}

	diagonalPositiva <- 0
	kMAX <- iteracoes
	e <- 1
	k <- 0

	aux <- 0
	aux2 <- 0
	soma <- 0


	D <- matrix(0,n,n)
	E <- matrix(0,n,n)
	f <- matrix(0,n,n)
	S <- 0

	while(k < kMAX && erro < e)
	{
		for(i in 1:n)
		{
			aux <- 0
			aux2 <- 0

			if(i >= 2)
			{
				for(j in 1:(i-1))
				{
					aux <- aux + A[i,j]*x_prox[j]
				}
			}

			if((i+1)<=n)
			{
				for(j in (i+1):n)
				{
					aux2 <- aux2 + A[i,j] * x_ant[j]
				}
			}

			x_prox[i] <- (b[i] - aux - aux2)/A[i,i]
		}
		e <- max(abs(x_prox-x_ant))/max(abs(x_prox))
		x_ant <- x_prox
		k <- k + 1
	}

	# Montando D
	for(i in 1:n)
	{
		D[i,i] <- A[i,i]
	}

	# Montando E
	for(i in 1:(n-1))
	{
		for(j in 2:n)
		{
			if(i!=j) E[j,i] <- A[j,i]
		}
	}

	# Montando F
	for(i in 1:(n-1))
	{
		for(j in 2:n)
		{
			if(i!=j) f[i,j] = A[i,j]
		}
	}

	diagonais <- seq(from=0,to=0, length.out=n)
	for(i in 1:n)
	{
		for(j in 1:n)
		{
			if(i != j)
			{
				soma <- soma + abs(A[i,j])
			}
		}
		if(A[i,i] > soma) diagonais[i] <- 1
		soma <- 0
	}
	if((sum(diagonais) == n) == TRUE) diagonalPositiva <- 1

	if(diagonalPositiva == 1) cat("A matriz é Estritamente Positiva (Diagonal Dominante)\n")
	else cat("A matriz não é Estritamente Positiva (Diagonal Dominante)\n")

	soma <- D+E
	soma <- solve(soma)
	s <- (-soma)%*%(f)
	S <- eigen(s)$values
	S <- abs(S)
	if(max(S) > 1) 
	{ 
		cat("O método não converge\n")
		# return() 
	}
	else cat("O método converge\n")
	cat("P(S): ", max(S), "\n")
	cat("Erro: ", e, " Iteracoes: ", k, "\n")
	return(x_prox)
}
