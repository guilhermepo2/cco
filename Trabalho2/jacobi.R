# Jacobi

jacobi <- function(A, b)
{
	x_prox <- c()
	n <- sqrt(length(A))
	A <- matrix(A,n,n)										# transformando o vetor em matriz quadrada
	soma <- 0
	
	x_ant <- c()
	for(i in 1:nrow(A))
	{
		x_ant[i] <- b[i]/A[i,i]
	}
	
	diagonalPositiva <- 0
	
	kMAX <- 50												# Numero maximo de iteracoes
	erro <- (10^(-5))										# Erro a ser atingido
	e <- 1													# Erro inicial
	k <- 0													# Iteracao Atual
	aux <- 0
	
	D <- matrix(0, n, n)
	E <- matrix(0, n, n)
	f <- matrix(0, n, n)
	J <- 0
	
	while(erro < e && k < kMAX)
	{
		for(i in 1:n)
		{
			aux <- 0
			for(j in 1:n)
			{
				if(i!=j) aux <- aux + A[i,j] * x_ant[j]
			}
			x_prox[i] <- 1/A[i,i] * (b[i]-aux)
		}
		
		cat("Iteracao: ", k, "\n")
		cat("x_ant: ", x_ant, "\n" )
		cat("x_prox: ", x_prox, "\n")
		
		k <- k + 1
		e = max(abs(x_prox - x_ant)) / max(abs(x_prox))
		cat("Erro: ", e, "\n")
		
		x_ant <- x_prox
		cat("\n")
	}
	
	# Montando D
	for(i in 1:n)
	{
		D[i,i] = A[i,i]
	}
	# Montando E
	for(i in 1:(n-1))
	{
		for(j in 2:n)
		{
			if(i!=j) E[j,i] = A[j,i]
		}
	}
	# Montando F
	for(i in 1:(n-1))
	{
		for(j in 2:n)
		{
			if(i != j) f[i,j] = A[i,j]
		}
	}
	
	# Verificando se a matriz e estritamente positiva
	for(i in 1:n)
	{
		for(j in 1:n)
		{
			soma <- soma + abs(A[i,j])
		}
		if(A[i,i] > soma) diagonalPositiva <- 1
		soma <- 0
	}
	
	if(diagonalPositiva == 1) cat("Estritamente Positiva\n")
	else cat("Nao e estritamente positiva\n")
	
	d <- solve(D)
	j <- (-d)%*%(E+f)
	J <- eigen(j)$values
	J <- abs(J)
	if(max(J)>1) cat("O metodo nao Converge\n")
	else cat("O metodo Converge.\n")
	cat("P(J): ", max(J), "\n")
	cat("Resultados: ", x_prox, "\nErro: ", e, " Iteracoes: ", k)
}