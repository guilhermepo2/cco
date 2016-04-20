# Sistemas Lineares - Eliminação de Gauss com Pivoteamento
# Preservar a matriz original

gauss_pivoteamento <- function(A, b)
{
	n <- sqrt(length(A))
	B <- matrix(c(A,b), nrow=sqrt(length(A)), ncol=sqrt(length(A))+1)
	L <- matrix(0, nrow=sqrt(length(A)), ncol=sqrt(length(A)))
	for(i in 1:dim(L)[1])
	{
		L[i,i] <- 1
	}
	
	# Resolvendo a Matriz
	
	X <- c()
	
	# Eliminnação Progressiva - Vai eliminando variaveis das equações até que a matriz do sistema seja uma matriz triangular superior
	nb <- n + 1
	for(k in 1:(n-1)) # linha pivo
	{
		for(i in (k+1):n) # linhas abaixo da linha pivo
		{
			# cat("i: ", i, "\n")
			# cat("k: ", k, "\n")
			fator <- B[i,k]/B[k,k]
			# Colocando o fator na matriz L
			L[i, k] <- fator
			for(j in (k):nb) # alterar as linhas abaixo da linha pivo
			{
				B[i,j] <- B[i,j] - (fator * B[k,j])
			}
		}
	}
	print(L)
	
	# Substituicao Regressiva
	X[n] <- B[n,nb]/B[n,n]
	for(i in seq((n-1),1,-1))
	{
		s <- 0
		for(j in (i+1):n)
		{
			s <- s + B[i,j] * X[j]
		}
		X[i] <- (B[i,nb]-s)/(B[i,i])
	}
	
	# Calcular determinante
	det <- 1
	for(i in 1:dim(B)[1])
	{
		det <- det * B[i,i]
	}
	
	
	cat("Matriz B: \n")
	print(B)
	cat("Coeficientes: \n")
	print(X)
	cat("Determinante: ", det, "\n")

}