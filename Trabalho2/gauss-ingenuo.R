# Sistemas Lineares - Eliminação de Gauss " Ingênuo "
# Preservar a matriz original


gauss_ingenuo <- function()
{
	n <- 2
	# Exemplo 1 - Input
	A <- matrix(0, nrow=2, ncol=2)
	A[1,1] <- 0.0003
	A[1,2] <- 3
	A[2,1] <- 1
	A[2,2] <- 1
	
	b <- matrix(0, nrow=2, ncol=1)
	b[1,1] <- 2.0001
	b[2,1] <- 1
	
	B <- matrix(0, nrow=2, ncol=3)
	B[1,1] <- A[1,1]
	B[1,2] <- A[1,2]
	B[1,3] <- b[1,1]
	B[2,1] <- A[2,1]
	B[2,2] <- A[2,2]
	B[2,3] <- b[2,1]
	print(B)
	# ------------------------------------------------------------------------------ #
	# Resolvendo a Matriz
	
	X <- c()
	
	# Eliminnação Progressiva - Vai eliminando variaveis das equações até que a matriz do sistema seja uma matriz triangular superior
	nb <- n + 1
	for(k in 1:(n-1)) # linha pivo
	{
		for(i in (k+1):n) # linhas abaixo da linha pivo
		{
			cat("i: ", i, "\n")
			cat("k: ", k, "\n")
			fator <- B[i,k]/B[k,k]
			for(j in (k):nb) # alterar as linhas abaixo da linha pivo
			{
				B[i,j] <- B[i,j] - (fator * B[k,j])
			}
		}
	}
	
	# Substituicao Regressiva
	X[n] <- B[n,nb]/B[n,n]
	
	
	print(B)
	return(B)



}