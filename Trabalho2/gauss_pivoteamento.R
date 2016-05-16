# Sistemas Lineares - Eliminação de Gauss com Pivoteamento
# Preservar a matriz original

# Exemplos de entrada:
# c(0.0003, 1, 3, 1)
# c(2.0001, 1)

# c(1, 0.99, 0.99, 0.98)


gauss_pivoteamento <- function(A, b)
{
	n <- sqrt(length(A))
	B <- matrix(c(A,b), nrow=n, ncol=n+1)
	# L <- matrix(0, nrow=sqrt(length(A)), ncol=sqrt(length(A)))


	# Fazendo a diagonal da matriz L ser 1
	# for(i in 1:dim(L)[1])
	# {
	#	L[i,i] <- 1
	# }

	# Resolvendo a Matriz

	X <- c()

	# Eliminnação Progressiva - Vai eliminando variaveis das equações até que a matriz do sistema seja uma matriz triangular superior

	nb <- n + 1
	for(k in 1:(n-1)) # linha pivo
	{

		# ------------------------------------------------------------------------------------------------------------------------------------------
		# maximo -> maior elemento da coluna
		# indice -> indice do maior elemento da coluna
		maximo <- abs(B[k,k])
		indice <- k

		# Pegando o maior elemento da coluna para ser utilizado como base para o calculo do fator
		# Tambem faz a troca das colunas caso necessario
		for(i in (k+1):n)
		{
			if(abs(B[i,k]) > maximo)
			{
				maximo <- abs(B[i,k])
				indice <- i
			}
		}

		# Se o indice for diferente do indice k, significa que ha uma coluna com elemento de modulo maior
		# entao deve ser trocado
		if(indice != k)
		{
					aux <- B[k,]
					B[k,] <- B[i,]
					B[i,] <- aux
		}
		# ------------------------------------------------------------------------------------------------------------------------------------------

		# Resolucao da Matriz
		for(i in (k+1):n) # linhas abaixo da linha pivo
		{
			# cat("i: ", i, "\n")
			# cat("k: ", k, "\n")
			fator <- B[i,k]/B[k,k]
			# Colocando o fator na matriz L
			# L[i, k] <- fator

			# alterando as linhas abaixo da linha pivo
			for(j in (k):nb)
			{
				B[i,j] <- B[i,j] - (fator * B[k,j])
			}
		}
	}
	# cat("Matriz L: \n")
	# print(L)


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
	return(X)
}
