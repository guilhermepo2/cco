# Sistemas Lineares - Eliminação de Gauss " Ingênuo "
# Exemplo de entrada:
# gauss_ingenuo(c(0.0003, 1, 3, 1),c(2.0001, 1))

gauss_ingenuo <- function(A, b)
{
	n <- sqrt(length(A))
	B <- matrix(c(A,b), nrow=n, ncol=n+1)
	cat("Matriz B inicial: \n")
	print(B)
	X <- c() 										                                  # Coeficientes desejados
	# --------------------------------------------------------------------------------------------------------------------------------------------------- #
	# Eliminnação Progressiva - Vai eliminando variáveis das equações até que a matriz do sistema seja uma matriz triangular superior
	nb <- n + 1
	for(k in 1:(n-1)) 																  # K armazena o indice da linha pivô
	{
		for(i in (k+1):n) 															  # I armazena o indice das linhas abaixo da linha pivô que serão alteradas
		{
			fator <- B[i,k]/B[k,k]													  # Fator que será utilizado para a Eliminação Progressiva
			for(j in (k):nb) 														  # Alterar as linhas abaixo da linha pivo
			{
				B[i,j] <- B[i,j] - (fator * B[k,j])									  # Bij = Bij - (fator * Bkj)
			}
		}
	}

	# --------------------------------------------------------------------------------------------------------------------------------------------------- #
	# Substituicao Regressiva - Obtendo os coeficientes desejados
	X[n] <- B[n,nb]/B[n,n]															  # O Último X é calculado manualmente, pois é necessário para os anteriores
	for(i in seq((n-1),1,-1))
	{
		s <- 0
		for(j in (i+1):n)
		{
			s <- s + B[i,j] * X[j]													  # Soma = sum(Bij * Xj) para i de n-1 ate 1 e j de i+1 até n
		}
		X[i] <- (B[i,nb]-s)/(B[i,i])												  # Xi = Bi,nb - soma / Bi,i
	}

	# --------------------------------------------------------------------------------------------------------------------------------------------------- #
	# Calcular determinante da matriz resultante
	det <- 1
	for(i in 1:dim(B)[1])
	{
		det <- det * B[i,i]
	}

	# --------------------------------------------------------------------------------------------------------------------------------------------------- #
	# Impressão do Resultado

	cat("Matriz B final: \n")
	print(B)
	cat("Coeficientes: \n")
	print(X)
	cat("Determinante: ", det, "\n")
	return(X)
}
