# residuo = b - Ax
# r = 0 -> solucao exata

# Ax = b ---> L(Ux - d) = 0
#
decomposicao_lu <- function(A, b)
{
	n <- sqrt(length(A))
	L <- matrix(0, nrow=n, ncol=n)
	
	# Colocando 1 na diagonal de L
	for(i in 1:(n))
	{
		L[i,i] <- 1
	}
	
	d <- c()
	U <- matrix(A, nrow=n, ncol=n)
	# nb <- n + 1
	
	# --------------------------------------------------------------------------------------------------------------------------------
	# Decomposicao ingenua para se obter L e U
	for(k in 1:(n-1))
	{
		for(i in (k+1):n)
		{
			fator <- U[i,k]/U[k,k]
			L[i,k] <- fator
			for(j in 1:n)
			{
				U[i,j] <- U[i,j] - (fator * U[k,j])
			}
		}
	}
	# --------------------------------------------------------------------------------------------------------------------------------
	# Achando o d
	d[1] <- b[1]
	
	for(i in 2:n)
	{
		soma <- 0
		for(j in 1:(i-1))
		{
			soma <- soma + (L[i,j] * d[j])
		}
		d[i] <- b[i] - soma
	}
	
	print(U)
	print(L)
	print(d)
	
}