# Cholesky
# Lt(L)x = b
# t(L)x = d
# Ld = b

cholesky <- function(A, b)
{
	n <- sqrt(length(A))
	d <- c()
	X <- c()

	if (identical(A, t(A)) == FALSE || !(all(eigen(A)$values > 0))) {
		cat("Matriz nao eh simetrica ou nao definida positiva\n")
		return()
	}

	L <- matrix(0, n, n)

	# ----------------------------------------------------------------------------------------------------------
	# Calculando L
	
	for(i in 1:n)
	{
		for(j in 1:n)
		{
			if(i == j)															# Diagonais
			{
				soma <- 0
				if((i-1) != 0)
				{
					for(k in 1:(i-1))
					{
						soma <- soma + (L[i,k]^2)
					}
				}
				L[i,i] <- sqrt(A[i,i] - soma)
			}
			else if (i > j)														# Triangular Inferior
			{
				soma <- 0
				if((j-1) != 0)
				{
					for(k in 1:(j-1))
					{
						soma <- soma + (L[i,k] * L[j,k])
					}
				}
				L[i,j] <- (A[i,j] - soma) / L[j,j]
			}
		}
	}
	
	cat("Matriz L: \n")
	print(L)	
	# ----------------------------------------------------------------------------------------------------------
	# Calculando Determinante
	det <- 1
	for(i in 1:n)
	{
		det <- det * L[i,i]
	}
	det <- det ^ 2
	
	cat("Determinante: ", det, "\n")
	if(det != 0) cat("A unicidade é verificada\n")
	else cat("A unicidade não é verificada\n")
	# ----------------------------------------------------------------------------------------------------------
	# Calculando o D 
	# Ld = b
	
	for(i in 1:n)
	{
		soma <- 0
		if((i-1) != 0)
		{
			for(j in 1:(i-1))
			{
				soma <- soma + L[i,j] * d[j]
			}
		}

		d[i] <- (b[i] - soma)/L[i,i]
	}
	
	cat("D: ", d ,"\n")

	# ----------------------------------------------------------------------------------------------------------
	# Descobrindo os X
	for(i in n:1)
	{
		soma <- 0
		if(i != n)
		{
			for(j in (i+1):n)
			{
				soma <- soma + L[j,i] * X[j]
			}
		}

		X[i] <- (d[i] - soma) / L[i,i]
	}
	# ----------------------------------------------------------------------------------------------------------
	return(X)
}
