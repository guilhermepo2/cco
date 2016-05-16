# Cholesky

cholesky <- function(A, b)
{
	n <- sqrt(length(A))
	d <- c()
	X <- c()

	if (identical(A, t(A)) == FALSE || all(eigen(A)$values < 0)) {
		cat("Matriz nao eh simetrica ou nao definida positiva\n")
		return()
	}

	L <- matrix(0, n, n)

	for(i in 1:n)
	{
		for(j in 1:n)
		{
			if(i == j)
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
			else if (i > j)
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
	return(X)
}
