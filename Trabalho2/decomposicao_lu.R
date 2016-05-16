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

	U <- matrix(A, nrow=n, ncol=n)
	X <- matrix(0, nrow=1, ncol=n)

	d <- c()
	# --------------------------------------------------------------------------------------------------------------------------------
		# Calculo de Condicionamento
		# ||A||2 = λmax se A = At ou gmax se A != At
		# λmax = |max(eigen(A)$values)|
		# gmax = sqrt(|max(eigen(t(A)*A)$values))|)

		transposta <- t(U)
		cond <- 0
		matriz_comparacao <- transposta == U
		RES <- TRUE
		for(i in matriz_comparacao) { RES <- (RES && i) }
		if(RES)
		{
			cond <- abs(max(eigen(U)$values))/abs(min(eigen(U)$values))
		}
		else
		{
			cond <- sqrt(abs(max(eigen(transposta*U)$values)))/sqrt(abs(min(eigen(transposta*U)$values)))
		}

	cat("Condicionamento: ", cond, "\n")
	# --------------------------------------------------------------------------------------------------------------------------------

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
	# Achando o detemirnante
	det <- 1
	for(i in 1:n)
	{
		det <- det * U[i,i]
	}

	cat("Determinante: ", det, "\n")
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
	# --------------------------------------------------------------------------------------------------------------------------------
	# Substituição Regressiva para achar a solução a partir de Ux = d

	X[n] <- d[n]/U[n,n]															      # O Último X é calculado manualmente, pois é necessário para os anteriores
	for(i in seq((n-1),1,-1))
	{
		s <- 0
		for(j in (i+1):n)
		{
			s <- s + U[i,j] * X[j]
		}
		X[i] <- (d[i]-s)/(U[i,i])
	}
	--------------------------------------------------------------------------------------------------------------------------------
	return(X)
}
