# residuo = b - Ax
# r = 0 -> solucao exata

# Ax = b ---> L(Ux - d) = 0

decomposicao_lu_pivoteamento <- function(A, b)
{
	n <- sqrt(length(A))
	L <- matrix(0, nrow=n, ncol=n)
	trocas <- 0

	# Colocando 1 na diagonal de L
	for(i in 1:(n))
	{
		L[i,i] <- 1
	}

	U <- matrix(A, nrow=n, ncol=n)
	X <- matrix(0, nrow=n, ncol=1)

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
	P <- L # P eh a matriz identidade que vai identificar quais linhas foram trocadas

	# Obtendo L e U por pivoteamento

	for(k in 1:(n-1))																# achando o maximo da coluna para fazer a troca
	{
		maximo <- abs(U[k,k])
		indice <- k

		for(i in (k+1):n)
		{
			if(abs(U[i,k] > maximo))
			{
				maximo <- abs(abs(U[i,k]))
				indice <- i
			}
		}

	if(indice != k) 																# monitorando as trocas na matriz P
	{
		aux <- P[k,]
		P[k,] <- P[i,]
		P[i,] <- aux
	}

	aux <- b[indice]																# fazendo a troca em b
	b[indice] <- b[k]
	b[k] <- aux
	trocas <- trocas + 1

	if(k != 1)																		#
	{
		for(j in 1:(k-1))
		{
			aux <- L[k,j]
			L[k,j] <- L[indice,j]
			L[indice,j] <- aux
		}
	}

	for(i in (k+1):n)
	{
		L[i,k] <- U[i,k]/U[k,k]
		for(m in 1:n)
		{
			U[i,m] <- U[i,m] - L[i,k]*U[k,m]
		}
	}
	}

	print(U)
	print(L)
	# --------------------------------------------------------------------------------------------------------------------------------
	# Achando o detemirnante
	det <- 1
	for(i in 1:n)
	{
		det <- det * U[i,i]
	}

	det <- det / ((-1) ^ trocas)


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
