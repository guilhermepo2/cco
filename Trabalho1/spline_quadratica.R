# As splines quadráticas são equações do tipo
#     Si(X) = Ai + Bi * (X - Xi) + Ci (X - Xi)^2
# 
# É necessário criar 1 spline, igual acima, para cada ponto menos
# o último, assim, teremos
#     (N_pontos-1) splines.
# Como os Ais são iguais aos Yis, temos 2 incógnitas (Bi e Ci) para
# cada spline. Por isso, ao todo teremos 
#     2 * (N-1) incógnitas.
# No entanto, para garantir que a segunda derivada no ponto X1 seja nula,
# C1 deve ser igual a 0. Assim, temos menos uma incógnita.
#     2 * (N-1) -1 incógnitas.
spline_quadratica <- function(X,Y,z,n)
{

	tamanho_incognitas <- 2 * n - 3
	
	# Calculando os indices ai das splines
# Condição 1 - Si(X) = Yi, para garantir continuidade
	Ai <- Y
	
# Condição 2 - Primeiras 3 linhas da Matriz
#     Si(Xi+1) = Si+1 (Xi+1), para garantir que splines adjacentes 
#       se conectem nos pontos internos.
#   Após matematicar, temos as seguintes condições:
#     Bi + Ci * (Xi+1 - Xi) = (Yi+1 - Yi) / (Xi+1 - Xi)

    # ddv -> diferencas divididas -> (Yi+1 - Yi) / (Xi+1 - Xi)
	ddv <- seq(0,0, length.out = tamanho_incognitas)
	Hi <- c() # (Xi+1 - Xi)

	for(i in 1:(n-1))
	{
		Hi[i] <- (X[i+1] - X[i])
		ddv[i] <- (Y[i+1] - Y[i]) / Hi[i]
	}
	
	matriz_diferenca_divididas <- matrix(ddv, nrow=tamanho_incognitas, 1)
	matriz_coeficientes <- matrix(0, nrow=tamanho_incognitas, ncol = tamanho_incognitas)
	
	# Calcular a matriz (tamanho x tamanho)
	
	# Primeira Linha da Matriz
	matriz_coeficientes[1,1] <- 1 
	j <- 2
	
	for(i in 2:(n-1))
	{
		matriz_coeficientes[i,j] <- 1
		matriz_coeficientes[i,j+1] <- Hi[i]
		j <- j+2
	}
	
# Condição 3 - Últimas Linhas da Matriz
	# bi+1 = bi + cihi
	# b1 = b0 + h0c0 => b1 = b0
	matriz_coeficientes[i+1,1] <- 1
	matriz_coeficientes[i+1,2] <- -1
	
	auxiliar_h <- 2
	k <- 4
	for(j in ((i+2):tamanho_incognitas))
	{
		matriz_coeficientes[j,k-2] <- 1
		matriz_coeficientes[j,k-1] <- 2 * Hi[auxiliar_h]
		matriz_coeficientes[j,k] <- -1
		auxiliar_h <- auxiliar_h + 1
		k <- k + 2
	}
	coeficientes <- solve(matriz_coeficientes, matriz_diferenca_divididas)
	
	# pegando os coeficientes
	
	bi <- c(coeficientes[1])
	ci <- c(0)
	for(i in 2:length(coeficientes))
	{
		if(i %% 2==0)
		{
			bi <- c(bi, coeficientes[i])
		}
		else {
			ci <- c(ci, coeficientes[i])
		}
	}

	# Interpolando os pontos desejados
	resultados <- c()
	for(i in z)
	{
		# i é o valor de z que é o valor a ser interpolado
		for(j in 1:(n-1))
		{
			if(i >= X[j] && i <= X[j+1])
			{
				res <- Ai[j] + bi[j] * (i - X[j]) + ci[j] * ((i - X[j])^2)
				resultados <- c(resultados, res)
			}
		}
	}
	
	# print (resultados)
	return(resultados)
	
}