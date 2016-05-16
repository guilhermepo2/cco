source("cholesky.R")
source("decomposicao_lu.R")
source("decomposicao_lu_pivoteamento.R")
source("gauss_ingenuo.R")
source("gauss_pivoteamento.R")
source("gauss_seidel.R")
source("jacobi.R")

sistemas_lineares <- function(coeficientes, igualdades, metodo) {
	dimensao_coeficientes <- sqrt(length(coeficientes))
	dimensao_igualdades <- length(igualdades)

	if (dimensao_coeficientes != dimensao_igualdades) {
		cat("Numero de coeficientes nao bate com o numero de igualdades!\n")
		return()
	} else if (dimensao_coeficientes %% 1 != 0){
		# dimensao_coeficientes not integer
		cat("A matriz de coeficientes deve ser quadrada!\n")
		return()
	}

	A = matrix(coeficientes, nrow=dimensao_coeficientes, ncol=dimensao_coeficientes)
	cat("Matriz inicial dos coeficientes: \n")
	print(A)

	if ( identical(A, t(A)) ) {
		condicionamento = abs(max(eigen(A)$values)) /
		                  abs(min(eigen(A)$values))
	} else {
		transposta = t(A)
		condicionamento = sqrt(abs(max(eigen(transposta*A)$values))) /
		                  sqrt(abs(min(eigen(transposta*A)$values)))
	}
	cat("Condicionamento dos coeficientes: ", condicionamento, "\n")
	# se o condicionamento for muito alto, a matriz de coeficientes eh considerada
	# instavel; uma pequena alteracao nela, pode gerar uma grande diferenca no resultado

	cat("\n------", metodo, "------\n")
	switch (metodo,
		gauss_ingenuo = {
			Xs = gauss_ingenuo(A, igualdades)
		},
		gauss_pivoteamento = {
			Xs = gauss_pivoteamento(A, igualdades)
		},
		decomposicao_lu = {
			Xs = decomposicao_lu(A, igualdades)
		},
		decomposicao_lu_pivoteamento = {
			Xs = decomposicao_lu_pivoteamento(A, igualdades)
		},
		cholesky = {
			Xs = cholesky(A, igualdades)
		},
		gauss_seidel = {
			Xs = gauss_seidel(A, igualdades)
		},
		jacobi = {
			Xs = jacobi(A, igualdades)
		},
		{
			cat("Metodo \"", metodo, "\" invalido!\n", sep="")
			return()
		}
	)
	cat("--------\n\n")

	cat("Possíveis valores para Xi:\n")
	print(Xs)

	residuos = igualdades - (A %*% Xs)
	cat("Residuo:\n")
	print(residuos)
	
	if(sum(residuos) == 0)
	{
		cat("A exatidão da matriz é verificada\n")
	}
	else {
		cat("A exatidão da matriz não é verificada\n")	
	}
}

# Teste 1: sistemas_lineares(c(5,1,0,1,2,8,1,-1,0,-3,6,2,-1,2,1,9),c(6,10,-5,0),"jacobi")
# Teste 2: sistemas_lineares(c(10, 2, 1, 3, 8, 1, -2, -1, 5),c(57, 20, -4),"jacobi")
# Teste 3: sistemas_lineares(c(.5, 1, .4, .6, 1, -.4, .3, 1, 1),c(.2, 0, -.6),"gauss_seidel")
# Teste 4: sistemas_lineares(c(9, 6, -3, 3, 6, 20, 2, 22, -3, 2, 6, 2, 3, 22, 2, 28),c(12, 64, 4, 82),"cholesky")