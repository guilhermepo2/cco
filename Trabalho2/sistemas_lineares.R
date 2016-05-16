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
		return
	} else if (dimensao_coeficientes %% 1 != 0){
		# dimensao_coeficientes not integer
		cat("A matriz de coeficientes deve ser quadrada!\n")
		return
	}

	A = matrix(coeficientes, nrow=dimensao_coeficientes, ncol=dimensao_coeficientes)

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
			Xs = decomposicao_lu(A, igualdades)
		},
		cholesky = {
			Xs = cholesky(A, igualdades)
		},
		gauss_seidel = {
			Xs = gauss_seidel(A, igualdades)
		},
		jacobi = {
			Xs = jacobi(A, igualdades)
		}
	)
	cat("Possíveis valores para Xi:\n", Xs, "\n")

	residuo = igualdades - (A %*% Xs)
	cat("Residuo: ", residuo, "\n")

}
