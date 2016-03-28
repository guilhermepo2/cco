source("cco-interpolacaonewton.r")
source("lagrange.r")
source("spline_linear.r")
source("spline_quadratica.r")


achar_intervalo <- function(Xs, Zs, tamanho)
{
	menor_z <- min(Zs)
	maior_z <- max(Zs)
	
	maior_i <- length(Xs)
	menor_i <- 1
	for(i in 1:length(Xs))
	{
		if(Xs[i] <= menor_z)
		{
			menor_i <- i
		}
		
		if(Xs[i] >= maior_z && Xs[i] < Xs[maior_i])
		{
			maior_i <- i
		}	
	}
	intervalo <- Xs[menor_i:maior_i]
	
	#print(sprintf('menor_i= %d , maior_i= %d', menor_i, maior_i))

	# caso o intervalo minimo tenha o tamanho escolhido, o processo é finalizado
	# retornando o intervalo.
	if(tamanho == length(intervalo)){
		return(intervalo)
	} else if(tamanho < length(intervalo)) {
		print(sprintf('ERROR: O tamanho minimo do intervalo eh de %d, e foi pedido um de %d', length(intervalo), tamanho))
		return(NULL)
	} else if(tamanho > length(Xs)){
		print(sprintf('WARNING: O tamanho maximo do intervalo eh de %d, e foi pedido um de %d', length(Xs), tamanho))
	}

	# criando dois vetores: maiores e menores. Nesses vetores estão os números
	# que não estavam no intervalo mínimo. No *maiores* estão os nº mais à direita
	# e no *menores*, os nº mais à esquerda.
	if(menor_i == 1) {
		menores = c()
	} else {
		menores = Xs[1:menor_i-1]
	}
	if(maior_i == length(Xs)) {
		maiores = c()
	} else {
		maiores = Xs[maior_i+1:length(Xs)]
	}

	res <- intervalo
	resto <- c()
	if(length(menores) > length(maiores)) {
		qtd = length(maiores)
		resto <- menores[qtd+1 : length(menores)]
	} else if (length(maiores) > length(menores)) {
		qtd = length(menores)
		resto <- maiores[qtd+1 : length(maiores)]
	}

	if(qtd != 0) {
		for (i in 1:qtd) {
			res <- c(res, menores[i], maiores[i])
		}
	}
	res <- c(res, resto)
	res <- sort(res[1:tamanho])

	return(res)
}

interpolar <- function(Xs,y,Zs,n, metodo="t",valor_verdadeiro = NULL)
{
	# Xs -> conjunto de valores no eixo Xs
	# Y -> conjunto de valores no eixo y
	# Zs -> valores a serem interpolados (eixo Xs)
	# n -> quantidade de pontos
	# valor_verdadeiro -> valor verdadeiro do ponto interpolado (valido somente se passar apenas um ponto para ser interpolado)
}
