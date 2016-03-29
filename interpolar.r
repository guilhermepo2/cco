
source("cco-interpolacaonewton.r")
source("lagrange.r")
source("spline_linear.r")
source("spline_quadratica.r")


# Xs -> pontos dados
# Zs -> pontos a se "encontrar" dentro de um intervalo em Xs.
# tamanho -> quantos pontos Xs devem ser utilizados. O "tamanho" pode ser no maximo
#            igual ao numero de elementos em Xs. E o valor minimo depende do menor
#            intervalo encontrado que "caiba" os pontos Zs.
#
# RETURN: um array com as posicoes correspondentes aos pontos selecionados do intervalo.
#         Assim, essas posicoes podem ser facilmente utilizadas para pegar os pontos Xs
#         e tambem os pontos Ys: ' Xs[achar_intervalo(...)] '.
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
	intervalo <- seq(menor_i, maior_i)

	# caso o intervalo minimo tenha o tamanho escolhido, o processo sera finalizado
	# retornando o intervalo.
	if(tamanho == length(intervalo)){
		return(intervalo)
	} else if(tamanho < length(intervalo)) {
		print(sprintf('WARNING: O tamanho minimo do intervalo eh de %d, e foi pedido um de %d', length(intervalo), tamanho))
		return(intervalo)
	} else if(tamanho > length(Xs)){
		print(sprintf('WARNING: O tamanho maximo do intervalo eh de %d, e foi pedido um de %d', length(Xs), tamanho))
	}

	# criando dois vetores: maiores e menores. Nesses vetores estao os indices dos numeros
	# que nao estavam no intervalo minimo. No *maiores* estao os indices mais a direita
	# e no *menores*, os mais a esquerda.
	if(menor_i == 1) {
		menores = c()
	} else {
		menores = seq(1, menor_i-1)
	}
	if(maior_i == length(Xs)) {
		maiores = c()
	} else {
		maiores = seq(maior_i+1, length(Xs))
	}

	res <- intervalo
	resto <- c()
	if(length(menores) > length(maiores)) {
		qtd = length(maiores)
		resto <- menores[qtd+1 : length(menores)]
	} else if (length(maiores) > length(menores)) {
		qtd = length(menores)
		resto <- maiores[qtd+1 : length(maiores)]
	} else {
		qtd = length(menores)
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
