source("newton.r")
source("lagrange.r")
source("spline_linear.r")
source("spline_quadratica.r")


# Xs -> pontos dados
# Zs -> pontos a se "encontrar" dentro de um intervalo em Xs.
# tamanho -> quantos pontos Xs devem ser utilizados. O "tamanho" deve ser no maximo
#            igual ao numero de elementos em Xs. E deve ser no minimo igual ao tamanho do
#            'intervalo_minimo' encontrado que "caiba" os pontos Zs.
#            
# RETURN: as posicoes dos pontos selecionados para o intervalo.
#         Assim, essas posicoes podem ser facilmente utilizadas para pegar os pontos Xs
#         e tambem os pontos Ys: ' Xs[achar_intervalo(...)] '.

achar_intervalo <- function(Xs, Zs, tamanho)
{
	# caso em que o intervalo retornado será todos os Xs.
	if(tamanho >= length(Xs)) {
		if(tamanho > length(Xs))
			print(sprintf('WARNING: O tamanho maximo do intervalo eh de %.0f, e foi pedido um de %.0f', length(Xs), tamanho))
		return(seq(1, length(Xs)))
	}
# como o tamanho é menor que length(Xs), temos que tirar elementos de Xs

	# encontra a posicao do 'menor-maior' e do 'maior-menor' X em relacao aos Zs
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
	# intervalo mínimo são todos os Xs que estao no intervalo (menor_z, menor_z)
	intervalo_min <- seq(menor_i, maior_i)


	# caso verdade, não precisaremos pegar mais elementos de Xs para completar
	# o intervalo. Iremos no máximo diminuir o intervalo.
	if(tamanho <= length(intervalo_min)) {
		if(tamanho == 2) {
			intervalo_min <- c(intervalo_min[1], intervalo_min[length(intervalo_min)])
		} else if (tamanho > 2) {
			escolhidos <- sample(intervalo_min[2:(length(intervalo_min)-1)], tamanho-2, replace=FALSE)
			intervalo_min <- c(intervalo_min[1], escolhidos, intervalo_min[length(intervalo_min)])
			intervalo_min <- sort(intervalo_min)
		}
		return(intervalo_min)
	}
# como voce quer um intervalo maior do que o intervalo minimo, precisamos
# adicionar outros elementos de Xs nesse intervalo.

	# criando dois vetores: maiores e menores. Nesses vetores estao os indices dos numeros
	# que nao estavam no intervalo minimo. No *maiores* estao os indices mais a direita
	# e no *menores*, os mais a esquerda.
	if(menor_i == 1) {
		menores = c()
	} else {
		menores = seq(menor_i-1, 1)
		# eh uma sequencia decrescente porque os primeiros a serem inseridos no intervalo
		# devem ser os maiores.
	}
	if(maior_i == length(Xs)) {
		maiores = c()
	} else {
		maiores = seq(maior_i+1, length(Xs))
	}

	# checando a relacao entre a quantidade de maiores e a de menores, e guardando a parte
	# que sobrar de um dos dois em 'resto'.
	res <- intervalo_min
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

	# cria o vetor 'res' que possui todos os indices dos Xs de acordo com a seguinte ordem:
	#   1) elementos do intervalo minimo
	#   2) elementos dos vetores 'menores' e 'maiores' intercalados ate que um dos dois se esgote
	#   3) resto dos elementos do vetor mais longo entre os 'maiores' e 'menores'
	for (i in 1:qtd) {
		res <- c(res, menores[i], maiores[i])
	}
	res <- c(res, resto)
	res <- sort(res[1:tamanho])
	
	return(res)
}

interpolar <- function(Xs, Ys, Zs, nro_pontos= length(Xs), metodo= "newton", valores_verdadeiros= NULL)
{
	# Xs -> conjunto de valores no eixo Xs
	# Y -> conjunto de valores no eixo y
	# Zs -> valores a serem interpolados (eixo Xs)
	# n -> quantidade de pontos
	# valor_verdadeiro -> valor verdadeiro do ponto interpolado (valido somente se passar apenas um ponto para ser interpolado)
	
	indices <- achar_intervalo(Xs, Zs, nro_pontos)
	Xs <- Xs[indices]
	Ys <- Ys[indices]
	
	switch (metodo,
		newton = {
			Ys_interpolados <- interpolacao_newton(Xs, Ys, Zs)
		},
		lagrange = {
			Ys_interpolados <- lagrange(Xs, Ys, Zs)
		},
		spline_linear = {
			Ys_interpolados <- spline_linear(Xs, Ys, Zs, length(Xs))
		},
		spline_quadratica = {
			Ys_interpolados <- spline_quadratica(Xs, Ys, Zs, length(Xs))
		}
	)
	
	plot(Xs, Ys, type='l', col="blue")
	lines(Zs, Ys_interpolados,  col='red')

	print("Valores Interpolados: ")
	print(Ys_interpolados)
	
	if(length(Zs) == 1) {
		points(Zs, Ys_interpolados,  col='yellow')
	} else {
		lines(Zs, Ys_interpolados,  col='yellow')
	}
	lines(Xs, 0*Ys, col='black')
	lines(0*Xs, Ys, col='black')

	
	#TODO a professora quer o erro pra cada ponto
	if(!missing(valores_verdadeiros)) {
		erros <- abs(Ys_interpolados - valores_verdadeiros)
		erros <- erros * 100
		for(i in 1:length(erros)){
			cat(sprintf("Erro Relativo Verdadeiro para o ponto %.4f : %.4f%% \n", Zs[i], erros[i]))
		}
		erro <- mean(erros)
		string_erro <- sprintf("Media dos erros Relativos Verdadeiros: %.4f%%", erro)
		print(string_erro)
		points(Zs, valores_verdadeiros, col="green")
	}
	
	return(Ys_interpolados)
}


teste <- function() {
	Xs = seq(1, 10, length.out=100)
	interpolar(Xs, log(Xs), c(2, 3, 5), valores_verdadeiros=c(0.69314, 1.09861, 1.60943))
}




