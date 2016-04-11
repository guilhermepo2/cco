source("newton.r")
source("lagrange.r")
source("spline_linear.r")
source("spline_quadratica.r")
source("spline_cubica.r")
source("intervalo.r")

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
		},
		spline_cubica = {
			Ys_interpolados <- spline_cubica_natural(Xs,Ys,Zs,length(Xs)-1)
		}
	)
	
	plot(Xs, Ys, type='l', col="blue")
	lines(Zs, Ys_interpolados,  col='red')

	cat("Valores Interpolados: \n")
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
		for(i in 1:length(erros)) {
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




