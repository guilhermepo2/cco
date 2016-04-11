# Interpolação pelo Método de Newton
# 
# O algoritmo deve:
# 	Poder receber vários pontos para serem interpolados (ok)
# 	Imprimir a forma geral do polinômio (ok)
# 	Plotar o gráfico (ok)
# 	Calcular o erro (relativo, verdadeiro, percentual) (ok)
# 	Ao invés de passar os pontos, passar a função


interpolacao_newton <- function(Xs, Ys, Zs)
{
	# x -> conjunto de valores no eixo x
	# y -> conjunto de valores no eixo y
	# zi -> valores a serem interpolados
	# n -> quantidade de pontos
	# valor_verdadeiro <- valor verdadeiro de um ponto que deseja se calcular o erro
	cat("\n    NEWTON\n")

	n <- length(Xs)
	B <- Ys
	for(k in 1:(n-1))
	{
		for(i in n:(k+1))
		{
			B[i] <- (B[i] - B[i-1])/(Xs[i] - Xs[i-k])
		}
	}
	
	# Avaliando o valor do polinômio nos pontos Z
	y_resultado <- c()
	for(z in Zs)
	{
		R <- B[n]
		for (i in (n-1):1)
		{
			R <- R * (z - Xs[i]) + B[i]
		}
		y_resultado <- c(y_resultado, R)
	}
	
	polinomio <- "Polinomio: "
	polinomio_aux <- ""
	for(i in 1:(length(B)-1))
	{
		# com esse if, somente vou adicionar os coeficientes que não forem muito
		# próximos de zero à string 'polinomio'. Isso facilitará a visualização 
		# do polinômio
		if(abs(B[i]) > 0.000001) {
			polinomio_aux <- sprintf("%f * x^%d +", B[i], i-1)
			polinomio <- paste(polinomio, polinomio_aux)
		}
	}
	polinomio_aux <- sprintf("%f * x^%d", B[i+1], i)
	polinomio <- paste(polinomio, polinomio_aux)
	
	
	# Imprimindo o Resultado e o Polinomio
	print(polinomio)
	print(y_resultado)
	return (y_resultado)
}

funcao <- function(Xs)
{
	return (1 / (1 + (25*Xs*Xs)))
}

exercicio_geral <- function(grau, intervalo_menor, intervalo_maior, nro_pontos = 100 )
{
	if(missing(nro_pontos))
	{
		nro_pontos <- grau
	}
	
	x_func <- seq(from=intervalo_menor, to=intervalo_maior, length.out=(grau+1))
	y_func <- funcao(x_func)
	x_plot <- seq(from=intervalo_menor, to=intervalo_maior, length.out = nro_pontos)
	
	y_interpolacao <- interpolacao_newton(x_func, y_func, x_plot, length(x_func))
	y_verdadeiro <- funcao(x_plot)
	erro_verdadeiro <- abs((y_verdadeiro - y_interpolacao)/ y_verdadeiro)
	string_erro <- sprintf("Media dos Erros: %f%%", mean(erro_verdadeiro)*100)
	print(string_erro)
	
	plot(x_plot, y_interpolacao, type="l", col="green")
	lines(x_plot, y_verdadeiro, col="red")
	eixo_x <- seq(from=intervalo_menor, to=intervalo_maior, length.out=5)
	lines(eixo_x,0 * eixo_x ,col="black")
}

exercicio <- function()
{
	x_func <- seq(from=-1, to=1, length.out=11)
	y_func <- (1 / (1 + (25*x_func*x_func)))
	x_plot <- seq(from=-1, to=1, length.out=100)
	
	y_interpolacao <- interpolacao_newton(x_func, y_func, x_plot)
	
	plot(x_plot, y_interpolacao, type="l", col="green")
	lines(x_plot, (1 / (1 + (25*x_plot*x_plot))), col="red")
}

plotar <- function()
{
	x_plot <- seq(from=1, to=6, length.out = 100)
	plot(x_plot, interpolacao_newton(c(1,4,6), c(0, 1.386294, 1.791759), x_plot), type="l", col="green" )
	lines(x_plot, log(x_plot), col="red")
}
