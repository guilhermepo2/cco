interpolacao_newton <- function(x, y, zi, n)
{
	# x -> conjunto de valores no eixo x
	# y -> conjunto de valores no eixo y
	# zi -> valores a serem interpolados
	# n -> quantidade de pontos
	
	y_resultado <- c()
	B <- y
	
	for(k in 1:(n-1))
	{
		for(i in n:(k+1))
		{
			B[i] <- (B[i] - B[i-1])/(x[i] - x[i-k])
		}
	}
	
	for(z in zi)
	{
		R <- B[n]
		for (i in (n-1):1)
		{
			R <- R * (z - x[i]) + B[i]
		}
	y_resultado <- c(y_resultado, R)
	}
	
	# Criando string para exibir o resultado e o polinomio
	resultado <- sprintf("Resultado: %f", R)
	polinomio <- "Polinomio: "
	for(i in 1:length(B)-1)
	{
		polinomio_aux <- sprintf("%f * x^%d +", B[i], i-1)
		#cat(polinomio,polinomio_aux)
		polinomio <- paste(polinomio, polinomio_aux)
	}
	polinomio_aux <- sprintf("%f * x^%d", B[i+1], i)
	polinomio <- paste(polinomio, polinomio_aux)
	
	
	# Imprimindo o Resultado e o Polinomio
	print(resultado)
	print(polinomio)
	return (y_resultado)
}

plotar <- function()
{
	x_plot <- seq(from=1, to=6, length.out = 100)
	plot(x_plot, interpolacao_newton(c(1,4,6), c(0, 1.386294, 1.791759), x_plot, 3), type="l", col="green")
	lines(x_plot, log(x_plot), col="red")
}