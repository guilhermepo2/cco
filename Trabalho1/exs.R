source("interpolar.r")

	# Exercicio 01
	# Calcular um polinômio interpolador de Quarto grau, utilizando Newton e Lagrange, considerando os pontos:
	#     x   0.1    0.3    0.4    0.6    0.7
	#     y 0.3162 0.5477 0.6325 0.7746 0.8367
	# Retornar o Polinômio, o valor da interpolação em z = 0.2
	# Mostrar os gráficos dos polinômios, destacando o valor interpolado.
	
primeiro <- function()
{
	Xs <- c(0.1, 0.3, 0.4, 0.6, 0.7)
	Ys <- c(0.3162, 0.5477, 0.6325, 0.7746, 0.8367)
	z <- 0.2
	interpolar(Xs,Ys,z,nro_pontos=5, metodo="newton")
}

	# Exercicio 02
	# Dada a função f(x) = e^x se -2 <= x <= 0
	# 				  	   xsen(5x)+1 se 0<=x<=4
	# Determine n=7 pontos, (xi, f(xi)), tais que os xi's estao igualmente espacados por h = 6/n-1
	# Utilzando splines cubicas naturais, interpolar z1 = -1.95 e z2 = 3.05 retornando as expressões gerais das splines correspondentes
	# Calcular o erro nas interpolações de z1 e z2
	# Mostrar o gráfico de f(x) e das splines
	
	
fx <- function(valor_x)
{
	if(valor_x < 0 && valor_x >= -2)
	{
		return (exp(valor_x))
	}
	else if(valor_x >=0 && valor_x <= 4)
	{
		return ((valor_x*sin(5*valor_x))+1)
	}
	else return(0)
}	

segundo <- function(nro_pontos)
{
	h = (6 / (nro_pontos-1))
	print(h)
	Xs <- seq(from=-2, to=4, by=h)
	print(Xs)
	Xs_plot <- seq(from=-2, to=4, length.out=100)
	
	Ys <- c()
	for(i in Xs)
	{
		Ys <- c(Ys, fx(i))
	}
	
	Ys_plot <- c()
	
	for(i in Xs_plot)
	{
		Ys_plot <- c(Ys_plot, fx(i))
	}
	
	z <- -1.95
	z_verdadeiro <- fx(z)
	interpolar(Xs,Ys,z,valores_verdadeiros=z_verdadeiro, metodo="spline_cubica")
	#interpolar(Xs, Ys, Xs_plot, metodo="spline_cubica")
	#plot(Xs_plot, Ys_plot, type="l", col="blue")
	#lines(Xs_plot, spline_cubica_natural(Xs,Ys,Xs_plot,length(Xs)-1), type="l", col="red")
}