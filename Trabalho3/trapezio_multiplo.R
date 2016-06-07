trapezio_multiplo <- function(expressao, f, a, b, n)
{
    # ---------------------------------------------------------------------------------------------------------------------------------------
    # Determinando o intervalo, o somatório, a integral e o erro relativo
	# Determinando o somatório
    h = (b-a) / n
    intervalo_somatorio = seq(from=a+h, to=b-h, by=h)
	somatorio = sum(f(intervalo_somatorio))

	# Cálculo da Integral e do Erro Relativo
	valor_f_integrada <- integrate(f,a,b)$value    # Valor real da integral
	valor_aproximacao <- (h/2) * (f(a) + 2 * somatorio + f(b))     # Aproximação pela interpolação pelo método do trapézio multiplo
	erro_relativo <- abs((valor_f_integrada - valor_aproximacao) / valor_f_integrada)*100	# Cálculo do erro relativo
	# ---------------------------------------------------------------------------------------------------------------------------------------

	# ---------------------------------------------------------------------------------------------------------------------------------------
	# Cálculo do Erro de Interpolação

	derivada_segunda <- D( D(expressao, "x") , "x")
	x = seq(from=a, to=b, length.out=n)     # esse x é apenas pra funcionar com o eval() abaixo
	Ys_derivada_segunda = eval(derivada_segunda)

	erro_interpolacao <- -1 * ( ( ((b-a)^3) / (12*(n^2)) ) * max(Ys_derivada_segunda) )
	# ---------------------------------------------------------------------------------------------------------------------------------------
	cat("Expressao: \n")
	print(expressao)

	cat("Valor real da integração de f: ", valor_f_integrada ,"\n")
	cat("Valor aproximado pela interpolação trapezio multiplo: ", valor_aproximacao ,"\n")
	cat("Erro Relativo: ", erro_relativo ,"%\n")
	cat("Derivada Segunda: \n")
	print(derivada_segunda)
	cat("Erro de interpolação: ", erro_interpolacao ,"\n")
	
	# Plotando o grafico
	x <- seq(from=a,to=b,length.out=100)
	plot(x, f(x), type="l", col="blue")
	lines(c(a, a+h),c(f(a),f(a+h)), col="red")
	lines(c(a,a), c(a,f(a)), col="red")
	lines(c(a+h,a+h), c(a,f(a+h)), col="red")
	for(i in intervalo_somatorio) 
	{
		lines(c(i,i+h),c(f(i),f(i+h)), col="red")
		lines(c(i,i),c(0,f(i)), col="red")
	}
	lines(c(b-h, b),c(f(b-h),f(b)), col="red")
	lines(c(b,b), c(0,f(b)), col="red")
}
