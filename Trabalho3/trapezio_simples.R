trapezio_simples <- function(expressao, f, a, b, n)
{
	# ---------------------------------------------------------------------------------------------------------------------------------------
	# Cálculo da Integral e do erro relativo
	
	valor_f_integrada <- integrate(f,a,b)$value												# Valor real da integral
	valor_aproximacao <- ((b-a) * ((f(a) + f(b)) / (2)))									# Aproximação pela interpolação de um polinômio de grau 1 (Método do Trapézio)
	erro_relativo <- abs((valor_f_integrada - valor_aproximacao) / valor_f_integrada)*100	# Cálculo do erro relativo
	# ---------------------------------------------------------------------------------------------------------------------------------------
	
	# ---------------------------------------------------------------------------------------------------------------------------------------
	# Cálculo do Erro de Interpolação
	
	derivada_segunda <- D( D(expressao, "x") , "x")											# Derivada segunda da expressao
	intervalo <- seq(from=a, to=b, length.out=n)											# Intervalo do cálculo do erro de interpolação
	x <- abs(avaliar_segunda_derivada(derivada_segunda, intervalo[1]))						# Cálculo de (x - xi) para o cálculo do erro de interpolação
	for(i in 2:n)
	{
		if(x < abs(avaliar_segunda_derivada(derivada_segunda, intervalo[i])))
		{
			x <- abs(avaliar_segunda_derivada(derivada_segunda, intervalo[i]))
		}
	}
	
	Et <- -1 * ((((b-a)^3) / 12) * x)														# Erro de interpolação
	# ---------------------------------------------------------------------------------------------------------------------------------------
	cat("Expressao: \n")
	print(expressao)
	cat("Valor real da integração de f: ", valor_f_integrada ,"\n")
	cat("Valor aproximado pela interpolação linear: ", valor_aproximacao ,"\n")
	cat("Erro Relativo: ", erro_relativo ,"%\n")
	cat("Derivada Segunda: \n")
	print(derivada_segunda)
	cat("Erro de interpolação: ", Et ,"\n")	
	
	# Plotando o Grafico
	x <- seq(from=a,to=b,length.out=100)
	plot(x,f(x),type="l",col="blue")
	lines(c(a,b), c(f(a),f(b)), col="red")
	lines(c(a,a), c(0,f(a)), col="red")
	lines(c(b,b), c(0,f(b)), col="red")
	
}