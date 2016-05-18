trapezio_multiplo <- function(expressao, f, a, b, n)
{
	# ---------------------------------------------------------------------------------------------------------------------------------------
	# Determinando o intervalo, o somatório, a integral e o erro relativo
	# Determinando o somatório
	
	h <- ((b-a)/n)																			# Passo para o cálculo do somatório
	intervalo_somatorio <- seq(from=a,to=b,by=h)											# Intervalo para o cálculo do somatório		
	somatorio <- f(intervalo_somatorio[1])
	for(i in 2:n)
	{
		somatorio <- somatorio + f(intervalo_somatorio[i])
	}
	
	# Cálculo da Integral e do Erro Relativo
	valor_f_integrada <- integrate(f,a,b)$value												# Valor real da integral
	valor_aproximacao <- (h/2) * (f(a) + 2 * somatorio + f(b))								# Aproximação pela interpolação pelo método do trapézio multiplo
	erro_relativo <- abs((valor_f_integrada - valor_aproximacao) / valor_f_integrada)*100	# Cálculo do erro relativo
	# ---------------------------------------------------------------------------------------------------------------------------------------
	
	# ---------------------------------------------------------------------------------------------------------------------------------------
	# Cálculo do Erro de Interpolação
	
	derivada_segunda <- D( D(expressao, "x") , "x")											# Derivada segunda da expressao
	intervalo <- seq(from=a, to=b, length.out=n)											# Intervalo do cálculo da Integral para achar max(f(alfa))
	
	x <- abs(avaliar_segunda_derivada(derivada_segunda, intervalo[1]))						# Cálculo de (x - xi) para o cálculo do erro de interpolação
	for(i in 2:n)
	{
		if(x < abs(avaliar_segunda_derivada(derivada_segunda, intervalo[i])))
		{
			x <- abs(avaliar_segunda_derivada(derivada_segunda, intervalo[i]))
		}
	}
	
	Et <- -1 * ((((b-a)^3) / 12*(n^2)) * x)													# Erro de interpolação
	# ---------------------------------------------------------------------------------------------------------------------------------------
	cat("Expressao: \n")
	print(expressao)
	
	cat("Valor real da integração de f: ", valor_f_integrada ,"\n")
	cat("Valor aproximado pela interpolação trapezio multiplo: ", valor_aproximacao ,"\n")
	cat("Erro Relativo: ", erro_relativo ,"%\n")
	cat("Derivada Segunda: \n")
	print(derivada_segunda)
	cat("Erro de interpolação: ", Et ,"\n")	
}