um_terco_simpson_multiplo <- function(expressao, f, a, b, n)
{
	# ---------------------------------------------------------------------------------------------------------------------------------------
	# Cálculo da Integral e do erro relativo
	
	h <- ( b - a ) / n
	valor_f_integrada <- integrate(f,a,b)$value												# Valor real da integral
	intervalo_somatorio <- seq(from=a+h, to=b-h, by=h)
	
	somatorio_par <- 0
	somatorio_impar <- 0
	for(i in 1:(n-1))
	{
		if(i %% 2 == 0)
		{
			somatorio_par <- somatorio_par + f(intervalo_somatorio[i])
		} else {
			somatorio_impar <- somatorio_impar + f(intervalo_somatorio[i])
		}
	}
	
	valor_aproximacao <- ((h / 3) * (f(a) + ((4 * somatorio_impar) + (2 * somatorio_par)) + f(b)))							# Aproximação pelo metodo 1/3 Simpson multiplo
	erro_relativo <- abs((valor_f_integrada - valor_aproximacao) / valor_f_integrada)*100	# Cálculo do erro relativo
	# ---------------------------------------------------------------------------------------------------------------------------------------
	
	# ---------------------------------------------------------------------------------------------------------------------------------------
	# Cálculo do Erro de Interpolação
	derivada_quarta <- D( D ( D ( D ( expressao, "x" ), "x" ), "x" ), "x" )
	x <- seq(from=a, to=b, length.out=n)
	Ys_derivada_quarta <- eval(derivada_quarta)

	erro_interpolacao <- -1/180 * ((b-a)*(h^4)) * max(Ys_derivada_quarta) 
	# ---------------------------------------------------------------------------------------------------------------------------------------
	
	cat("Expressao: \n")
	print(expressao)
	cat("Valor real da integração de f: ", valor_f_integrada ,"\n")
	cat("Valor aproximado pelo metodo de 1/3 simpson multiplo: ", valor_aproximacao ,"\n")
	cat("Erro Relativo: ", erro_relativo ,"%\n")
	cat("Derivada Quarta: \n")
	print(derivada_quarta)
	cat("Erro de interpolação: ", erro_interpolacao ,"\n")	
}