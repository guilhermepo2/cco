tres_oitavos_simpson_multiplo <- function(expressao, f, a, b, n)
{
	# ---------------------------------------------------------------------------------------------------------------------------------------
	# Cálculo da Integral e do erro relativo
	
	h <- ( b - a ) / n
	valor_f_integrada <- integrate(f,a,b)$value												# Valor real da integral
	
	somatorio_multiplo_3 <- 0
	somatorio_nao_multiplo_3 <- 0
	intervalo_somatorio <- seq(from=a+h, to=b-h, by=h)
	print(intervalo_somatorio)
	for(i in 1:(n-1))
	{
		if(i %% 3 == 0)
		{
			somatorio_multiplo_3 <- somatorio_multiplo_3 + f(intervalo_somatorio[i])
		} else {
			somatorio_nao_multiplo_3 <- somatorio_nao_multiplo_3 + f(intervalo_somatorio[i])
		}
	}
	
	print(somatorio_multiplo_3)
	print(somatorio_nao_multiplo_3)
	
	valor_aproximacao <- ((3/8)*h) * (f(a) + (3 * somatorio_nao_multiplo_3) + (2 * somatorio_multiplo_3) + f(b))		# Aproximação pelo metodo 3/8 Simpson multiplo
	erro_relativo <- abs((valor_f_integrada - valor_aproximacao) / valor_f_integrada)*100	# Cálculo do erro relativo
	# ---------------------------------------------------------------------------------------------------------------------------------------
	# ---------------------------------------------------------------------------------------------------------------------------------------
	# Cálculo do Erro de Interpolação
	derivada_quarta <- D( D ( D ( D ( expressao, "x" ), "x" ), "x" ), "x" )
	x <- seq(from=a, to=b, length.out=n)
	Ys_derivada_quarta <- eval(derivada_quarta)

	erro_interpolacao <- abs((-1/80 * (((b-a)^5)/(n^4)) * max(Ys_derivada_quarta)))
	# ---------------------------------------------------------------------------------------------------------------------------------------
	
	cat("Expressao: \n")
	print(expressao)
	cat("Valor real da integração de f: ", valor_f_integrada ,"\n")
	cat("Valor aproximado pelo metodo de 3/8 simpson multiplo: ", valor_aproximacao ,"\n")
	cat("Erro Relativo: ", erro_relativo ,"%\n")
	cat("Derivada Quarta: \n")
	print(derivada_quarta)
	cat("Erro de interpolação: ", erro_interpolacao ,"\n")	
}