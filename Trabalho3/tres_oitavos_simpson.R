tres_oitavos_simpson <- function(expressao, f, a, b, n)
{
	# ---------------------------------------------------------------------------------------------------------------------------------------
	# Cálculo da Integral e do erro relativo
	
	h <- ( b - a ) / 3
	valor_f_integrada <- integrate(f,a,b)$value												# Valor real da integral
	valor_aproximacao <- ((((3/8))*h) * (f(a) + (3*f(a+h)) + (3*f(b-h)) + f(b)))			# Aproximação pelo metodo 3/8 Simpson
	erro_relativo <- abs((valor_f_integrada - valor_aproximacao) / valor_f_integrada)*100	# Cálculo do erro relativo
	# ---------------------------------------------------------------------------------------------------------------------------------------
	# ---------------------------------------------------------------------------------------------------------------------------------------
	# Cálculo do Erro de Interpolação
	derivada_quarta <- D( D ( D ( D ( expressao, "x" ), "x" ), "x" ), "x" )
	x <- seq(from=a, to=b, length.out=n)
	Ys_derivada_quarta <- eval(derivada_quarta)

	erro_interpolacao <- abs((-1/80 * ((b-a)^5) * max(Ys_derivada_quarta)))
	# ---------------------------------------------------------------------------------------------------------------------------------------
	
	cat("Expressao: \n")
	print(expressao)
	cat("Valor real da integração de f: ", valor_f_integrada ,"\n")
	cat("Valor aproximado pelo metodo de 3/8 simpson: ", valor_aproximacao ,"\n")
	cat("Erro Relativo: ", erro_relativo ,"%\n")
	cat("Derivada Quarta: \n")
	print(derivada_quarta)
	cat("Erro de interpolação: ", erro_interpolacao ,"\n")	
}