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
	x <- seq(from=a, to=b, length.out=n)													# Intervalo do cálculo do erro de interpolação
	Ys_derivada_segunda <- eval(derivada_segunda)
	
	erro_interpolacao <- abs(-1/12 * ((((b-a)^3)) * max(Ys_derivada_segunda)))										# Erro de interpolação
	# ---------------------------------------------------------------------------------------------------------------------------------------
	cat("Expressao: \n")
	print(expressao)
	cat("Valor real da integração de f: ", valor_f_integrada ,"\n")
	cat("Valor aproximado pela interpolação linear: ", valor_aproximacao ,"\n")
	cat("Erro Relativo: ", erro_relativo ,"%\n")
	cat("Derivada Segunda: \n")
	print(derivada_segunda)
	cat("Erro de interpolação: ", erro_interpolacao ,"\n")	
	
	# Plotando o Grafico
	x <- seq(from=a,to=b,length.out=100)
	plot(x,f(x),type="l",col="blue")
	lines(c(a,b), c(f(a),f(b)), col="red")
	lines(c(a,a), c(0,f(a)), col="red")
	lines(c(b,b), c(0,f(b)), col="red")
	
}