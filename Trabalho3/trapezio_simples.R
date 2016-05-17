# Trapézio Simples

# A função trapézio simples calcula a integração númerica de uma determina função (f) através do método do trapézio simples
# Entrada -> expressao: expressão da forma expression() não avaliada em R
#			 f: Uma função que recebe um argumento númerico e retorna uma expressão avaliada nesse argumento
#			 a: Intervalo inferior de Integração
#			 b: Intervalo superior de Integração
#			 n: nós de interpolação, em quantos subintervalos o intervalo será dividido para se procurar o valor máximo para f(alfa)
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
	# Função D(expr, name)
	#					expr: expressão não avaliada em R ( expression() )
	#					name: string contendo a variável em qual a expressão será derivada
	
	derivada_segunda <- D( D(expressao, "x") , "x")											# Derivada segunda da expressao
	intervalo <- seq(from=a, to=b, length.out=n)												# Intervalo do cálculo do erro de interpolação
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
}