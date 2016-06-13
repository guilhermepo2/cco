source("integracao_numerica.r")

# Exemplo de como criar um script de aplicacao

# Função de Integração e Derivação:
# Função integrate(f, lower, upper)
#		   f: uma função em R que aceita um argumento numérico e retornando a expressão avaliada nesse argumento
#		   lower: intervalo inferior de integração
#		   upper: intervalo superior de integração

# Função D(expr, name)
#	   expr: expressão não avaliada em R ( expression() )
#	   name: string contendo a variável em qual a expressão será derivada

teste_aula <- function(n=NULL, tolerancia=NULL, metodo=NULL)
{
	# PARA CALCULAR A INTEGRAL
	# Função realizada na sala de aula
	f1 <- function(x)
	{
		return (.2 + 25*(x) - 200*(x^2) + 675*(x^3) - 900*(x^4) + 400*(x^5))
	}
	
	# PARA CALCULAR A DERIVADA
	# expression() armazena uma expressão não avaliada
	expressao <- expression( (.2 + 25*(x) - 200*(x^2) + 675*(x^3) - 900*(x^4) + 400*(x^5)))
	
	a <- 0
	b <- 0.8
	
	integracao_numerica(expressao, f1, a, b, n=n, metodo=metodo, tolerancia=tolerancia)
}