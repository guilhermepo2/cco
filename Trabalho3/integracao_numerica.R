source("trapezio_simples.r")
source("trapezio_multiplo.r")
source("um_terco_simpson.r")
source("um_terco_simpson_multiplo.r")
source("tres_oitavos_simpson.r")
source("tres_oitavos_simpson_multiplo.r")

# A função Trapézio calcula a integração númerica de uma determina função (f) através do método do trapézio simples ou multiplo
# Entrada -> expressao: expressão da forma expression() não avaliada em R
#			f: Uma função que recebe um argumento númerico e retorna uma expressão avaliada nesse argumento
#			a: Intervalo inferior de Integração
#			b: Intervalo superior de Integração
#			n: nós de interpolação, em quantos subintervalos o intervalo será dividido para se procurar o valor máximo para f(alfa)
trapezio <- function(expressao, f, a, b, n, metodo="simples")
{
	switch(metodo,
		simples = {
			trapezio_simples(expressao, f, a, b, n)
		},
		multiplo = {
			trapezio_multiplo(expressao, f, a, b, n)
		},
		um_terco_simpson = {
			um_terco_simpson(expressao, f, a, b, n)
		}
	)
}

# Função de Integração e Derivação:

# Função integrate(f, lower, upper)
#		   f: uma função em R que aceita um argumento numérico e retornando a expressão avaliada nesse argumento
#		   lower: intervalo inferior de integração
#		   upper: intervalo superior de integração

# Função D(expr, name)
#	   expr: expressão não avaliada em R ( expression() )
#	   name: string contendo a variável em qual a expressão será derivada

teste_aula <- function(n, metodo="simples")
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
	
	switch(metodo,
		simples = {
			trapezio_simples(expressao, f1, a, b, n)
		},
		multiplo = {
			trapezio_multiplo(expressao, f1, a, b, n)
		},
		um_terco_simpson = {
			um_terco_simpson(expressao, f1, a, b, n)
		},
		um_terco_simpson_multiplo = {
			um_terco_simpson_multiplo(expressao, f1, a, b, n)
		},
		tres_oitavos_simpson = {
			tres_oitavos_simpson(expressao, f1, a, b, n)
		},
		tres_oitavos_simpson_multiplo = {
			tres_oitavos_simpson_multiplo(expressao, f1, a, b, n)
		}
	)
}
