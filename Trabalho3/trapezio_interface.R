# Trapézio Genérico
source("avaliar_segunda_derivada.r")
source("trapezio_simples.r")
source("trapezio_multiplo.r")

trapezio <- function(expressao, f, a, b, n, metodo="simples")
{
	switch(metodo,
		simples = {
			trapezio_simples(expressao, f, a, b, n)
		},
		multiplo = {
			trapezio_multiplo(expressao, f, a, b, n)
		}
	)
}

teste_aula <- function(n, metodo="simples")
{
	# Função realizada na sala de aula
	f1 <- function(x)
	{
		return (.2 + 25*(x) - 200*(x^2) + 675*(x^3) - 900*(x^4) + 400*(x^5))
	}
	
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
		ambos = {
			trapezio_simples(expressao, f1, a, b, n)
			trapezio_multiplo(expressao, f1, a, b, n)
		}
	)
}