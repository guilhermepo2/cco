source("integracao_numerica.r")

calcular_n <- function(tolerancia = ((10)^(-3)), metodo="trapezio", a, b, expressao)
{
	n <- 0
	derivada_segunda <- D(D(expressao,"x"),"x")
	derivada_quarta <- D(D(derivada_segunda,"x"),"x")
	x <- (b-a) / 2
	Ys_derivada_segunda <- eval(derivada_segunda)
	Ys_derivada_quarta <- eval(derivada_quarta)
	
	switch(metodo,
		trapezio = {
			n <- sqrt(abs(-1/12 * ( ( ((b-a)^3) / ((tolerancia)) ) * max(Ys_derivada_segunda) )))
		},
		um_terco = {
			n <- sqrt(sqrt(abs(-1/180 * ((b-a)^5 / (tolerancia)) * max(Ys_derivada_quarta))))
		},
		tres_oitavos = {
			n <- sqrt(sqrt((abs((-1/80 * (((b-a)^5)/(tolerancia)) * max(Ys_derivada_quarta))))))
		}
	)
	
	return(n)
}

testinho <- function(n, metodo="simples")
{
	# PARA CALCULAR A INTEGRAL
	f1 <- function(x)
	{
		return (exp(x))
	}
	# PARA CALCULAR A DERIVADA
	# expression() armazena uma expressão não avaliada
	expressao <- expression(exp(x))
	
	a <- 0
	b <- 1.0
	
	print(calcular_n( ((10)^(-3)), metodo="trapezio", a, b, expressao))
	print(calcular_n( ((10)^(-3)), metodo="um_terco", a, b, expressao))
	print(calcular_n( ((10)^(-3)), metodo="tres_oitavos", a, b, expressao))
	
	
	switch(metodo,
		simples = {
			trapezio_simples(expressao, f1, a, b, 2)
		},
		multiplo = {
			trapezio_multiplo(expressao, f1, a, b, n)
		},
		um_terco_simpson = {
			um_terco_simpson(expressao, f1, a, b, 3)
		},
		um_terco_simpson_multiplo = {
			um_terco_simpson_multiplo(expressao, f1, a, b, n)
		},
		tres_oitavos_simpson = {
			tres_oitavos_simpson(expressao, f1, a, b, 4)
		},
		tres_oitavos_simpson_multiplo = {
			tres_oitavos_simpson_multiplo(expressao, f1, a, b, n)
		}
	)
}