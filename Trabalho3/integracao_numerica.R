source("trapezio_simples.r")
source("trapezio_multiplo.r")
source("um_terco_simpson.r")
source("um_terco_simpson_multiplo.r")
source("tres_oitavos_simpson.r")
source("tres_oitavos_simpson_multiplo.r")

# A integracao_numerica calcula a integração númerica de uma determina função (f) através de um desses métodos:
#																												- Trapézio Simples
#																												- Trapézio Múltiplo
#																												- 1/3 Simpson
#																												- 1/3 Simpson Múltiplo
#																												- 3/8 Simpson
#																												- 3/8 Simpson Múltiplo
# Entrada -> expressao: expressão da forma expression() não avaliada em R
#			f: Uma função que recebe um argumento númerico e retorna uma expressão avaliada nesse argumento
#			a: Intervalo inferior de Integração
#			b: Intervalo superior de Integração
#			n: nós de interpolação, em quantos subintervalos o intervalo será dividido para se procurar o valor máximo para f(alfa) ou em quantos subintervalos a integral será dividida, no caso de métodos múltiplos
#			metodo: string do método que será utilizado para calcular a integral
#					(trapezio_simples, trapezio_multiplo, um_terco_simpson, um_terco_simpson_multiplo, tres_oitavos_simpson, tres_oitavos_simpson_multiplo)
#			tolerancia: Tolerância para o erro, caso deseje se calcular o n a partir de uma certa tolerância para o erro
integracao_numerica <- function(expressao, f, a, b, n=NULL, metodo=NULL, tolerancia=NULL)
{
	if( !( xor(is.null(n), is.null(tolerancia)) ) )
	{
		cat("Informe a tolerancia ou o erro\n")
		return()
	}
	
	if(!(is.null(tolerancia)))
	{
		x <- (b-a) / 2
		switch(metodo,
			trapezio_multiplo = {
				derivada_segunda <- D(D(expressao,"x"),"x")
				Ys_derivada_segunda <- eval(derivada_segunda)
				n <- sqrt(abs(-1/12 * ( ( ((b-a)^3) / ((tolerancia)) ) * max(Ys_derivada_segunda) )))
			},
			um_terco_simpson_multiplo = {
				derivada_quarta <- D(D(D(D(expressao,"x"),"x"),"x"),"x")
				Ys_derivada_quarta <- eval(derivada_quarta)
				n <- sqrt(sqrt(abs(-1/180 * ((b-a)^5 / (tolerancia)) * max(Ys_derivada_quarta))))
				
			},
			tres_oitavos_simpson_multiplo = {
				derivada_quarta <- D(D(D(D(expressao,"x"),"x"),"x"),"x")
				Ys_derivada_quarta <- eval(derivada_quarta)
				n <- sqrt(sqrt((abs((-1/80 * (((b-a)^5)/(tolerancia)) * max(Ys_derivada_quarta))))))
			},
			{
				cat("Nem faz sentido passar a tolerancia pra esse metodo!\n")
				return()
			}
		)

		n <- ceiling(n)
	}
	
	cat("N calculado pela tolerancia: ", n ,"\n")
	
	switch(metodo,
		trapezio_simples = {
			trapezio_simples(expressao, f, a, b, n)
		},
		trapezio_multiplo = {
			trapezio_multiplo(expressao, f, a, b, n)
		},
		um_terco_simpson = {
			um_terco_simpson(expressao, f, a, b, n)
		},
		um_terco_simpson_multiplo = {
			um_terco_simpson_multiplo(expressao, f, a, b, n)
		},
		tres_oitavos_simpson = {
			tres_oitavos_simpson(expressao, f, a, b, n)
		},
		tres_oitavos_simpson_multiplo = {
			tres_oitavos_simpson_multiplo(expressao, f, a, b, n)
		}
	)
}
