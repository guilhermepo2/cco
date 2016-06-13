source("trapezio_simples.r")
source("trapezio_multiplo.r")
source("um_terco_simpson.r")
source("um_terco_simpson_multiplo.r")
source("tres_oitavos_simpson.r")
source("tres_oitavos_simpson_multiplo.r")
source("../Trabalho1/interpolar.r", chdir=TRUE)

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
		cat("N calculado pela tolerancia: ", n ,"\n")
	}
	
	
	y_interpolado <- c()
	x <- seq(from=a, to=b, length.out=100)
	
	switch(metodo,
		trapezio_simples = {
			y_interpolado <- interpolar(c(a,b), c(f(a),f(b)), x)
			trapezio_simples(expressao, f, a, b, n)
		},
		trapezio_multiplo = {
			intervalo_aproximacao <- seq(from=a, to=b, by=((b-a)/n))
			for(i in 1:(length(intervalo_aproximacao)-1))
			{
				meu_x <- c(intervalo_aproximacao[i], intervalo_aproximacao[i+1])
				meu_y <- f(meu_x)
				# PEGANDO MEUS Zs...
				t1 <- (x >= meu_x[1]) # Quero todos os zs maiores ou iguais o primeiro x do meu intervalo
				t2 <- (x < meu_x[2])  # Quero todos os zs menores que o segundo x do meu intervalo
				t3 <- c()			  # Quero os dois juntos
				for(j in 1:length(x))
				{
					t3 <- c(t3, (t1[j] && t2[j]))
				}
				if(i == (length(intervalo_aproximacao)-1)) 
				{
					t3[100] <- TRUE
				}
				meu_z <- x[t3]
				#print(t1)
				#print(t2)
				#print(t3)
				#print(meu_x)
				#print(meu_y)
				#print(meu_z)
				#cat("tamanho do meu z: ", length(meu_z), "\n")
				y_interpolado <- c(y_interpolado, interpolar( meu_x, meu_y, meu_z ))
			}
			trapezio_multiplo(expressao, f, a, b, n)
		},
		um_terco_simpson = {
			x_metodo <- seq(from=a, to=b, by=(( b - a ) / 2))
			y_interpolado <- interpolar(x_metodo, f(x_metodo), x)
			um_terco_simpson(expressao, f, a, b, n)
		},
		um_terco_simpson_multiplo = {
			um_terco_simpson_multiplo(expressao, f, a, b, n)
		},
		tres_oitavos_simpson = {
			x_metodo <- seq(from=a, to=b, by=(( b - a ) / 3))
			y_interpolado <- interpolar(x_metodo, f(x_metodo), x)
			tres_oitavos_simpson(expressao, f, a, b, n)
		},
		tres_oitavos_simpson_multiplo = {
			x_metodo <- seq(from=a,to=b,by=((b-a)/n))
			tres_oitavos_simpson_multiplo(expressao, f, a, b, n)
		}
	)
	
	# Plottando !
	#print(x)
	#print(f(x))
	#print(x_metodo)
	#print(y_metodo)
	#print("printando a funcao")
	plot(x,f(x), type="l", col="blue")
	#print("printando a interpolacao")
	lines(x, y_interpolado, col="red")
}
