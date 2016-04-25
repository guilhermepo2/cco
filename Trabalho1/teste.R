



Splinequadratica <- function(x,y,x1)

{

	#vetor hi

	h <- c()

	#vetor ai

	a <- c()

	#tamanho do vetor

	n <- length(x)

	#Ax = b -> result = b.

	result <- c()

	

	#Atribuindo hi's e ai's

	for (i in 1:(n-1))

	{

		h <- append(h, x[i+1] - x[i])

		a <- append(a, y[i]) # ai = f(xi)

	}

	

	#Esse for é para o vetor das diferenças entres os fxs: f(xi+1) - f(xi)

	for(i in 1:(2*n-3))

	{

		#Já que temos n pontos, temos n-1 diferenças, caso estejamos em uma

		#posição maior que n-1 colocamos 0

		if(i>n-1)

		{	

			result <- append(result,0)

		}

		#Se não colocamos as diferenças entres os fxs

		else

		{

			result <- append(result, y[i+1] - y[i])

		}

	}

	#Criando a matriz com 0 em suas posições.

	matriz = matrix(data=0, nrow=2*n-3, ncol=2*n-3)

	

	#Criando a linha que possui h1 como único elemento

	matriz[1][1] = h[1]

	

	#Indice j que será incrementado de 2 em 2 devido ao padrão da matriz.

	j <- 2

	for (i in 2:(n-1))

	{

		#cat("O j vale: ", j)

		matriz[i, j] = h[i]

		matriz[i, (j+1)] = h[i]*h[i]

		j<-j+2

	}




	#Na linha[n] o padrão é b2 = b1, portanto:

	matriz[n, 1] = -1

	matriz[n, 2] = 1




	#Fazer j valer 2 novamente

	j <- 2

	k <- 2

	for(i in (n+1):(2*n-3))

	{

		matriz[i, j] = -1

		matriz[i, j+1] = -2*h[k]

		matriz[i, j+2] = 1

		

		j <- j + 2

		k <- k + 1




	}

	

	vetorbc <- solve(matriz, result)




	b <- c(vetorbc[1])

	c <- c(0)




	for (i in 2:(2*n-3))

	{

		if(i%%2==0)

		{

			b <- append(b, vetorbc[i])

		}

		else

		{

			c <- append(c, vetorbc[i])

		}

	}




	

	cat("O vetor de ai's\n", a, "\n")

	#print(h)

	#print(result)

	print(matriz)

	cat("O vetor de bi's e ci's\n", vetorbc, "\n")

	cat("Vetor bi's", b, "\n")

	cat("Vetor ci's", c, "\n")







	#Criando o vetor de splines

	s <- c()

	

	for(k in 1:length(x1))

	{

		for (i in 1:(n-1))

		{

			if(x1[k] >= x[i] && x1[k] <= x[i+1])

			{

				s <- append(s, (a[i] + b[i]*(x1[k]-x[i]) + c[i] * (x1[k]-x[i])**2))

				break

			}

		}




	}

	return(s)	

}
