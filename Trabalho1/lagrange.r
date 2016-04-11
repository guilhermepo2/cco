

lagrange <- function(Xs, Ys, Zs)
{
	n <- length(Xs)
	
	cat("        P(x) = L(i) * F(Xi) + ...\n")
	interpolados <- c()
	for(z in Zs) {	
		R <- 0
		Ls <- c()
		for(i in 1:n) {
			C <- 1
			D <- 1
			for (j in 1:n) {
				if(i != j){
					C <- C * (z - Xs[j])
					D <- D * (Xs[i] - Xs[j])
				}
			}
			L <- (C/D)
			Ls <- c(Ls, L)
			R <- R + Ys[i] * L
		}

		cat("Polinomio para o ponto", z, ": ")
		for(i in 1:(n-1)) {
			cat(Ls[i], '*', Ys[i], '+ ')
		}
		cat(Ls[i+1], '*', Ys[i+1], '\n')
		
		sprintf("Resultado: %f", R)
		interpolados <- c(interpolados, R)
	}
	print(interpolados)
	return(interpolados)
}



plotar <- function()
{
	x_plot <- seq(from=1, to=6, length.out = 100)
	plot(x_plot, lagrange(c(1,4,6), c(0, 1.386294, 1.791759), c(2, 3)), type="l", col="green")
	lines(x_plot, log(x_plot), col="red")
}