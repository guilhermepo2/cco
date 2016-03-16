

lagrange <- function(X, Y, n, x)
{
	R <- 0
	Ls <- c()
	for(i in 1:n) {
		C <- 1
		D <- 1
		for (j in 1:n) {
			if(i != j){
				C <- C * (x - X[j])
				D <- D * (X[i] - X[j])
			}
		}
		L <- (C/D)
		Ls <- c(Ls, L)
		R <- R + Y[i] * L
	}
	
	polinomio <- "Polinomio: "
	for(i in 1:(n-1)) {
		polinomio = paste(polinomio, sprintf("%f * %f +", Ls[i], Y[i]))
	}
	polinomio = paste(polinomio, sprintf("%f * %f", Ls[i+1], Y[i+1]))
	print(polinomio)
	
	
	sprintf("Resultado: %f", R)
}



plotar <- function()
{
	x_plot <- seq(from=1, to=6, length.out = 100)
	plot(x_plot, lagrange(c(1,4,6), c(0, 1.386294, 1.791759), 2, 3), type="l", col="green")
	lines(x_plot, log(x_plot), col="red")
}