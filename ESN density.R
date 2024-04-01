
rm(list = ls())

mu <- 0
Omega <- 1
alpha <- 3

# Function to calculate the density function of X
fx <- function(x, lambda) {
  c <- sqrt(1 + (alpha^2) * Omega)
  term1 <- pnorm(lambda + alpha * (x - mu))
  term2 <- dnorm((x - mu) / sqrt(Omega)) * (1 / sqrt(Omega))
  result <- (1 / pnorm(lambda / c)) * term1 * term2 
  return(result)
}

# Values of lambda
lambda_values <- c(-1, -3, -5, 0, 1, 3, 5)

# Plotting the density functions on one plot
x_range <- seq(-2, 4, by = 0.1)
y_values <- sapply(lambda_values, function(lambda) fx(x_range, lambda))
matplot(x_range, y_values, type = 'l', col = 1:length(lambda_values), lwd = 2,
        xlab = 'x', ylab = 'Density')
legend('topright', legend = as.expression(sapply(lambda_values, function(l) bquote(lambda == .(l)))), col = 1:length(lambda_values), lwd = 2, cex = 0.8)