rm(list = ls())
library("stats")
library(cubature)
library(MomTrunc)


mu <- c(6, 10, 5)
alpha <- c(10, 30, 20)
Omega <- matrix(c(1, 0.5, 0.1, 0.5, 3, -0.5, 0.1, -0.5, 1), nrow = 3, ncol = 3,byrow = TRUE)
lambda <- -20
I <- c(1, 1, 1)
mu_S <- t(I) %*% mu
Omega_S <- t(I) %*% Omega %*% I
omega_S <- sqrt(Omega_S)
lambda_S <- lambda / sqrt(1 + t(alpha) %*% (Omega - Omega %*% I %*% solve(Omega_S) %*% t(I) %*% Omega) %*% alpha)
alpha_S <- (solve(Omega_S) %*% t(I) %*% Omega %*% alpha) / sqrt(1 + t(alpha) %*% (Omega - Omega %*% I %*% solve(Omega_S) %*% t(I) %*% Omega) %*% alpha)
c_S <- sqrt(1 + t(alpha_S) %*% Omega_S %*% alpha_S)
alpha1_S<-sqrt( t(alpha_S) %*% Omega_S %*% alpha_S)
delta_S <- (Omega_S*alpha_S)/c_S
q <- 0.95

fx <- function(x) {
  term1 <- pnorm(lambda_S+t(alpha_S)*(x-mu_S))
  term2 <- dnorm((x-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  result <- (1/pnorm(lambda_S/c_S)) * term1 * term2 
  return(result)
}


F_x <- function(x) integrate(fx, -Inf, x)$value
# slove s_q--------------------

s_q <- uniroot(function(x) F_x(x)-q, interval = c(mu_S-10,mu_S+10))$root 

# i=1 j=2-----------
B <- matrix(c(1, 0, 0, 0, 1, 0, 1, 1, 1), nrow = 3, ncol = 3, byrow=TRUE)
mu_12S = B %*% mu
Omega_12S = B %*% Omega %*% t(B)
gamma_12S <- sqrt(1+t(alpha)%*%(Omega-Omega %*% t(B)%*%solve(Omega_12S)%*% B %*%Omega) %*% alpha)
lambda_12S = lambda/gamma_12S
alpha_12S <- (rep(1/gamma_12S,3))*(solve(Omega_12S)%*%B%*%Omega%*%alpha)
alpha_12 <- matrix(c(alpha_12S[1],alpha_12S[2]), nrow = 2, ncol = 1)
alpha_S12 <- alpha_12S[3] 
omega_11 <- Omega_12S[1,1]
omega_12 <- Omega_12S[1,2]
omega_1S <- Omega_12S[1,3]
omega_21 <- Omega_12S[2,1]
omega_22 <- Omega_12S[2,2]
omega_2S <- Omega_12S[2,3]
omega_SS <- Omega_12S[3,3]
mu_1 <- mu_12S[1]
mu_2 <- mu_12S[2]
mu_S <- mu_12S[3]
Omega_12_S <- matrix(c(omega_11,omega_12,omega_21,omega_22),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_1S,omega_2S),nrow=2,ncol=1)%*%matrix(c(omega_1S,omega_2S),nrow=1,ncol=2)
Omega_12_S


#  Calculate the square root matrix
eig <- eigen(Omega_12_S)

lambd12 <- eig$values
v <- eig$vectors

lambd12_sqrt <- sqrt(lambd12)

Omega_12_S_sqrt <- v %*% diag(lambd12_sqrt) %*% solve(v)

Omega_12_S_sqrt


c_12_S <- sqrt(1+t(alpha_12)%*%Omega_12_S%*%alpha_12)
delta_12_S = Omega_12_S%*%alpha_12* rep(1/c_12_S,2)
alpha1_12_S =sqrt(t(alpha_12)%*%Omega_12_S%*%alpha_12)

mu_12_S <-function(s) {
  matrix(c(mu_1,mu_2),nrow=2, ncol =1) +rep((1/omega_SS),2)*matrix(c(omega_1S,omega_2S), nrow=2, ncol =1)*(s-mu_S)
}


Omega_S_12 <- matrix(c(omega_1S,omega_2S),nrow=1, ncol=2 )

lambda_12_S <- function(s){
  lambda_12S + (alpha_S12 + (1/omega_SS)*Omega_S_12%*%alpha_12)*(s-mu_S)
}

lambda12S =  Omega_12_S_sqrt %*% alpha_12


# i=1,j=3
B_13 <- matrix(c(1, 0, 0, 0, 0, 1, 1, 1, 1), nrow = 3, ncol = 3, byrow=TRUE)
mu_13S = B_13 %*% mu
Omega_13S = B_13 %*% Omega %*% t(B_13)

gamma_13S <- sqrt(1+t(alpha)%*%(Omega-Omega %*% t(B_13)%*%solve(Omega_13S)%*% B_13 %*%Omega) %*% alpha)
lambda_13S = lambda/gamma_13S

alpha_13S <- (rep(1/gamma_13S,3))*(solve(Omega_13S)%*%B_13%*%Omega%*%alpha)
alpha_13S
alpha_13 <- matrix(c(alpha_13S[1],alpha_13S[2]), nrow = 2, ncol = 1)
alpha_S13 <- alpha_13S[3] 
alpha_S13
omega_13 <- Omega_13S[1,2]
omega_31 <- Omega_13S[2,1]
omega_33 <- Omega_13S[2,2]
omega_3S <- Omega_13S[2,3]
mu_3 <- mu_13S[2]

Omega_13_S <- matrix(c(omega_11,omega_13,omega_31,omega_33),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_1S,omega_3S),nrow=2,ncol=1)%*%matrix(c(omega_1S,omega_3S),nrow=1,ncol=2)
c_13_S <- sqrt(1+t(alpha_13)%*%Omega_13_S%*%alpha_13)
delta_13_S = Omega_13_S%*%alpha_13* rep(1/c_13_S,2)
alpha1_13_S =sqrt(t(alpha_13)%*%Omega_13_S%*%alpha_13)
Omega_S_13 <- matrix(c(omega_1S,omega_3S),nrow=1, ncol=2 )

lambda_13_S <- function(s){
  lambda_13S + (alpha_S13 + (1/omega_SS)*Omega_S_13%*%alpha_13)*(s-mu_S)
}

mu_13_S <-function(s) {
  matrix(c(mu_1,mu_3),nrow=2, ncol =1) +rep((1/omega_SS),2)*matrix(c(omega_1S,omega_3S), nrow=2, ncol =1)*(s-mu_S)
}


#Calculate the square root matrix
eig <- eigen(Omega_13_S)


lambd13 <- eig$values
v <- eig$vectors


lambd13_sqrt <- sqrt(lambd13)


Omega_13_S_sqrt <- v %*% diag(lambd13_sqrt) %*% solve(v)

Omega_13_S_sqrt

lambda13S =  Omega_13_S_sqrt %*% alpha_13


# i=2,j=3---------

B_23 <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 1), nrow = 3, ncol = 3, byrow=TRUE)
mu_23S = B_23 %*% mu
Omega_23S = B_23 %*% Omega %*% t(B_23)
gamma_23S <- sqrt(1+t(alpha)%*%(Omega-Omega %*% t(B_23)%*%solve(Omega_23S)%*% B_23 %*%Omega) %*% alpha)
lambda_23S = lambda/gamma_23S
alpha_23S <- (rep(1/gamma_23S,3))*(solve(Omega_23S)%*%B_23%*%Omega%*%alpha)
alpha_23 <- matrix(c(alpha_23S[1],alpha_23S[2]), nrow = 2, ncol = 1)
alpha_S23 <- alpha_23S[3] 
omega_23 <- Omega_23S[1,2]
omega_32 <- Omega_23S[2,1]

Omega_23_S <- matrix(c(omega_22,omega_23,omega_32,omega_33),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_2S,omega_3S),nrow=2,ncol=1)%*%matrix(c(omega_2S,omega_3S),nrow=1,ncol=2)
c_23_S <- sqrt(1+t(alpha_23)%*%Omega_23_S%*%alpha_23)
delta_23_S = Omega_23_S%*%alpha_23* rep(1/c_23_S,2)
alpha1_23_S =sqrt(t(alpha_23)%*%Omega_23_S%*%alpha_23)

Omega_S_23 <- matrix(c(omega_2S,omega_3S),nrow=1, ncol=2 )

lambda_23_S <- function(s){
  lambda_23S + (alpha_S23 + (1/omega_SS)*Omega_S_23%*%alpha_23)*(s-mu_S)
}

mu_23_S <-function(s) {
  matrix(c(mu_2,mu_3),nrow=2, ncol =1) +rep((1/omega_SS),2)*matrix(c(omega_2S,omega_3S), nrow=2, ncol =1)*(s-mu_S)
}

#Calculate the square root matrix
eig <- eigen(Omega_23_S)


lambd23 <- eig$values
v <- eig$vectors


lambd23_sqrt <- sqrt(lambd23)


Omega_23_S_sqrt <- v %*% diag(lambd23_sqrt) %*% solve(v)

Omega_23_S_sqrt

lambda23S =  Omega_23_S_sqrt %*% alpha_23







#----------

a = c(-Inf, -Inf)
b = c(Inf, Inf)

E12_S <- function(s) {
  mu_12_S <- mu_12_S(s)
  lambda_12_S <- lambda_12_S(s)
  result <- momentsTMD(c(2,2),a,b,mu_12_S,Omega_12_S,lambda = lambda12S,tau = lambda_12_S,dist = "ESN")
  return(result)  
}

E13_S <- function(s) {
  mu_13_S <- mu_13_S(s)
  lambda_13_S <- lambda_13_S(s)
  result <- momentsTMD(c(2,2),a,b,mu_13_S,Omega_13_S,lambda = lambda13S,tau = lambda_13_S,dist = "ESN")
  return(result) 
}


E23_S <- function(s) {
  mu_23_S <- mu_23_S(s)
  lambda_23_S <- lambda_23_S(s)
  result <- momentsTMD(c(2,2),a,b,mu_23_S,Omega_23_S,lambda = lambda23S,tau = lambda_23_S,dist = "ESN")
  return(result)  
}


# Defining integral function
result_integral <- function(s) {
  E1_S_vals <- E12_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[2, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E1S <- integral_results / (1 - q)

print(E1S)



result_integral <- function(s) {
  E2_S_vals <- E12_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E2_S_vals[4, "E[k]"]  
  return(as.numeric(result)) 
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E2S <- integral_results / (1 - q)

print(E2S)



result_integral <- function(s) {
  E3_S_vals <- E13_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E3_S_vals[4, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-13)$integral

E3S <- integral_results / (1 - q)

print(E3S)


E1S
E2S
E3S

#solve EijS

result_integral <- function(s) {
  E1_S_vals <- E12_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[5, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E12S <- integral_results / (1 - q)

print(E12S)



result_integral <- function(s) {
  E3_S_vals <- E13_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E3_S_vals[5, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E13S <- integral_results / (1 - q)

print(E13S)


result_integral <- function(s) {
  E3_S_vals <- E23_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E3_S_vals[5, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E23S <- integral_results / (1 - q)

print(E23S)




result_integral <- function(s) {
  E1_S_vals <- E12_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[8, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E1_22S <- integral_results / (1 - q)

print(E1_22S)


result_integral <- function(s) {
  E1_S_vals <- E12_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[6, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E12_2S <- integral_results / (1 - q)

print(E12_2S)




result_integral <- function(s) {
  E1_S_vals <- E13_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[8, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E1_32S <- integral_results / (1 - q)

print(E1_32S)



result_integral <- function(s) {
  E1_S_vals <- E13_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[6, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E12_3S <- integral_results / (1 - q)

print(E12_3S)





result_integral <- function(s) {
  E1_S_vals <- E23_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[8, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E2_32S <- integral_results / (1 - q)

print(E2_32S)




result_integral <- function(s) {
  E1_S_vals <- E23_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[6, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E22_3S <- integral_results / (1 - q)

print(E22_3S)





result_integral <- function(s) {
  E1_S_vals <- E12_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[3, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E1_2S <- integral_results / (1 - q)

print(E1_2S)




result_integral <- function(s) {
  E1_S_vals <- E12_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[7, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E2_2S <- integral_results / (1 - q)

print(E2_2S)



result_integral <- function(s) {
  E1_S_vals <- E23_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[7, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E3_2S <- integral_results / (1 - q)

print(E3_2S)


E12_S <- function(s) {
  mu_12_S <- mu_12_S(s)
  lambda_12_S <- lambda_12_S(s)
  result <- momentsTMD(c(3,3),a,b,mu_12_S,Omega_12_S,lambda = lambda12S,tau = lambda_12_S,dist = "ESN")
  return(result)  
}

E13_S <- function(s) {
  mu_13_S <- mu_13_S(s)
  lambda_13_S <- lambda_13_S(s)
  result <- momentsTMD(c(3,3),a,b,mu_13_S,Omega_13_S,lambda = lambda13S,tau = lambda_13_S,dist = "ESN")
  return(result) 
}


result_integral <- function(s) {
  E1_S_vals <- E12_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[4, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E1_3 <- integral_results / (1 - q)

print(E1_3)


result_integral <- function(s) {
  E1_S_vals <- E12_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[13, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E2_3 <- integral_results / (1 - q)

print(E2_3)


result_integral <- function(s) {
  E1_S_vals <- E13_S(s)
  result <- (1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) * E1_S_vals[13, "E[k]"]  
  return(as.numeric(result))  
}


integral_results <- adaptIntegrate(f = result_integral, lowerLimit = s_q, upperLimit = Inf, tol = 1e-12)$integral

E3_3 <- integral_results / (1 - q)

print(E3_3)

sigma_1_1 = E1_2S - (E1S)^2
sigma_2_2 = E2_2S - (E2S)^2
sigma_3_3 = E3_2S - (E3S)^2
sigma_1_2 = E12S - E1S * E2S
sigma_1_2
sigma_2_1 = sigma_1_2
sigma_1_3 = E13S - E1S * E3S
sigma_3_1 = sigma_1_3
sigma_2_3 = E23S - E2S * E3S
sigma_3_2 = sigma_2_3

sigma_2_1_1 = E1_3 - E1S * E1_2S
sigma_2_2_2 = E2_3 - E2S * E2_2S
sigma_2_3_3 = E3_3 - E3S * E3_2S

sigma_2_1_2 = E1_22S - E1S * E2_2S
sigma_2_2_1 = E12_2S - E2S * E1_2S
sigma_2_1_3 = E1_32S - E1S * E3_2S
sigma_2_3_1 = E12_3S - E3S * E1_2S
sigma_2_2_3 = E2_32S - E2S * E3_2S
sigma_2_3_2 = E22_3S - E3S * E2_2S


#final result------------------
SIGMA_S <- matrix(c(sigma_1_1,sigma_1_2,sigma_1_3,sigma_2_1,sigma_2_2,sigma_2_3,sigma_3_1,sigma_3_2,sigma_3_3),nrow=3, ncol=3, byrow=TRUE)

SIGMA_S 

beta= 0.01
a=solve((8*beta*SIGMA_S+2*diag(3)))
a

# solve zetai
zetai <- numeric(3)

zetai <- c(E1S, E2S, E3S)



sigma_2_i_j <- matrix(c(sigma_2_1_1,sigma_2_1_2,sigma_2_1_3,sigma_2_2_1,sigma_2_2_2,sigma_2_2_3,sigma_2_3_1,sigma_2_3_2,sigma_2_3_3),nrow=3, ncol=3, byrow=TRUE)
sigma_2_i_j

# solvedelta_i

delta <- rep(0, 3)

# Calculate the delta_i value for each i value
for (i in 1:3) {
  # Calculate the sum of sigma_2_j_i values
  sigma2_sum <- sum(sigma_2_i_j[i,]) 
  
  # solve delta_i 
  delta[i] <- 4 * beta * sigma2_sum + 2 * zetai[i]
}
print(sigma2_sum)

# displaying results
cat("\n\\delta_i å€?:\n")
print(delta)



numerator <- 25
denominator <- 0
first_sum <- 0
second_sum <- 0
third_sum <- 0

# Calculate the sum of the denominators
for (k in 1:3) {
  for (l in 1:3) {
    denominator <- denominator + a[k, l]
  }
}
print(denominator)
# Calculate the first summation part
for (j in 1:3) {
  first_sum <- first_sum + a[2, j]
}
print(first_sum)
# Calculate the sum of the numerators of the first summation part
for (k in 1:3) {
  for (l in 1:3) {
    third_sum <- third_sum + a[k, l] * delta[l]
  }
}
numerator <- ((numerator - third_sum) / denominator) * first_sum

# Calculate the second summation part
for (j in 1:3) {
  second_sum <- second_sum + a[2, j] * delta[j]
}
print(second_sum)


result <- numerator + second_sum

# print
cat('The calculation result is', result, '\n')

print(result/25)



