rm(list = ls())
library("stats")
library(cubature)

mu <- c(6, 10, 5)
alpha <- c(10, 30, 20)
Omega <- matrix(c(1, 0.5, 0.1, 0.5, 3, -0.5, 0.1, -0.5, 1), nrow = 3, ncol = 3,byrow = TRUE)
Omega
Omega
lambda <- 10
I <- c(1, 1, 1)
mu_S <- t(I) %*% mu
Omega_S <- t(I) %*% Omega %*% I
omega_S <- sqrt(Omega_S)
lambda_S <- lambda / sqrt(1 + t(alpha) %*% (Omega - Omega %*% I %*% solve(Omega_S) %*% t(I) %*% Omega) %*% alpha)
alpha_S <- (solve(Omega_S) %*% t(I) %*% Omega %*% alpha) / sqrt(1 + t(alpha) %*% (Omega - Omega %*% I %*% solve(Omega_S) %*% t(I) %*% Omega) %*% alpha)
c_S <- sqrt(1 + t(alpha_S) %*% Omega_S %*% alpha_S)
alpha1_S<-sqrt( t(alpha_S) %*% Omega_S %*% alpha_S)
delta_S <- (Omega_S*alpha_S)/c_S
q <- 0.98


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

B_11 <- matrix(c(1, 0, 0, 1, 0, 0, 1, 1, 1), nrow = 3, ncol = 3, byrow=TRUE)
Omega_11S = B_11 %*% Omega %*% t(B_11)
Omega_11S
Omega_12S

B_22 <- matrix(c(0, 1, 0, 0, 1, 0, 1, 1, 1), nrow = 3, ncol = 3, byrow=TRUE)
Omega_22S = B_22 %*% Omega %*% t(B_22)
Omega_22S
Omega_12S


B_13 <- matrix(c(1, 0, 0, 0, 0, 1, 1, 1, 1), nrow = 3, ncol = 3, byrow=TRUE)
mu_13S = B_13 %*% mu
mu_13S
Omega_13S = B_13 %*% Omega %*% t(B_13)
Omega_13S
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





B_21 <- matrix(c(0, 1, 0, 1, 0, 0, 1, 1, 1), nrow = 3, ncol = 3, byrow=TRUE)
mu_21S = B_21 %*% mu
mu_21S
Omega_21S = B_21 %*% Omega %*% t(B_21)
Omega_21S
gamma_21S <- sqrt(1+t(alpha)%*%(Omega-Omega %*% t(B_21)%*%solve(Omega_21S)%*% B_21 %*%Omega) %*% alpha)
lambda_21S = lambda/gamma_21S
alpha_21S <- (rep(1/gamma_21S,3))*(solve(Omega_21S)%*%B_21%*%Omega%*%alpha)
alpha_21 <- matrix(c(alpha_21S[1],alpha_21S[2]), nrow = 2, ncol = 1)
alpha_S21 <- alpha_21S[3] 

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

B_31 <- matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 1), nrow = 3, ncol = 3, byrow=TRUE)
mu_31S = B_31 %*% mu
Omega_31S = B_31 %*% Omega %*% t(B_31)
gamma_31S <- sqrt(1+t(alpha)%*%(Omega-Omega %*% t(B_31)%*%solve(Omega_31S)%*% B_31 %*%Omega) %*% alpha)
lambda_31S = lambda/gamma_31S
alpha_31S <- (rep(1/gamma_31S,3))*(solve(Omega_31S)%*%B_31%*%Omega%*%alpha)
alpha_31 <- matrix(c(alpha_31S[1],alpha_31S[2]), nrow = 2, ncol = 1)
alpha_S31 <- alpha_31S[3]

B_32 <- matrix(c(0, 0, 1, 0, 1, 0, 1, 1, 1), nrow = 3, ncol = 3, byrow=TRUE)
mu_32S = B_32 %*% mu
Omega_32S = B_32 %*% Omega %*% t(B_32)
gamma_32S <- sqrt(1+t(alpha)%*%(Omega-Omega %*% t(B_32)%*%solve(Omega_32S)%*% B_32 %*%Omega) %*% alpha)
lambda_32S = lambda/gamma_32S
alpha_32S <- (rep(1/gamma_32S,3))*(solve(Omega_32S)%*%B_32%*%Omega%*%alpha)
alpha_32 <- matrix(c(alpha_32S[1],alpha_32S[2]), nrow = 2, ncol = 1)
alpha_S32 <- alpha_32S[3] 



Omega_12_S <- matrix(c(omega_11,omega_12,omega_21,omega_22),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_1S,omega_2S),nrow=2,ncol=1)%*%matrix(c(omega_1S,omega_2S),nrow=1,ncol=2)
c_12_S <- sqrt(1+t(alpha_12)%*%Omega_12_S%*%alpha_12)
delta_12_S = Omega_12_S%*%alpha_12* rep(1/c_12_S,2)
alpha1_12_S =sqrt(t(alpha_12)%*%Omega_12_S%*%alpha_12)

Omega_13_S <- matrix(c(omega_11,omega_13,omega_31,omega_33),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_1S,omega_3S),nrow=2,ncol=1)%*%matrix(c(omega_1S,omega_3S),nrow=1,ncol=2)
c_13_S <- sqrt(1+t(alpha_13)%*%Omega_13_S%*%alpha_13)
delta_13_S = Omega_13_S%*%alpha_13* rep(1/c_13_S,2)
alpha1_13_S =sqrt(t(alpha_13)%*%Omega_13_S%*%alpha_13)

Omega_21_S <- matrix(c(omega_22,omega_21,omega_12,omega_11),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_2S,omega_1S),nrow=2,ncol=1)%*%matrix(c(omega_2S,omega_1S),nrow=1,ncol=2)
c_21_S <- sqrt(1+t(alpha_21)%*%Omega_21_S%*%alpha_21)
delta_21_S = Omega_21_S%*%alpha_21* rep(1/c_21_S,2)
alpha1_21_S =sqrt(t(alpha_21)%*%Omega_21_S%*%alpha_21)

Omega_23_S <- matrix(c(omega_22,omega_23,omega_32,omega_33),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_2S,omega_3S),nrow=2,ncol=1)%*%matrix(c(omega_2S,omega_3S),nrow=1,ncol=2)
c_23_S <- sqrt(1+t(alpha_23)%*%Omega_23_S%*%alpha_23)
delta_23_S = Omega_23_S%*%alpha_23* rep(1/c_23_S,2)
alpha1_23_S =sqrt(t(alpha_23)%*%Omega_23_S%*%alpha_23)

Omega_31_S <- matrix(c(omega_33,omega_31,omega_13, omega_11),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_3S,omega_1S),nrow=2,ncol=1)%*%matrix(c(omega_3S,omega_1S),nrow=1,ncol=2)
c_31_S <- sqrt(1+t(alpha_31)%*%Omega_31_S%*%alpha_31)
delta_31_S = Omega_31_S%*%alpha_31* rep(1/c_31_S,2)
alpha1_31_S =sqrt(t(alpha_31)%*%Omega_31_S%*%alpha_31)

Omega_32_S <- matrix(c(omega_33,omega_32,omega_23,omega_22),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_3S,omega_2S),nrow=2,ncol=1)%*%matrix(c(omega_3S,omega_2S),nrow=1,ncol=2)
c_32_S <- sqrt(1+t(alpha_32)%*%Omega_32_S%*%alpha_32)
delta_32_S = Omega_32_S%*%alpha_32* rep(1/c_32_S,2)
alpha1_32_S =sqrt(t(alpha_32)%*%Omega_32_S%*%alpha_32)


mu_12_S <-function(s) {
  matrix(c(mu_1,mu_2),nrow=2, ncol =1) +rep((1/omega_SS),2)*matrix(c(omega_1S,omega_2S), nrow=2, ncol =1)*(s-mu_S)
}

Omega_S_12 <- matrix(c(omega_1S,omega_2S),nrow=1, ncol=2 )

lambda_12_S <- function(s){
  lambda_12S + (alpha_S12 + (1/omega_SS)*Omega_S_12%*%alpha_12)*(s-mu_S)
}


Omega_S_13 <- matrix(c(omega_1S,omega_3S),nrow=1, ncol=2 )

lambda_13_S <- function(s){
  lambda_13S + (alpha_S13 + (1/omega_SS)*Omega_S_13%*%alpha_13)*(s-mu_S)
}

Omega_S_21 <- matrix(c(omega_2S,omega_1S),nrow=1, ncol=2 )

lambda_21_S <- function(s){
  lambda_21S + (alpha_S21 + (1/omega_SS)*Omega_S_21%*%alpha_21)*(s-mu_S)
}


Omega_S_23 <- matrix(c(omega_2S,omega_3S),nrow=1, ncol=2 )

lambda_23_S <- function(s){
  lambda_23S + (alpha_S23 + (1/omega_SS)*Omega_S_23%*%alpha_23)*(s-mu_S)
}

Omega_S_31 <- matrix(c(omega_3S,omega_1S),nrow=1, ncol=2 )

lambda_31_S <- function(s){
  lambda_31S + (alpha_S31 + (1/omega_SS)*Omega_S_31%*%alpha_31)*(s-mu_S)
}


Omega_S_32 <- matrix(c(omega_3S,omega_2S),nrow=1, ncol=2 )

lambda_32_S <- function(s){
  lambda_32S + (alpha_S32 + (1/omega_SS)*Omega_S_32%*%alpha_32)*(s-mu_S)
}


B_1 <- matrix(c(1,0,0,1,1,1), nrow=2, ncol =3, byrow=TRUE)
B_2 <- matrix(c(0,1,0,1,1,1), nrow=2, ncol =3, byrow=TRUE)
B_3 <- matrix(c(0,0,1,1,1,1), nrow=2, ncol =3, byrow=TRUE)
mu_1S <- B_1 %*% mu
mu_2S <- B_2 %*% mu
mu_3S <- B_3 %*% mu
mu_1S
mu_2S
mu_3S


Omega_1S <- B_1 %*% Omega %*% t(B_1)
Omega_1S
Omega_2S <- B_2 %*% Omega %*% t(B_2)
Omega_3S <- B_3 %*% Omega %*% t(B_3)
gamma_1S <- sqrt(1+t(alpha)%*%(Omega-Omega%*%t(B_1)%*%solve(Omega_1S)%*%B_1%*%Omega )%*%alpha )
gamma_2S <- sqrt(1+t(alpha)%*%(Omega-Omega%*%t(B_2)%*%solve(Omega_2S)%*%B_2%*%Omega )%*%alpha )
gamma_3S <- sqrt(1+t(alpha)%*%(Omega-Omega%*%t(B_3)%*%solve(Omega_3S)%*%B_3%*%Omega )%*%alpha )
lambda_1S <- lambda/gamma_1S
lambda_2S <- lambda/gamma_2S
lambda_3S <- lambda/gamma_3S
alpha_1S <- (solve(Omega_1S)%*%B_1%*%Omega%*%alpha)*rep(1/gamma_1S,2)
alpha_2S <- (solve(Omega_2S)%*%B_2%*%Omega%*%alpha)*rep(1/gamma_2S,2)
alpha_3S <- (solve(Omega_3S)%*%B_3%*%Omega%*%alpha)*rep(1/gamma_3S,2)

omega_11_S <- Omega_1S[1,1]-((Omega_1S[1,2])^2)/Omega_1S[2,2]
omega_22_S <- Omega_2S[1,1]-((Omega_2S[1,2])^2)/Omega_2S[2,2]
omega_33_S <- Omega_3S[1,1]-((Omega_3S[1,2])^2)/Omega_3S[2,2]

c_1S = sqrt(1+((alpha_1S[1])^2)*omega_11_S)
c_2S = sqrt(1+((alpha_2S[1])^2)*omega_22_S)
c_3S = sqrt(1+((alpha_3S[1])^2)*omega_33_S)

alpha1_1_S =sqrt(((alpha_1S[1])^2)*omega_11_S)
alpha1_2_S =sqrt(((alpha_2S[1])^2)*omega_22_S)
alpha1_3_S =sqrt(((alpha_3S[1])^2)*omega_33_S)

lambda_1_S <- function(s) {
  lambda_1S + ((alpha_1S[2])+solve(Omega_1S[2,2])*Omega_1S[1,2]*alpha_1S[1])*(s-mu_1S[2])
}

lambda_2_S <- function(s) {
  lambda_2S + ((alpha_2S[2])+solve(Omega_2S[2,2])*Omega_2S[1,2]*alpha_2S[1])*(s-mu_2S[2])
}

lambda_3_S <- function(s) {
  lambda_3S + ((alpha_3S[2])+solve(Omega_3S[2,2])*Omega_3S[1,2]*alpha_3S[1])*(s-mu_3S[2])
}


#-----------------
fx <- function(x) {
  term1 <- pnorm(lambda_S+t(alpha_S)*(x-mu_S))
  term2 <- dnorm((x-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  result <- (1/pnorm(lambda_S/c_S)) * term1 * term2 
  return(result)
}


F_x <- function(x) integrate(fx, -Inf, x)$valu





# æ±‚E[S|S>S_q]--------------------

q_quantile <- uniroot(function(x) F_x(x)-q, interval = c(mu_S-20,mu_S+20))$root 


print(q_quantile) 


conditional_expectation_function <- function(x){
  x*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(x-mu_S))*dnorm((x-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
}  #Define the function representing the conditional expectation


M1 <- integrate(conditional_expectation_function, lower = q_quantile, upper = Inf)$value#Integrate the function from q_quantile to Inf

#Divide by the survival function at q_quantile to get the conditional expectation
M1 <- M1 / (1-q)

# print the result
print(M1)




conditional_expectation_function_square <- function(x){
  x^2*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(x-mu_S))*dnorm((x-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
}


M2 <- integrate(conditional_expectation_function_square, lower = q_quantile, upper= Inf)$value

M2 <- M2 / (1-q)

print(M2)



conditional_expectation_function_square <- function(x){
  x^3*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(x-mu_S))*dnorm((x-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
}


M3 <- integrate(conditional_expectation_function_square, lower = q_quantile, upper= Inf)$value

M3 <- M3 / (1-q)

print(M3)

# Var(S|S>s_q)-------------

Var <- M2 - (M1)^2

print(Var)


#i =1, j=2---------------
W1_12S_integral <- function(s) {
  lambda_12_S <- lambda_12_S(s)
  W1_12S <- (dnorm(lambda_12_S/c_12_S) / pnorm((lambda_12_S) / c_12_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W1_12S) 
}

W1_12S <- adaptIntegrate(f = W1_12S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W1_12S <- W1_12S / (1-q)
print(W1_12S)


W2_12S_integral <- function(s) {
  lambda_12_S <- lambda_12_S(s)
  W2_12S <- ((lambda_12_S)^2)* (dnorm((lambda_12_S/c_12_S)) / pnorm((lambda_12_S) / c_12_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W2_12S) 
}

W2_12S <- adaptIntegrate(f = W2_12S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W2_12S <- W2_12S / (1-q)
print(W2_12S)

W3_12S_integral <- function(s) {
  lambda_12_S <- lambda_12_S(s)
  W3_12S <- (lambda_12_S)*s* (dnorm(lambda_12_S/c_12_S) / pnorm((lambda_12_S) / c_12_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W3_12S) 
}

W3_12S <- adaptIntegrate(f = W3_12S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W3_12S <- W3_12S / (1-q)
print(W3_12S)



W4_12S_integral <- function(s) {
  lambda_12_S <- lambda_12_S(s)
  W4_12S <- (lambda_12_S)* (dnorm(lambda_12_S/c_12_S) / pnorm((lambda_12_S) / c_12_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W4_12S) 
}

W4_12S <- adaptIntegrate(f = W4_12S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W4_12S <- W4_12S / (1-q)
print(W4_12S)


W5_12S_integral <- function(s) {
  lambda_12_S <- lambda_12_S(s)
  W5_12S <- (s^2)* (dnorm(lambda_12_S/c_12_S) / pnorm((lambda_12_S) / c_12_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W5_12S) 
}

W5_12S <- adaptIntegrate(f = W5_12S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W5_12S <- W5_12S / (1-q)
print(W5_12S)


W6_12S_integral <- function(s) {
  lambda_12_S <- lambda_12_S(s)
  W6_12S <- (s)* (dnorm(lambda_12_S/c_12_S) / pnorm((lambda_12_S) / c_12_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W6_12S)
}

W6_12S <- adaptIntegrate(f = W6_12S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W6_12S <- W6_12S / (1-q)
print(W6_12S)



#i =1, j=3---------------
W1_13S_integral <- function(s) {
  lambda_13_S <- lambda_13_S(s)
  W1_13S <- (dnorm(lambda_13_S/c_13_S) / pnorm((lambda_13_S) / c_13_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W1_13S) 
}

W1_13S <- adaptIntegrate(f = W1_13S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W1_13S <- W1_13S / (1-q)
print(W1_13S)


W2_13S_integral <- function(s) {
  lambda_13_S <- lambda_13_S(s)
  W2_13S <- ((lambda_13_S)^2)* (dnorm(lambda_13_S/c_13_S) / pnorm((lambda_13_S) / c_13_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W2_13S) 
}

W2_13S <- adaptIntegrate(f = W2_13S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W2_13S <- W2_13S / (1-q)
print(W2_13S)

W3_13S_integral <- function(s) {
  lambda_13_S <- lambda_13_S(s)
  W3_13S <- (lambda_13_S)*s* (dnorm(lambda_13_S/c_13_S) / pnorm((lambda_13_S) / c_13_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W3_13S) 
}

W3_13S <- adaptIntegrate(f = W3_13S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W3_13S <- W3_13S / (1-q)
print(W3_13S)



W4_13S_integral <- function(s) {
  lambda_13_S <- lambda_13_S(s)
  W4_13S <- (lambda_13_S)* (dnorm(lambda_13_S/c_13_S) / pnorm((lambda_13_S) / c_13_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W4_13S) 
}

W4_13S <- adaptIntegrate(f = W4_13S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W4_13S <- W4_13S / (1-q)
print(W4_13S)


W5_13S_integral <- function(s) {
  lambda_13_S <- lambda_13_S(s)
  W5_13S <- (s^2)* (dnorm(lambda_13_S/c_13_S) / pnorm((lambda_13_S) / c_13_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W5_13S) 
}

W5_13S <- adaptIntegrate(f = W5_13S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W5_13S <- W5_13S / (1-q)
print(W5_13S)


W6_13S_integral <- function(s) {
  lambda_13_S <- lambda_13_S(s)
  W6_13S <- (s)* (dnorm(lambda_13_S/c_13_S) / pnorm((lambda_13_S) / c_13_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W6_13S) 
}

W6_13S <- adaptIntegrate(f = W6_13S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W6_13S <- W6_13S / (1-q)
print(W6_13S)

#i =2, j=1---------------
W1_21S_integral <- function(s) {
  lambda_21_S <- lambda_21_S(s)
  W1_21S <- (dnorm(lambda_21_S/c_21_S) / pnorm((lambda_21_S) / c_21_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W1_21S) 
}

W1_21S <- adaptIntegrate(f = W1_21S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W1_21S <- W1_21S / (1-q)
print(W1_21S)


W2_21S_integral <- function(s) {
  lambda_21_S <- lambda_21_S(s)
  W2_21S <- ((lambda_21_S)^2)* (dnorm(lambda_21_S/c_21_S) / pnorm((lambda_21_S) / c_21_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W2_21S) 
}

W2_21S <- adaptIntegrate(f = W2_21S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W2_21S <- W2_21S / (1-q)
print(W2_21S)

W3_21S_integral <- function(s) {
  lambda_21_S <- lambda_21_S(s)
  W3_21S <- (lambda_21_S)*s* (dnorm(lambda_21_S/c_21_S) / pnorm((lambda_21_S) / c_21_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W3_21S) 
}

W3_21S <- adaptIntegrate(f = W3_21S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W3_21S <- W3_21S / (1-q)
print(W3_21S)



W4_21S_integral <- function(s) {
  lambda_21_S <- lambda_21_S(s)
  W4_21S <- (lambda_21_S)* (dnorm(lambda_21_S/c_21_S) / pnorm((lambda_21_S) / c_21_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W4_21S) 
}

W4_21S <- adaptIntegrate(f = W4_21S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W4_21S <- W4_21S / (1-q)
print(W4_21S)


W5_21S_integral <- function(s) {
  lambda_21_S <- lambda_21_S(s)
  W5_21S <- (s^2)* (dnorm(lambda_21_S/c_21_S) / pnorm((lambda_21_S) / c_21_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W5_21S) 
}

W5_21S <- adaptIntegrate(f = W5_21S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W5_21S <- W5_21S / (1-q)
print(W5_21S)


W6_21S_integral <- function(s) {
  lambda_21_S <- lambda_21_S(s)
  W6_21S <- (s)* (dnorm(lambda_21_S/c_21_S) / pnorm((lambda_21_S) / c_21_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W6_21S) 
}

W6_21S <- adaptIntegrate(f = W6_21S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W6_21S <- W6_21S / (1-q)
print(W6_21S)

#i =2, j=3---------------
W1_23S_integral <- function(s) {
  lambda_23_S <- lambda_23_S(s)
  W1_23S <- (dnorm(lambda_23_S/c_23_S) / pnorm((lambda_23_S) / c_23_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W1_23S) 
}

W1_23S <- adaptIntegrate(f = W1_23S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W1_23S <- W1_23S / (1-q)
print(W1_23S)


W2_23S_integral <- function(s) {
  lambda_23_S <- lambda_23_S(s)
  W2_23S <- ((lambda_23_S)^2)* (dnorm(lambda_23_S/c_23_S) / pnorm((lambda_23_S) / c_23_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W2_23S) 
}

W2_23S <- adaptIntegrate(f = W2_23S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W2_23S <- W2_23S / (1-q)
print(W2_23S)

W3_23S_integral <- function(s) {
  lambda_23_S <- lambda_23_S(s)
  W3_23S <- (lambda_23_S)*s* (dnorm(lambda_23_S/c_23_S) / pnorm((lambda_23_S) / c_23_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W3_23S) 
}

W3_23S <- adaptIntegrate(f = W3_23S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W3_23S <- W3_23S / (1-q)
print(W3_23S)



W4_23S_integral <- function(s) {
  lambda_23_S <- lambda_23_S(s)
  W4_23S <- (lambda_23_S)* (dnorm(lambda_23_S/c_23_S) / pnorm((lambda_23_S) / c_23_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W4_23S) 
}

W4_23S <- adaptIntegrate(f = W4_23S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W4_23S <- W4_23S / (1-q)
print(W4_23S)


W5_23S_integral <- function(s) {
  lambda_23_S <- lambda_23_S(s)
  W5_23S <- (s^2)* (dnorm(lambda_23_S/c_23_S) / pnorm((lambda_23_S) / c_23_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W5_23S) 
}

W5_23S <- adaptIntegrate(f = W5_23S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W5_23S <- W5_23S / (1-q)
print(W5_23S)


W6_23S_integral <- function(s) {
  lambda_23_S <- lambda_23_S(s)
  W6_23S <- (s)* (dnorm(lambda_23_S/c_23_S) / pnorm((lambda_23_S) / c_23_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W6_23S) 
}

W6_23S <- adaptIntegrate(f = W6_23S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W6_23S <- W6_23S / (1-q)
print(W6_23S)


#i =3, j=1---------------
W1_31S_integral <- function(s) {
  lambda_31_S <- lambda_31_S(s)
  W1_31S <- (dnorm(lambda_31_S/c_31_S) / pnorm((lambda_31_S) / c_31_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W1_31S)
}

W1_31S <- adaptIntegrate(f = W1_31S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W1_31S <- W1_31S / (1-q)
print(W1_31S)


W2_31S_integral <- function(s) {
  lambda_31_S <- lambda_31_S(s)
  W2_31S <- ((lambda_31_S)^2)* (dnorm(lambda_31_S/c_31_S) / pnorm((lambda_31_S) / c_31_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W2_31S) 
}

W2_31S <- adaptIntegrate(f = W2_31S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W2_31S <- W2_31S / (1-q)
print(W2_31S)

W3_31S_integral <- function(s) {
  lambda_31_S <- lambda_31_S(s)
  W3_31S <- (lambda_31_S)*s* (dnorm(lambda_31_S/c_31_S) / pnorm((lambda_31_S) / c_31_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W3_31S) 
}

W3_31S <- adaptIntegrate(f = W3_31S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W3_31S <- W3_31S / (1-q)
print(W3_31S)



W4_31S_integral <- function(s) {
  lambda_31_S <- lambda_31_S(s)
  W4_31S <- (lambda_31_S)* (dnorm(lambda_31_S/c_31_S) / pnorm((lambda_31_S) / c_31_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W4_31S) 
}

W4_31S <- adaptIntegrate(f = W4_31S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W4_31S <- W4_31S / (1-q)
print(W4_31S)


W5_31S_integral <- function(s) {
  lambda_31_S <- lambda_31_S(s)
  W5_31S <- (s^2)* (dnorm(lambda_31_S/c_31_S) / pnorm((lambda_31_S) / c_31_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W5_31S) 
}

W5_31S <- adaptIntegrate(f = W5_31S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W5_31S <- W5_31S / (1-q)
print(W5_31S)


W6_31S_integral <- function(s) {
  lambda_31_S <- lambda_31_S(s)
  W6_31S <- (s)* (dnorm(lambda_31_S/c_31_S) / pnorm((lambda_31_S) / c_31_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W6_31S) 
}

W6_31S <- adaptIntegrate(f = W6_31S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W6_31S <- W6_31S / (1-q)
print(W6_31S)


#i =3, j=2---------------
W1_32S_integral <- function(s) {
  lambda_32_S <- lambda_32_S(s)
  W1_32S <- (dnorm(lambda_32_S/c_32_S) / pnorm((lambda_32_S) / c_32_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W1_32S) 
}

W1_32S <- adaptIntegrate(f = W1_32S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W1_32S <- W1_32S / (1-q)
print(W1_32S)


W2_32S_integral <- function(s) {
  lambda_32_S <- lambda_32_S(s)
  W2_32S <- ((lambda_32_S)^2)* (dnorm(lambda_32_S/c_32_S) / pnorm((lambda_32_S) / c_32_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W2_32S) 
}

W2_32S <- adaptIntegrate(f = W2_32S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W2_32S <- W2_32S / (1-q)
print(W2_32S)

W3_32S_integral <- function(s) {
  lambda_32_S <- lambda_32_S(s)
  W3_32S <- (lambda_32_S)*s* (dnorm(lambda_32_S/c_32_S) / pnorm((lambda_32_S) / c_32_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W3_32S) 
}

W3_32S <- adaptIntegrate(f = W3_32S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W3_32S <- W3_32S / (1-q)
print(W3_32S)



W4_32S_integral <- function(s) {
  lambda_32_S <- lambda_32_S(s)
  W4_32S <- (lambda_32_S)* (dnorm(lambda_32_S/c_32_S) / pnorm((lambda_32_S) / c_32_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W4_32S) 
}

W4_32S <- adaptIntegrate(f = W4_32S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W4_32S <- W4_32S / (1-q)
print(W4_32S)


W5_32S_integral <- function(s) {
  lambda_32_S <- lambda_32_S(s)
  W5_32S <- (s^2)* (dnorm(lambda_32_S/c_32_S) / pnorm((lambda_32_S) / c_32_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W5_32S) 
}

W5_32S <- adaptIntegrate(f = W5_32S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W5_32S <- W5_32S / (1-q)
print(W5_32S)


W6_32S_integral <- function(s) {
  lambda_32_S <- lambda_32_S(s)
  W6_32S <- (s)* (dnorm(lambda_32_S/c_32_S) / pnorm((lambda_32_S) / c_32_S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W6_32S) 
}

W6_32S <- adaptIntegrate(f = W6_32S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W6_32S <- W6_32S / (1-q)
print(W6_32S)



# i=1 j=1 -------------
W1_1S_integral <- function(s) {
  lambda_1_S <- lambda_1_S(s)
  W1_1S <- (dnorm(lambda_1_S/c_1S) / pnorm((lambda_1_S) / c_1S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W1_1S) 
}

W1_1S <- adaptIntegrate(f = W1_1S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W1_1S <- W1_1S / (1-q)
print(W1_1S)


W1_2S_integral <- function(s) {
  lambda_2_S <- lambda_2_S(s)
  W1_2S <- (dnorm(lambda_2_S/c_2S) / pnorm((lambda_2_S) / c_2S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W1_2S) 
}

W1_2S <- adaptIntegrate(f = W1_2S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W1_2S <- W1_2S / (1-q)
print(W1_2S)


W1_3S_integral <- function(s) {
  lambda_3_S <- lambda_3_S(s)
  W1_3S <- (dnorm(lambda_3_S/c_3S) / pnorm((lambda_3_S) / c_3S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W1_3S) 
}

W1_3S <- adaptIntegrate(f = W1_3S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W1_3S <- W1_3S / (1-q)
print(W1_3S)


W2_1S_integral <- function(s) {
  lambda_1_S <- lambda_1_S(s)
  W2_1S <- ((lambda_1_S)^2)* (dnorm(lambda_1_S/c_1S) / pnorm((lambda_1_S) / c_1S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W2_1S) 
}

W2_1S <- adaptIntegrate(f = W2_1S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W2_1S <- W2_1S / (1-q)
print(W2_1S)


W2_2S_integral <- function(s) {
  lambda_2_S <- lambda_2_S(s)
  W2_2S <- ((lambda_2_S)^2)* (dnorm(lambda_2_S/c_2S) / pnorm((lambda_2_S) / c_2S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W2_2S) 
}

W2_2S <- adaptIntegrate(f = W2_2S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W2_2S <- W2_2S / (1-q)
print(W2_2S)


W2_3S_integral <- function(s) {
  lambda_3_S <- lambda_3_S(s)
  W2_3S <- ((lambda_3_S)^2)* (dnorm(lambda_3_S/c_3S) / pnorm((lambda_3_S) / c_3S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W2_3S)
}

W2_3S <- adaptIntegrate(f = W2_3S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W2_3S <- W2_3S / (1-q)
print(W2_3S)


W3_1S_integral <- function(s) {
  lambda_1_S <- lambda_1_S(s)
  W3_1S <- (lambda_1_S)*s* (dnorm(lambda_1_S/c_1S) / pnorm((lambda_1_S) / c_1S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W3_1S) 
}

W3_1S <- adaptIntegrate(f = W3_1S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W3_1S <- W3_1S / (1-q)
print(W3_1S)

W3_2S_integral <- function(s) {
  lambda_2_S <- lambda_2_S(s)
  W3_2S <- (lambda_2_S)*s* (dnorm(lambda_2_S/c_2S) / pnorm((lambda_2_S) / c_2S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W3_2S) 
}

W3_2S <- adaptIntegrate(f = W3_2S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W3_2S <- W3_2S / (1-q)
print(W3_2S)

W3_3S_integral <- function(s) {
  lambda_3_S <- lambda_3_S(s)
  W3_3S <- (lambda_3_S)*s* (dnorm(lambda_3_S/c_3S) / pnorm((lambda_3_S) / c_3S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W3_3S) 
}

W3_3S <- adaptIntegrate(f = W3_3S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W3_3S <- W3_3S / (1-q)
print(W3_3S)




W4_1S_integral <- function(s) {
  lambda_1_S <- lambda_1_S(s)
  W4_1S <- (lambda_1_S)* (dnorm(lambda_1_S/c_1S) / pnorm((lambda_1_S) / c_1S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W4_1S) 
}

W4_1S <- adaptIntegrate(f = W4_1S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W4_1S <- W4_1S / (1-q)
print(W4_1S)


W4_2S_integral <- function(s) {
  lambda_2_S <- lambda_2_S(s)
  W4_2S <- (lambda_2_S)* (dnorm(lambda_2_S/c_2S) / pnorm((lambda_2_S) / c_2S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W4_2S) 
}

W4_2S <- adaptIntegrate(f = W4_2S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W4_2S <- W4_2S / (1-q)
print(W4_2S)


W4_3S_integral <- function(s) {
  lambda_3_S <- lambda_3_S(s)
  W4_3S <- (lambda_3_S)* (dnorm(lambda_3_S/c_3S) / pnorm((lambda_3_S) / c_3S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W4_3S) 
}

W4_3S <- adaptIntegrate(f = W4_3S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W4_3S <- W4_3S / (1-q)
print(W4_3S)

W5_1S_integral <- function(s) {
  lambda_1_S <- lambda_1_S(s)
  W5_1S <- (s^2)* (dnorm(lambda_1_S/c_1S) / pnorm((lambda_1_S) / c_1S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W5_1S) 
}

W5_1S <- adaptIntegrate(f = W5_1S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W5_1S <- W5_1S / (1-q)
print(W5_1S)

W5_2S_integral <- function(s) {
  lambda_2_S <- lambda_2_S(s)
  W5_2S <- (s^2)* (dnorm(lambda_2_S/c_2S) / pnorm((lambda_2_S) / c_2S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W5_2S) 
}

W5_2S <- adaptIntegrate(f = W5_2S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W5_2S <- W5_2S / (1-q)
print(W5_2S)

W5_3S_integral <- function(s) {
  lambda_3_S <- lambda_3_S(s)
  W5_3S <- (s^2)* (dnorm(lambda_3_S/c_3S) / pnorm((lambda_3_S) / c_3S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W5_3S) 
}

W5_3S <- adaptIntegrate(f = W5_3S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W5_3S <- W5_3S / (1-q)
print(W5_3S)



W6_1S_integral <- function(s) {
  lambda_1_S <- lambda_1_S(s)
  W6_1S <- (s)* (dnorm(lambda_1_S/c_1S) / pnorm((lambda_1_S) / c_1S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W6_1S) 
}

W6_1S <- adaptIntegrate(f = W6_1S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W6_1S <- W6_1S / (1-q)
print(W6_1S)

W6_2S_integral <- function(s) {
  lambda_2_S <- lambda_2_S(s)
  W6_2S <- (s)* (dnorm(lambda_2_S/c_2S) / pnorm((lambda_2_S) / c_2S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W6_2S) 
}

W6_2S <- adaptIntegrate(f = W6_2S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W6_2S <- W6_2S / (1-q)
print(W6_2S)

W6_3S_integral <- function(s) {
  lambda_3_S <- lambda_3_S(s)
  W6_3S <- (s)* (dnorm(lambda_3_S/c_3S) / pnorm((lambda_3_S) / c_3S) )*(1/pnorm(lambda_S/c_S))*pnorm(lambda_S+t(alpha_S)*(s-mu_S))*dnorm((s-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S))
  return(W6_3S) 
}

W6_3S <- adaptIntegrate(f = W6_3S_integral, lowerLimit = rep(q_quantile, 1), upperLimit = rep(Inf, 1),tol = 1e-12)$integral

W6_3S <- W6_3S / (1-q)
print(W6_3S)


#c_i = omega_iS/omega_SS   k_j = mu_j - mu_S*omega_jS/omega_SS

c_1 <- omega_1S/omega_SS
c_2 <- omega_2S/omega_SS
c_3 <- omega_3S/omega_SS
k_1 <- mu_1 - (mu_S*omega_1S)/omega_SS
k_2 <- mu_2 - (mu_S*omega_2S)/omega_SS
k_3 <- mu_3 - (mu_S*omega_3S)/omega_SS
delta_1_S <- (omega_11_S* alpha_1S[1])/c_1S
delta_2_S <- (omega_22_S* alpha_2S[1])/c_2S
delta_3_S <- (omega_22_S* alpha_3S[1])/c_3S

#sigma_i_j--------------
sigma_1_2 <- Omega_12_S[1,2] - (1/c_12_S)*delta_12_S[1]*delta_12_S[2]*W4_12S+ (delta_12_S[2]*c_1+delta_12_S[1]*c_2)*W6_12S+ (delta_12_S[2]*k_1+delta_12_S[1]*k_2)*W1_12S+
            c_1*c_2*M2+(c_1*k_2+c_2*k_1)*M1+k_1*k_2  - (c_1*M1+k_1+delta_12_S[1]*W1_1S)*(c_2*M1+k_2+delta_12_S[2]*W1_2S)
sigma_1_2 

sigma_2_1 <- Omega_21_S[1,2] - (1/c_21_S)*delta_21_S[1]*delta_21_S[2]*W4_21S+ (delta_21_S[2]*c_2+c_1*delta_21_S[1])*W6_21S+ (delta_21_S[2]*k_2+k_1*delta_21_S[2])*W1_21S+
  c_2*c_1*M2+ (c_2*k_1+c_1*k_2)*M1+k_2*k_1  - (c_2*M1+k_2+delta_21_S[1]*W1_2S)*(c_1*M1+k_1+delta_21_S[2]*W1_1S)
sigma_2_1 


sigma_1_3 <- Omega_13_S[1,2] - (1/c_13_S)*delta_13_S[1]*delta_13_S[2]*W4_13S+ (delta_13_S[2]*c_1+delta_13_S[1]*c_3)*W6_13S+ (delta_13_S[2]*k_1+delta_13_S[1]*k_3)*W1_13S+
  c_1*c_3*M2+(c_1*k_3+c_3*k_1)*M1+k_1*k_3  - (c_1*M1+k_1+delta_13_S[1]*W1_1S)*(c_3*M1+k_3+delta_13_S[2]*W1_3S)
sigma_1_3 

sigma_3_1 <- Omega_31_S[1,2] - (1/c_31_S)*delta_31_S[1]*delta_31_S[2]*W4_31S+(delta_31_S[2]*c_3+delta_31_S[1]*c_1)*W6_31S+ (delta_31_S[2]*k_3+delta_31_S[1]*k_1)*W1_31S+
  c_3*c_1*M2+(c_3*k_1+c_1*k_3)*M1+k_3*k_1  - (c_3*M1+k_3+delta_31_S[1]*W1_3S)*(c_1*M1+k_1+delta_31_S[2]*W1_1S)
sigma_3_1 


sigma_2_3 <- Omega_23_S[1,2] - (1/c_23_S)*delta_23_S[1]*delta_23_S[2]*W4_23S+(delta_23_S[2]*c_2+c_3*delta_23_S[1])*W6_23S+ (delta_23_S[2]*k_2+k_3*delta_23_S[2])*W1_23S+
  c_2*c_3*M2+(c_2*k_3+c_3*k_2)*M1+k_2*k_3  - (c_2*M1+k_2+delta_23_S[1]*W1_2S)*(c_3*M1+k_3+delta_23_S[2]*W1_3S)
sigma_2_3 

sigma_3_2 <- sigma_2_3


sigma_1_1 <- Omega_12_S[1,1]-(1/c_1S)*(delta_1_S^2)*W4_1S+2*delta_1_S*c_1*W6_1S+2*delta_1_S*k_1*W1_1S+(c_1^2)*M2+2*c_1*k_1*M1+k_1^2  -
          (c_1*M1+k_1+delta_1_S*W1_1S)^2


sigma_2_2 <- Omega_21_S[1,1]-(1/c_2S)*(delta_2_S^2)*W4_2S+2*delta_2_S*c_2*W6_2S+2*delta_2_S*k_2*W1_2S+(c_2^2)*M2+2*c_2*k_2*M1+k_2^2  -
  (c_2*M1+k_2+delta_2_S*W1_2S)^2
sigma_2_2

sigma_3_3 <- Omega_31_S[1,1]-(1/c_3S)*(delta_3_S^2)*W4_3S+2*delta_3_S*c_3*W6_3S+2*delta_3_S*k_3*W1_3S+(c_3^2)*M2+2*c_3*k_3*M1+k_3^2  -
  (c_3*M1+k_3+delta_3_S*W1_3S)^2
sigma_3_3


#solve M3_12s------
A <- matrix(c(0, -1, 1, 0), nrow = 2, ncol = 2)
M_12S <- t(A) %*% solve(Omega_12_S) %*% delta_12_S %*% t(delta_12_S) %*% solve(Omega_12_S) %*% A
gamma1_12S <- function(s) {
  return(2 * (alpha1_12_S / c_12_S) * (dnorm(lambda_12_S(s)/c_12_S) - (1 / sqrt(2 * pi))) +
           (alpha1_12_S / c_12_S^3) * (dnorm(lambda_12_S(s)/c_12_S )  - (1 / sqrt(2 * pi))) +
           ((alpha1_12_S^3 * lambda_12_S(s)^2) / c_12_S^5) * (1 / sqrt(2 * pi)) * exp(-0.5 * (lambda_12_S((s)) / c_12_S)^2))
}
gamma2_12S <- function(s) {
  return((alpha1_12_S / c_12_S) * ( dnorm(lambda_12_S(s)/c_12_S) - (1 / sqrt(2 * pi))))
}

Gamma1_12S <- function(s) {
  return(1 / pnorm(lambda_12_S(s) / c_12_S) * gamma1_12S(s) * (c_12_S / alpha1_12_S) ^ 3)
}
Gamma2_12S <- function(s) {
  return(1 / pnorm(lambda_12_S(s) / c_12_S) * gamma2_12S(s) * det(Omega_12_S) * (c_12_S / alpha1_12_S) ^ 3)
}

E3_12S <- function(s) {
  return ((1 / sqrt(2 * pi)) * (1 / pnorm(lambda_12_S(s) / c_12_S)) * (2 * delta_12_S[2] * Omega_12_S[1, 2] + delta_12_S[1] * Omega_12_S[2, 2] - delta_12_S[1] * (delta_12_S[2] ^ 2)) +
            Gamma2_12S(s) * (2 * delta_12_S[2] * M_12S[1, 2] + delta_12_S[1] * M_12S[2, 2]) + Gamma1_12S(s) * delta_12_S[1] * (delta_12_S[2] ^ 2))
}
E3_int_12S <- function(s) {
  return(E3_12S(s) * fx(s))
}
M3_12S <- integrate(E3_int_12S, lower = q_quantile, upper = Inf,subdivisions = 10000000)$value
M3_12S <- M3_12S/(1-q)
print(M3_12S)

#solve M3_13s------
A <- matrix(c(0, -1, 1, 0), nrow = 2, ncol = 2)
M_13S <- t(A) %*% solve(Omega_13_S) %*% delta_13_S %*% t(delta_13_S) %*% solve(Omega_13_S) %*% A

gamma1_13S <- function(s) {
  return(2 * (alpha1_13_S / c_13_S) * (dnorm(lambda_13_S(s)/c_13_S) - (1 / sqrt(2 * pi))) +
           (alpha1_13_S / c_13_S^3) * (dnorm(lambda_13_S(s)/c_13_S )  - (1 / sqrt(2 * pi))) +
           ((alpha1_13_S^3 * lambda_13_S(s)^2) / c_13_S^5) * (1 / sqrt(2 * pi)) * exp(-0.5 * (lambda_13_S((s)) / c_13_S)^2))
}
gamma2_13S <- function(s) {
  return((alpha1_13_S / c_13_S) * ( dnorm(lambda_13_S(s)/c_13_S) - (1 / sqrt(2 * pi))))
}
Gamma1_13S <- function(s) {
  return(1 / pnorm(lambda_13_S(s) / c_13_S) * gamma1_13S(s) * (c_13_S / alpha1_13_S) ^ 3)
}
Gamma2_13S <- function(s) {
  return(1 / pnorm(lambda_13_S(s) / c_13_S) * gamma2_13S(s) * det(Omega_13_S) * (c_13_S / alpha1_13_S) ^ 3)
}
E3_13S <- function(s) {
  return ((1 / sqrt(2 * pi)) * (1 / pnorm(lambda_13_S(s) / c_13_S)) * (2 * delta_13_S[2] * Omega_13_S[1, 2] + delta_13_S[1] * Omega_13_S[2, 2] - delta_13_S[1] * (delta_13_S[2] ^ 2)) +
            Gamma2_13S(s) * (2 * delta_13_S[2] * M_13S[1, 2] + delta_13_S[1] * M_13S[2, 2]) + Gamma1_13S(s) * delta_13_S[1] * (delta_13_S[2] ^ 2))
}
E3_int_13S <- function(s) {
  return(E3_13S(s) * fx(s))
}
M3_13S <- integrate(E3_int_13S, lower = q_quantile, upper = Inf,subdivisions = 10000000)$value
M3_13S <- M3_13S/(1-q)
print(M3_13S)
#solve M3_21s------
A <- matrix(c(0, -1, 1, 0), nrow = 2, ncol = 2)
M_21S <- t(A) %*% solve(Omega_21_S) %*% delta_21_S %*% t(delta_21_S) %*% solve(Omega_21_S) %*% A
gamma1_21S <- function(s) {
  return(2 * (alpha1_21_S / c_21_S) * (dnorm(lambda_21_S(s)/c_21_S) - (1 / sqrt(2 * pi))) +
           (alpha1_21_S / c_21_S^3) * (dnorm(lambda_21_S(s)/c_21_S )  - (1 / sqrt(2 * pi))) +
           ((alpha1_21_S^3 * lambda_21_S(s)^2) / c_21_S^5) * (1 / sqrt(2 * pi)) * exp(-0.5 * (lambda_21_S((s)) / c_21_S)^2))
}
gamma2_21S <- function(s) {
  return((alpha1_21_S / c_21_S) * ( dnorm(lambda_21_S(s)/c_21_S) - (1 / sqrt(2 * pi))))
}
Gamma1_21S <- function(s) {
  return(1 / pnorm(lambda_21_S(s) / c_21_S) * gamma1_21S(s) * (c_21_S / alpha1_21_S) ^ 3)
}
Gamma2_21S <- function(s) {
  return(1 / pnorm(lambda_21_S(s) / c_21_S) * gamma2_21S(s) * det(Omega_21_S) * (c_21_S / alpha1_21_S) ^ 3)
}
E3_21S <- function(s) {
  return ((1 / sqrt(2 * pi)) * (1 / pnorm(lambda_21_S(s) / c_21_S)) * (2 * delta_21_S[2] * Omega_21_S[1, 2] + delta_21_S[1] * Omega_21_S[2, 2] - delta_21_S[1] * (delta_21_S[2] ^ 2)) +
            Gamma2_21S(s) * (2 * delta_21_S[2] * M_21S[1, 2] + delta_21_S[1] * M_21S[2, 2]) + Gamma1_21S(s) * delta_21_S[1] * (delta_21_S[2] ^ 2))
}
E3_int_21S <- function(s) {
  return(E3_21S(s) * fx(s))
}
M3_21S <- integrate(E3_int_21S, lower = q_quantile, upper = Inf,subdivisions = 10000000)$value
M3_21S <- M3_21S/(1-q)
print(M3_21S)

#solve M3_23s------
A <- matrix(c(0, -1, 1, 0), nrow = 2, ncol = 2)
M_23S <- t(A) %*% solve(Omega_23_S) %*% delta_23_S %*% t(delta_23_S) %*% solve(Omega_23_S) %*% A
gamma1_23S <- function(s) {
  return(2 * (alpha1_23_S / c_23_S) * (dnorm(lambda_23_S(s)/c_23_S) - (1 / sqrt(2 * pi))) +
           (alpha1_23_S / c_23_S^3) * (dnorm(lambda_23_S(s)/c_23_S )  - (1 / sqrt(2 * pi))) +
           ((alpha1_23_S^3 * lambda_23_S(s)^2) / c_23_S^5) * (1 / sqrt(2 * pi)) * exp(-0.5 * (lambda_23_S((s)) / c_23_S)^2))
}
gamma2_23S <- function(s) {
  return((alpha1_23_S / c_23_S) * ( dnorm(lambda_23_S(s)/c_23_S) - (1 / sqrt(2 * pi))))
}
Gamma1_23S <- function(s) {
  return(1 / pnorm(lambda_23_S(s) / c_23_S) * gamma1_23S(s) * (c_23_S / alpha1_23_S) ^ 3)
}
Gamma2_23S <- function(s) {
  return(1 / pnorm(lambda_23_S(s) / c_23_S) * gamma2_23S(s) * det(Omega_23_S) * (c_23_S / alpha1_23_S) ^ 3)
}
E3_23S <- function(s) {
  return ((1 / sqrt(2 * pi)) * (1 / pnorm(lambda_23_S(s) / c_23_S)) * (2 * delta_23_S[2] * Omega_23_S[1, 2] + delta_23_S[1] * Omega_23_S[2, 2] - delta_23_S[1] * (delta_23_S[2] ^ 2)) +
            Gamma2_23S(s) * (2 * delta_23_S[2] * M_23S[1, 2] + delta_23_S[1] * M_23S[2, 2]) + Gamma1_23S(s) * delta_23_S[1] * (delta_23_S[2] ^ 2))
}
E3_int_23S <- function(s) {
  return(E3_23S(s) * fx(s))
}
M3_23S <- integrate(E3_int_23S, lower = q_quantile, upper = Inf,subdivisions = 10000000)$value
M3_23S <- M3_23S/(1-q)
print(M3_23S)

#solve M3_31s------
A <- matrix(c(0, -1, 1, 0), nrow = 2, ncol = 2)
M_31S <- t(A) %*% solve(Omega_31_S) %*% delta_31_S %*% t(delta_31_S) %*% solve(Omega_31_S) %*% A
gamma1_31S <- function(s) {
  return(2 * (alpha1_31_S / c_31_S) * (dnorm(lambda_31_S(s)/c_31_S) - (1 / sqrt(2 * pi))) +
           (alpha1_31_S / c_31_S^3) * (dnorm(lambda_31_S(s)/c_31_S )  - (1 / sqrt(2 * pi))) +
           ((alpha1_31_S^3 * lambda_31_S(s)^2) / c_31_S^5) * (1 / sqrt(2 * pi)) * exp(-0.5 * (lambda_31_S((s)) / c_31_S)^2))
}
gamma2_31S <- function(s) {
  return((alpha1_31_S / c_31_S) * ( dnorm(lambda_31_S(s)/c_31_S) - (1 / sqrt(2 * pi))))
}
Gamma1_31S <- function(s) {
  return(1 / pnorm(lambda_31_S(s) / c_31_S) * gamma1_31S(s) * (c_31_S / alpha1_31_S) ^ 3)
}
Gamma2_31S <- function(s) {
  return(1 / pnorm(lambda_31_S(s) / c_31_S) * gamma2_31S(s) * det(Omega_31_S) * (c_31_S / alpha1_31_S) ^ 3)
}
E3_31S <- function(s) {
  return ((1 / sqrt(2 * pi)) * (1 / pnorm(lambda_31_S(s) / c_31_S)) * (2 * delta_31_S[2] * Omega_31_S[1, 2] + delta_31_S[1] * Omega_31_S[2, 2] - delta_31_S[1] * (delta_31_S[2] ^ 2)) +
            Gamma2_31S(s) * (2 * delta_31_S[2] * M_31S[1, 2] + delta_31_S[1] * M_31S[2, 2]) + Gamma1_31S(s) * delta_31_S[1] * (delta_31_S[2] ^ 2))
}
E3_int_31S <- function(s) {
  return(E3_31S(s) * fx(s))
}
M3_31S <- integrate(E3_int_31S, lower = q_quantile, upper = Inf,subdivisions = 10000000)$value
M3_31S <- M3_31S/(1-q)
print(M3_31S)

#solve M3_32s------
A <- matrix(c(0, -1, 1, 0), nrow = 2, ncol = 2)
M_32S <- t(A) %*% solve(Omega_32_S) %*% delta_32_S %*% t(delta_32_S) %*% solve(Omega_32_S) %*% A
gamma1_32S <- function(s) {
  return(2 * (alpha1_32_S / c_32_S) * (dnorm(lambda_32_S(s)/c_32_S) - (1 / sqrt(2 * pi))) +
           (alpha1_32_S / c_32_S^3) * (dnorm(lambda_32_S(s)/c_32_S )  - (1 / sqrt(2 * pi))) +
           ((alpha1_32_S^3 * lambda_32_S(s)^2) / c_32_S^5) * (1 / sqrt(2 * pi)) * exp(-0.5 * (lambda_32_S((s)) / c_32_S)^2))
}
gamma2_32S <- function(s) {
  return((alpha1_32_S / c_32_S) * ( dnorm(lambda_32_S(s)/c_32_S) - (1 / sqrt(2 * pi))))
}
Gamma1_32S <- function(s) {
  return(1 / pnorm(lambda_32_S(s) / c_32_S) * gamma1_32S(s) * (c_32_S / alpha1_32_S) ^ 3)
}
Gamma2_32S <- function(s) {
  return(1 / pnorm(lambda_32_S(s) / c_32_S) * gamma2_32S(s) * det(Omega_32_S) * (c_32_S / alpha1_32_S) ^ 3)
}
E3_32S <- function(s) {
  return ((1 / sqrt(2 * pi)) * (1 / pnorm(lambda_32_S(s) / c_32_S)) * (2 * delta_32_S[2] * Omega_32_S[1, 2] + delta_32_S[1] * Omega_32_S[2, 2] - delta_32_S[1] * (delta_32_S[2] ^ 2)) +
            Gamma2_32S(s) * (2 * delta_32_S[2] * M_32S[1, 2] + delta_32_S[1] * M_32S[2, 2]) + Gamma1_32S(s) * delta_32_S[1] * (delta_32_S[2] ^ 2))
}
E3_int_32S <- function(s) {
  return(E3_32S(s) * fx(s))
}
M3_32S <- integrate(E3_int_32S, lower = q_quantile, upper = Inf,subdivisions = 10000000)$value
M3_32S <- M3_32S/(1-q)
print(M3_32S)
# sigma_2_i_j------
sigma_2_1_2 <- matrix(M3_12S, nrow = 1, ncol = 1)+2*k_2*Omega_12_S[1,2]+2*c_2*Omega_12_S[1,2]*M1 + k_1*Omega_12_S[2,2]+
             c_1*Omega_12_S[2,2]*M1 - (2/c_12_S)*delta_12_S[1]*delta_12_S[2]*c_2*W3_12S - (2/c_12_S)*delta_12_S[1]*delta_12_S[2]*k_2*W4_12S-
     ((delta_12_S[2]^2)/c_12_S)*c_1*W3_12S - ((delta_12_S[2]^2)/c_12_S)*k_1*W4_12S + 2*delta_12_S[2]*c_1*c_2*W5_12S +4*c_1*k_2*delta_12_S[2]*W6_12S+
     2*k_1*k_2*delta_12_S[2]*W1_12S + (c_2^2)*delta_12_S[1]*W5_12S +2*c_2*k_2*delta_12_S[1]*W6_12S +(k_2^2)*delta_12_S[1]*W1_12S +c_1*(c_2^2)*M3 + (c_2^2)*k_1*M2+
     2*c_1*c_2*k_2*M2+2*c_2*k_1*k_2*M1 + c_1*(k_2^2)*M1+k_1*(k_2^2)- (c_1*M1+k_1+delta_1_S*W1_1S)*(Omega_21_S[1,1]-(1/c_2S)*(delta_2_S^2)*W4_2S+2*delta_2_S*c_2*W6_2S+
                                                                                                     2*delta_2_S*k_2*W1_2S+(c_2^2)*M2+2*c_2*k_2*M1+k_2^2 )
sigma_2_1_2

sigma_2_1_3 <- matrix(M3_13S, nrow = 1, ncol = 1)+2*k_3*Omega_13_S[1,2] +2*c_3*Omega_13_S[1,2]*M1+ k_1*Omega_13_S[2,2]+
  c_1*Omega_13_S[2,2]*M1 - (2/c_13_S)*delta_13_S[1]*delta_13_S[2]*c_3*W3_13S - (2/c_13_S)*delta_13_S[1]*delta_13_S[2]*k_3*W4_13S-
  ((delta_13_S[2]^2)/c_13_S)*c_1*W3_13S - ((delta_13_S[2]^2)/c_13_S)*k_1*W4_13S + 2*delta_13_S[2]*c_1*c_3*W5_13S +4*c_1*k_3*delta_13_S[2]*W6_13S+
  2*k_1*k_3*delta_13_S[2]*W1_13S + (c_3^2)*delta_13_S[1]*W5_13S +2*c_3*k_3*delta_13_S[1]*W6_13S +(k_3^2)*delta_13_S[1]*W1_13S +c_1*(c_3^2)*M3 + (c_3^2)*k_1*M2+
  2*c_1*c_3*k_3*M2+2*c_3*k_1*k_3*M1 + c_1*(k_3^2)*M1+k_1*(k_3^2)- (c_1*M1+k_1+delta_1_S*W1_1S)*(Omega_31_S[1,1]-(1/c_3S)*(delta_3_S^2)*W4_3S+2*delta_3_S*c_3*W6_3S+
                                                                                                  2*delta_3_S*k_3*W1_3S+(c_3^2)*M2+2*c_3*k_3*M1+k_3^2) 
   sigma_2_1_3                                                                                             
                                                                                                
sigma_2_2_1 <- matrix(M3_21S, nrow = 1, ncol = 1)+2*k_1*Omega_21_S[1,2] +2*c_1*Omega_21_S[1,2]*M1+ k_2*Omega_21_S[2,2]+
  c_2*Omega_21_S[2,2]*M1 - (2/c_21_S)*delta_21_S[1]*delta_21_S[2]*c_1*W3_21S - (2/c_21_S)*delta_21_S[1]*delta_21_S[2]*k_1*W4_21S-
  ((delta_21_S[2]^2)/c_21_S)*c_2*W3_21S - ((delta_21_S[2]^2)/c_21_S)*k_2*W4_21S + 2*delta_21_S[2]*c_2*c_1*W5_21S +4*c_2*k_1*delta_21_S[2]*W6_21S+
  2*k_2*k_1*delta_21_S[2]*W1_21S + (c_1^2)*delta_21_S[1]*W5_21S +2*c_1*k_1*delta_21_S[1]*W6_21S +(k_1^2)*delta_21_S[1]*W1_21S +c_2*(c_1^2)*M3 + (c_1^2)*k_2*M2+
  2*c_2*c_1*k_1*M2+2*c_1*k_2*k_1*M1 + c_2*(k_1^2)*M1+k_2*(k_1^2)- (c_2*M1+k_2+delta_2_S*W1_2S)*(Omega_12_S[1,1]-(1/c_1S)*(delta_1_S^2)*W4_1S+2*delta_1_S*c_1*W6_1S+
                                                                                                  2*delta_1_S*k_1*W1_1S+(c_1^2)*M2+2*c_1*k_1*M1+k_1^2 )
sigma_2_2_1
sigma_2_2_3 <- matrix(M3_23S, nrow = 1, ncol = 1)+2*k_3*Omega_23_S[1,2] +2*c_3*Omega_23_S[1,2]*M1 + k_2*Omega_23_S[2,2]+
  c_2*Omega_23_S[2,2]*M1 - (2/c_23_S)*delta_23_S[1]*delta_23_S[2]*c_3*W3_23S - (2/c_23_S)*delta_23_S[1]*delta_23_S[2]*k_3*W4_23S-
  ((delta_23_S[2]^2)/c_23_S)*c_2*W3_23S - ((delta_23_S[2]^2)/c_23_S)*k_2*W4_23S + 2*delta_23_S[2]*c_2*c_3*W5_23S +4*c_2*k_3*delta_23_S[2]*W6_23S+
  2*k_2*k_3*delta_23_S[2]*W1_23S + (c_3^2)*delta_23_S[1]*W5_23S +2*c_3*k_3*delta_23_S[1]*W6_23S +(k_3^2)*delta_23_S[1]*W1_23S +c_2*(c_3^2)*M3 + (c_3^2)*k_2*M2+
  2*c_2*c_3*k_3*M2+2*c_3*k_2*k_3*M1 + c_2*(k_3^2)*M1+k_2*(k_3^2)- (c_2*M1+k_2+delta_2_S*W1_2S)*(Omega_31_S[1,1]-(1/c_3S)*(delta_3_S^2)*W4_3S+2*delta_3_S*c_3*W6_3S+
                                                                                                  2*delta_3_S*k_3*W1_3S+(c_3^2)*M2+2*c_3*k_3*M1+k_3^2 )
sigma_2_2_3
sigma_2_3_1 <- matrix(M3_12S, nrow = 1, ncol = 1) + 2*k_1*Omega_31_S[1,2] +2*c_1*Omega_31_S[1,2]*M1 + k_3*Omega_31_S[2,2]+
  c_3*Omega_31_S[2,2]*M1 - (2/c_31_S)*delta_31_S[1]*delta_31_S[2]*c_1*W3_31S - (2/c_31_S)*delta_31_S[1]*delta_31_S[2]*k_1*W4_31S-
  ((delta_31_S[2]^2)/c_31_S)*c_3*W3_31S - ((delta_31_S[2]^2)/c_31_S)*k_3*W4_31S + 2*delta_31_S[2]*c_3*c_1*W5_31S +4*c_3*k_1*delta_31_S[2]*W6_31S+
  2*k_3*k_1*delta_31_S[2]*W1_31S + (c_1^2)*delta_31_S[1]*W5_31S +2*c_1*k_1*delta_31_S[1]*W6_31S +(k_1^2)*delta_31_S[1]*W1_31S +c_3*(c_1^2)*M3 + (c_1^2)*k_3*M2+
  2*c_3*c_1*k_1*M2+2*c_1*k_3*k_1*M1 + c_3*(k_1^2)*M1+k_3*(k_1^2)- (c_3*M1+k_3+delta_3_S*W1_3S)*(Omega_13_S[1,1]-(1/c_1S)*(delta_1_S^2)*W4_1S+2*delta_1_S*c_1*W6_1S+
                                                                                                  2*delta_1_S*k_1*W1_1S+(c_1^2)*M2+2*c_1*k_1*M1+k_1^2 )

sigma_2_3_1
sigma_2_3_2 <- matrix(M3_32S, nrow = 1, ncol = 1)+2*k_2*Omega_32_S[1,2]+2*c_2*Omega_32_S[1,2]*M1 + k_3*Omega_32_S[2,2]+
  c_3*Omega_32_S[2,2]*M1 - (2/c_32_S)*delta_32_S[1]*delta_32_S[2]*c_2*W3_32S - (2/c_32_S)*delta_32_S[1]*delta_32_S[2]*k_2*W4_32S-
  ((delta_32_S[2]^2)/c_32_S)*c_3*W3_32S - ((delta_32_S[2]^2)/c_32_S)*k_3*W4_32S + 2*delta_32_S[2]*c_3*c_2*W5_32S +4*c_3*k_2*delta_32_S[2]*W6_32S+
  2*k_3*k_2*delta_32_S[2]*W1_32S + (c_2^2)*delta_32_S[1]*W5_32S +2*c_2*k_2*delta_32_S[1]*W6_32S +(k_2^2)*delta_32_S[1]*W1_32S +c_3*(c_2^2)*M3 + (c_2^2)*k_3*M2+
  2*c_3*c_2*k_2*M2+2*c_2*k_3*k_2*M1 + c_3*(k_2^2)*M1+k_3*(k_2^2)- (c_3*M1+k_3+delta_3_S*W1_3S)*(Omega_23_S[1,1]-(1/c_2S)*(delta_2_S^2)*W4_2S+2*delta_2_S*c_2*W6_2S+
                                                                                                  2*delta_2_S*k_2*W1_2S+(c_2^2)*M2+2*c_2*k_2*M1+k_2^2 )
sigma_2_3_2
#---------------


E3_1S <- function(s) {
  return ((1 / sqrt(2 * pi)) * (1 / pnorm(lambda_12_S(s) / c_12_S)) * (3 * delta_12_S[1] * Omega_12_S[1, 1] - (delta_12_S[1] ^ 3)) +
            Gamma2_12S(s) * (3 * delta_12_S[1] * M_12S[1, 1] ) + Gamma1_12S(s) * (delta_12_S[1] ^ 3))
}
E3_int_1S <- function(s) {
  return(E3_1S(s) * fx(s))
}
M3_1S <- integrate(E3_int_1S, lower = q_quantile, upper = Inf,subdivisions = 10000000)$value
M3_1S <- M3_1S/(1-q)
print(M3_1S)

E3_2S <- function(s) {
  return ((1 / sqrt(2 * pi)) * (1 / pnorm(lambda_12_S(s) / c_12_S)) * (3 * delta_12_S[2] * Omega_12_S[2, 2] - (delta_12_S[2] ^ 3)) +
            Gamma2_12S(s) * (3 * delta_12_S[2] * M_12S[2, 2] ) + Gamma1_12S(s) * (delta_12_S[2] ^ 3))
}
E3_int_2S <- function(s) {
  return(E3_2S(s) * fx(s))
}
M3_2S <- integrate(E3_int_2S, lower = q_quantile, upper = Inf,subdivisions = 10000000)$value
M3_2S <- M3_2S/(1-q)
print(M3_2S)

E3_3S <- function(s) {
  return ((1 / sqrt(2 * pi)) * (1 / pnorm(lambda_13_S(s) / c_13_S)) * (3 * delta_13_S[2] * Omega_13_S[2, 2] - (delta_13_S[2] ^ 3)) +
            Gamma2_13S(s) * (3 * delta_13_S[2] * M_13S[2, 2] ) + Gamma1_13S(s) * (delta_13_S[2] ^ 3))
}
E3_int_3S <- function(s) {
  return(E3_3S(s) * fx(s))
}

M3_3S <- integrate(E3_int_3S, lower = q_quantile, upper = Inf,subdivisions = 10000000)$value
M3_3S <- M3_3S/(1-q)
print(M3_3S)


sigma_2_1_1 <- M3_1S+ 3*omega_11_S*c_1*M1 + 3*omega_11_S*k_1 - 
  3*(delta_1_S^2)*c_1*(1/c_1S)*W3_1S - 3*k_1*(delta_1_S^2)*(1/c_1S)*W4_1S + (c_1^3)*M3 + 3*(c_1^2)*k_1*M2+
  3*c_1*(k_1^2)*M1 + (k_1^3) + 3*(c_1^2)*delta_1_S*W5_1S + 6*c_1*k_1*delta_1_S*W6_1S + 3*(k_1^2)*delta_1_S*W1_1S - 
  (Omega_12_S[1,1]-(1/c_1S)*(delta_1_S^2)*W4_1S+2*delta_1_S*c_1*W6_1S+2*delta_1_S*k_1*W1_1S+(c_1^2)*M2+2*c_1*k_1*M1+k_1^2 )*(c_1*M1+k_1+delta_1_S*W1_1S)

sigma_2_1_1



sigma_2_2_2 <-M3_2S +3*omega_22_S*c_2*M1 + 3*omega_22_S*k_2 - 
  3*(delta_2_S^2)*c_2*(1/c_2S)*W3_2S - 3*k_2*(delta_2_S^2)*(1/c_2S)*W4_2S + (c_2^3)*M3 + 3*(c_2^2)*k_2*M2+
  3*c_2*(k_2^2)*M1 + (k_2^3) + 3*(c_2^2)*delta_2_S*W5_2S + 6*c_2*k_2*delta_2_S*W6_2S + 3*(k_2^2)*delta_2_S*W1_2S-
  (Omega_21_S[1,1]-(1/c_2S)*(delta_2_S^2)*W4_2S+2*delta_2_S*c_2*W6_2S+2*delta_2_S*k_2*W1_2S+(c_2^2)*M2+2*c_2*k_2*M1+k_2^2 )*(c_2*M1+k_2+delta_2_S*W1_2S)
  
 sigma_2_2_2 

  
 sigma_2_3_3 <-M3_3S +3*omega_33_S*c_3*M1 + 3*omega_33_S*k_3 - 
   3*(delta_3_S^2)*c_3*(1/c_3S)*W3_3S - 3*k_3*(delta_3_S^2)*(1/c_3S)*W4_3S + (c_3^3)*M3 + 3*(c_3^2)*k_3*M2+
   3*c_3*(k_3^2)*M1 + (k_3^3) + 3*(c_3^2)*delta_3_S*W5_3S + 6*c_3*k_3*delta_3_S*W6_3S + 3*(k_3^2)*delta_3_S*W1_3S-
   (Omega_31_S[1,1]-(1/c_3S)*(delta_3_S^2)*W4_3S+2*delta_3_S*c_3*W6_3S+ 2*delta_3_S*k_3*W1_3S+(c_3^2)*M2+2*c_3*k_3*M1+k_3^2 )*(c_3*M1+k_3+delta_3_S*W1_3S)
  
  sigma_2_3_3
  mu1 =c_1*M1 + k_1 +delta_1_S*W1_1S
  mu2 =c_2*M1 + k_2 +delta_2_S*W1_2S
  mu3 =c_3*M1 + k_3 +delta_3_S*W1_3S
  
  #final result------------------
  SIGMA_S <- matrix(c(sigma_1_1,sigma_1_2,sigma_1_3,sigma_2_1,sigma_2_2,sigma_2_3,sigma_3_1,sigma_3_2,sigma_3_3),nrow=3, ncol=3, byrow=TRUE)
  
  SIGMA_S 
  beta= 0.01
  a=solve((8*beta*SIGMA_S+2*diag(3)))
  a
  
  # solve zetai
  zetai <- numeric(3)

    zetai <- c(mu1, mu2, mu3)
    
  
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



  