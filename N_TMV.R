rm(list = ls())
library("stats")
library(cubature)
mu <- c(6, 10, 5)
Omega <- matrix(c(1, 0.5, 0.1, 0.5, 3, -0.5, 0.1, -0.5, 1), nrow = 3, ncol = 3,byrow = TRUE)
Omega
Omega
lambda <-0
I <- c(1, 1, 1)
mu_S <- t(I) %*% mu
Omega_S <- t(I) %*% Omega %*% I
omega_S <- sqrt(Omega_S)

q <- 0.95

#-----------------
fx <- function(x) {
  result <- dnorm((x-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)) 
  return(result)
}
F_x <- function(x) integrate(fx, -Inf, x)$value



q_quantile <- uniroot(function(x) F_x(x)-q, interval = c(mu_S-20,mu_S+20))$root 


print(q_quantile) 


conditional_expectation_function <- function(x){
  x*(dnorm((x-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)))
}  


M1 <- integrate(conditional_expectation_function, lower = q_quantile, upper = Inf)$value


M1 <- M1 / (1-q)


print(M1)




conditional_expectation_function_square <- function(x){
  x^2*(dnorm((x-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)))
}

M2 <- integrate(conditional_expectation_function_square, lower = q_quantile, upper= Inf)$value

M2 <- M2 / (1-q)

print(M2)




conditional_expectation_function_square <- function(x){
  x^3*(dnorm((x-mu_S)/sqrt(Omega_S))*(1/sqrt(Omega_S)))
}


M3 <- integrate(conditional_expectation_function_square, lower = q_quantile, upper= Inf)$value

M3 <- M3 / (1-q)

print(M3)



Var <- M2 - (M1)^2

print(Var)

B <- matrix(c(1, 0, 0, 0, 1, 0, 1, 1, 1), nrow = 3, ncol = 3, byrow=TRUE)
mu_12S = B %*% mu
Omega_12S = B %*% Omega %*% t(B)

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


B_23 <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 1), nrow = 3, ncol = 3, byrow=TRUE)
mu_23S = B_23 %*% mu
Omega_23S = B_23 %*% Omega %*% t(B_23)

omega_23 <- Omega_23S[1,2]
omega_32 <- Omega_23S[2,1]

B_31 <- matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 1), nrow = 3, ncol = 3, byrow=TRUE)
mu_31S = B_31 %*% mu
Omega_31S = B_31 %*% Omega %*% t(B_31)


B_32 <- matrix(c(0, 0, 1, 0, 1, 0, 1, 1, 1), nrow = 3, ncol = 3, byrow=TRUE)
mu_32S = B_32 %*% mu
Omega_32S = B_32 %*% Omega %*% t(B_32)

B_1 <- matrix(c(1,0,0,1,1,1), nrow=2, ncol =3, byrow=TRUE)
B_2 <- matrix(c(0,1,0,1,1,1), nrow=2, ncol =3, byrow=TRUE)
B_3 <- matrix(c(0,0,1,1,1,1), nrow=2, ncol =3, byrow=TRUE)
mu_1S <- B_1 %*% mu
mu_2S <- B_2 %*% mu
mu_3S <- B_3 %*% mu
mu_1S
mu_2S
mu_3S

Omega_12_S <- matrix(c(omega_11,omega_12,omega_21,omega_22),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_1S,omega_2S),nrow=2,ncol=1)%*%matrix(c(omega_1S,omega_2S),nrow=1,ncol=2)


Omega_13_S <- matrix(c(omega_11,omega_13,omega_31,omega_33),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_1S,omega_3S),nrow=2,ncol=1)%*%matrix(c(omega_1S,omega_3S),nrow=1,ncol=2)

Omega_21_S <- matrix(c(omega_22,omega_21,omega_12,omega_11),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_2S,omega_1S),nrow=2,ncol=1)%*%matrix(c(omega_2S,omega_1S),nrow=1,ncol=2)

Omega_23_S <- matrix(c(omega_22,omega_23,omega_32,omega_33),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_2S,omega_3S),nrow=2,ncol=1)%*%matrix(c(omega_2S,omega_3S),nrow=1,ncol=2)


Omega_31_S <- matrix(c(omega_33,omega_31,omega_13, omega_11),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_3S,omega_1S),nrow=2,ncol=1)%*%matrix(c(omega_3S,omega_1S),nrow=1,ncol=2)


Omega_32_S <- matrix(c(omega_33,omega_32,omega_23,omega_22),nrow=2,ncol=2,byrow=TRUE) - rep((1/omega_SS),2)*matrix(c(omega_3S,omega_2S),nrow=2,ncol=1)%*%matrix(c(omega_3S,omega_2S),nrow=1,ncol=2)


Omega_1S <- B_1 %*% Omega %*% t(B_1)
Omega_2S <- B_2 %*% Omega %*% t(B_2)
Omega_3S <- B_3 %*% Omega %*% t(B_3)
c_1 <- omega_1S/omega_SS
c_2 <- omega_2S/omega_SS
c_3 <- omega_3S/omega_SS
k_1 <- mu_1 - (mu_S*omega_1S)/omega_SS
k_2 <- mu_2 - (mu_S*omega_2S)/omega_SS
k_3 <- mu_3 - (mu_S*omega_3S)/omega_SS


mu1 =c_1*M1 + k_1 
mu2 =c_2*M1 + k_2 
mu3 =c_3*M1 + k_3 

sigma_1_2 = Omega_12_S[1,2] + c_1*c_2*Var
sigma_1_1 = Omega_12_S[1,1] + c_1*c_1*Var
sigma_1_3 = Omega_13_S[1,2] + c_1*c_3*Var
sigma_2_1 = Omega_21_S[1,2] + c_2*c_1*Var
sigma_2_2 = Omega_21_S[1,1] + c_2*c_2*Var
sigma_2_3 = Omega_23_S[1,2] + c_2*c_3*Var
sigma_3_1 = Omega_31_S[1,2] + c_3*c_1*Var
sigma_3_2 = Omega_32_S[1,2] + c_3*c_2*Var
sigma_3_3 = Omega_32_S[1,1] + c_3*c_3*Var

sigma_2_1_2 = c_1*(c_2)^2*M3 + 2*c_1*c_2*k_2*Var-c_1*(c_2)^2*M2*M1 + 2*c_2*Omega_12_S[1,2]*M1 + 2*k_2*Omega_12_S[1,2]
sigma_2_1_1 = c_1*(c_1)^2*M3 + 2*c_1*c_1*k_1*Var-c_1*(c_1)^2*M2*M1 + 2*c_1*Omega_12_S[1,1]*M1 + 2*k_1*Omega_12_S[1,1]
sigma_2_1_3 = c_1*(c_3)^2*M3 + 2*c_1*c_3*k_3*Var-c_1*(c_3)^2*M2*M1 + 2*c_3*Omega_13_S[1,2]*M1 + 2*k_3*Omega_13_S[1,2]
sigma_2_2_1 = c_2*(c_1)^2*M3 + 2*c_2*c_1*k_1*Var-c_2*(c_1)^2*M2*M1 + 2*c_1*Omega_21_S[1,2]*M1 + 2*k_1*Omega_21_S[1,2]
sigma_2_2_2 = c_2*(c_2)^2*M3 + 2*c_2*c_2*k_2*Var-c_2*(c_2)^2*M2*M1 + 2*c_2*Omega_21_S[1,1]*M1 + 2*k_2*Omega_21_S[1,1]
sigma_2_2_3 = c_2*(c_3)^2*M3 + 2*c_2*c_3*k_3*Var-c_2*(c_3)^2*M2*M1 + 2*c_3*Omega_23_S[1,2]*M1 + 2*k_3*Omega_23_S[1,2]
sigma_2_3_1 = c_3*(c_1)^2*M3 + 2*c_3*c_1*k_1*Var-c_3*(c_1)^2*M2*M1 + 2*c_1*Omega_31_S[1,2]*M1 + 2*k_1*Omega_31_S[1,2]
sigma_2_3_2 = c_3*(c_2)^2*M3 + 2*c_3*c_2*k_2*Var-c_3*(c_2)^2*M2*M1 + 2*c_2*Omega_32_S[1,2]*M1 + 2*k_2*Omega_32_S[1,2]
sigma_2_3_3 = c_3*(c_3)^2*M3 + 2*c_3*c_3*k_3*Var-c_3*(c_3)^2*M2*M1 + 2*c_3*Omega_31_S[1,1]*M1 + 2*k_3*Omega_31_S[1,1]

SIGMA_S <- matrix(c(sigma_1_1,sigma_1_2,sigma_1_3,sigma_2_1,sigma_2_2,sigma_2_3,sigma_3_1,sigma_3_2,sigma_3_3),nrow=3, ncol=3, byrow=TRUE)
SIGMA_S
beta=0.8
a=solve((8*beta*SIGMA_S+2*diag(3)))
a

mui <- numeric(3)



mui <- c(mu1, mu2, mu3)



sigma_2_i_j <- matrix(c(sigma_2_1_1,sigma_2_1_2,sigma_2_1_3,sigma_2_2_1,sigma_2_2_2,sigma_2_2_3,sigma_2_3_1,sigma_2_3_2,sigma_2_3_3),nrow=3, ncol=3, byrow=TRUE)
sigma_2_i_j



delta <- rep(0, 3)


for (i in 1:3) {
  
  sigma2_sum <- sum(sigma_2_i_j[i,])  
  
 
  delta[i] <- 4 * beta * sigma2_sum + 2 * mui[i]
}
print(sigma2_sum)


cat("\n\\delta_i �?:\n")
print(delta)



numerator <- 25
denominator <- 0
first_sum <- 0
second_sum <- 0
third_sum <- 0

for (k in 1:3) {
  for (l in 1:3) {
    denominator <- denominator + a[k, l]
  }
}
print(denominator)

for (j in 1:3) {
  first_sum <- first_sum + a[3, j]
}
print(first_sum)

for (k in 1:3) {
  for (l in 1:3) {
    third_sum <- third_sum + a[k, l] * delta[l]
  }
}
numerator <- ((25 - third_sum) / denominator) * first_sum


for (j in 1:3) {
  second_sum <- second_sum + a[3, j] * delta[j]
}
print(second_sum)

result <- numerator + second_sum

# 输出结果
cat('The calculation result is', result, '\n')

print(result/25)



