Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=TRUE)


head(Crabs)
install.packages("tidyverse")
library(tidyverse)




## Grouped data

y <- Crabs$y

yes <- table(Crabs$color[Crabs$y ==1])
yes <-as.vector(yes)


no <- table(Crabs$color[Crabs$y ==0])
no <- as.vector(no)
tot <- yes + no
tot

grouped_crab <- data.frame(yes, no, color = as.factor(c(1,2,3,4)))

fit <- glm(cbind(yes, no)~color, family = binomial, data = grouped_crab)
summary(fit)




X <- matrix(0, length(yes), 4)


X[,1] <- rep(1, length(yes))
X[2,2] <- 1
X[3,3] <- 1
X[4,4] <- 1
#X[,5] <- Crabs$width



L <- function(b){
  sum(yes * X %*% b) - sum(tot * log(1 + exp(X %*% b)))
}

solution <- fit$coefficients

L_2 <- function(b_1, b_2, b_3 = solution[-c(1,2)]){
  b <- c(b_1, b_2, b_3)
  sum(yes * X %*% b) - sum(tot * log(1 + exp(X %*% b)))
}



dL <- function(b){
  t(X) %*% (yes - tot * exp(X %*% b) / (1 + exp(X %*% b)))
}


dL_mat <- function(b){
  mat <- matrix(0, ncol(X), nrow(X))
  for(i in 1:nrow(X)) mat[ ,i] <- X[i, ] %*% (yes[i] - tot[i] * exp(X[i, ] %*% b) / (1 + exp(X[i, ] %*% b)))
  return(mat)
}




#mat2 <- outer( seq(-15, -5, length=100), seq(-1, 2, length=100), Vectorize(L_2))
#contour(x = seq(-15, -5, length=100), y = seq(-1, 2, length=100), mat2)


mat2 <- outer( seq(-1, 3, length=100), seq(-2, 1, length=100), Vectorize(L_2))
contour(x = seq(-1, 3, length=100), y = seq(-2, 1, length=100), mat2)

## gradient descent
par(mfrow = c(2,2))
epsilon <- 1e-12
t <- c(0.005, 0.01, 0.015, 0.02)
alpha <- 0.10
beta <- 0.90


for(i in 1:4) {
  t_it <- t[i]
  error <- 1
  b <- vector(mode = "numeric", length = 4)
  k <- 0
  kmat <- c(NULL, NULL)
  L_best <- L(b)
  while((error > epsilon) & (k < 100000)){

   while(L(b + t_it * dL(b)) < (L(b) + alpha * t_it * sum((dL(b))^2))){
     t_it <- t_it * beta
    }
    
    kmat <- rbind(kmat, b)
    update = t_it*dL(b)
    b = b + as.vector(update) 
  
    error = abs(L_best - L(b))
    L_best <- L(b)
    k = k+1
  }
  if(k == 100000){
    print(paste("iteration size exceeds limitation"))
  }else{
    print(paste("step size:", t_it))
    print(paste("iteration number:", k))
    print(paste("intercept:", b[1]))
    print(paste("slope:", b[-1]))
  }
  contour(x = seq(-1, 3, length=100), y = seq(-2, 1, length=100), mat2)
  points(x = kmat[, 1], y = kmat[, 2], type = "b")
}
solution

## stochastic gradient descent
par(mfrow = c(2,2))
epsilon <- 1e-12
t <- c(0.005, 0.01, 0.015, 0.02)
alpha <- 0.10
beta <- 0.90


for(i in 1:4) {
  t_it <- t[i]
  error <- 1
  b <- vector(mode = "numeric", length = 4)
  k <- 0
  kmat <- c(NULL, NULL)
  L_best <- L(b)
  while((error > epsilon) & (k < 10000)){
    
    
    kmat <- rbind(kmat, b)
    random <- sample(1:nrow(X), 2, replace = F)
    
    update = t[i]*apply(dL_mat(b)[, random], 1, sum)
    b = b + as.vector(update) 
    
    
    error = abs(L_best - L(b))
    L_best <- L(b)
    k = k+1
    
    t_it <- t[i] / k
  }
  if(k == 100000){
    print(paste("iteration size exceeds limitation"))
  }else{
    print(paste("step size:", t_it))
    print(paste("iteration number:", k))
    print(paste("intercept:", b[1]))
    print(paste("slope:", b[-1]))
  }
  contour(x = seq(-1, 3, length=100), y = seq(-2, 1, length=100), mat2)
  points(x = kmat[, 1], y = kmat[, 2], type = "b")
}

solution


## Ungrouped data


y <- Crabs$y
n <- length(y)


fit_un <- glm(y~ as.factor(color) + scale(width), family = binomial, data = Crabs)
summary(fit_un)



solution_un <- fit_un$coefficients

X_un <- matrix(0, n, 5)


X_un[,1] <- rep(1, n)
X_un[,2][Crabs$color == 2] <-  1
X_un[,3][Crabs$color == 3] <-  1
X_un[,4][Crabs$color == 4] <-  1
X_un[,5] <-scale(Crabs$width)
solution_un

L_un <- function(b){
  sum(y * X_un %*% b - log(1 + exp(X_un %*% b)))
}


L_un_2 <- function(b_1, b_2, b_3 = solution_un[-c(1,2)]){
  b <- c(b_1, b_2, b_3)
  sum(y * X_un%*% b - log(1 + exp(X_un %*% b)))
}

dL_un <- function(b){
  t(X_un) %*% (y - exp(X_un %*% b) / (1 + exp(X_un %*% b)))
}


dL_mat_un <- function(b){
  mat <- matrix(0, ncol(X_un), nrow(X_un))
  for(i in 1:nrow(X_un)) mat[ ,i] <- X_un[i, ] %*% (y[i] - exp(X_un[i, ] %*% b) / (1 + exp(X_un[i, ] %*% b)))
  return(mat)
}


solution_un

#mat2 <- outer( seq(-15, -5, length=100), seq(-1, 2, length=100), Vectorize(L_2))
#contour(x = seq(-15, -5, length=100), y = seq(-1, 2, length=100), mat2)

mat2 <- outer( seq(-1, 2, length=100), seq(-1, 2, length=100), Vectorize(L_un_2))
contour(x = seq(-1, 2, length=100), y = seq(-1, 2, length=100), mat2)

## gradient descent
par(mfrow = c(2,2))
epsilon <- 1e-12
t <- c(0.005, 0.01, 0.015, 0.02)
alpha <- 0.10
beta <- 0.90


for(i in 1:4) {
  t_it <- t[i]
  error <- 1
  b <- vector(mode = "numeric", length = 5)
  k <- 0
  kmat <- c(NULL, NULL)
  while((error > epsilon) & (k < 10000)){
    
    while(L_un(b + t_it * dL_un(b)) < (L_un(b) + alpha * t_it * sum((dL_un(b))^2))){
      t_it <- t_it * beta
    }
    
    kmat <- rbind(kmat, b)
    update = t_it*dL_un(b)
    b = b + as.vector(update) 

    error = abs(L_best - L_un(b))
    L_best <- L_un(b)

    k = k+1
  }
  
  
  if(k == 20000){
    print(paste("iteration size exceeds limitation"))
  }else{
    print(paste("step size:", t_it))
    print(paste("iteration number:", k))
    print(paste("intercept:", b[1]))
    print(paste("slope:", b[-1]))
  }
  contour(x = seq(-1, 3, length=100), y = seq(-2, 1, length=100), mat2)
  points(x = kmat[, 1], y = kmat[, 2], type = "b")
}

solution_un


## Stochastic gradient descent
par(mfrow = c(2,2))
epsilon <- 1e-12
t <- c(0.005, 0.01, 0.015, 0.02)
alpha <- 0.10
beta <- 0.90

for(i in 1:4) {
  t_it <- t[i]
  error <- 1
  b <- vector(mode = "numeric", length = 5) 
  k <- 0
  kmat <- c(NULL, NULL)
  L_best <- L_un(b)
  while((error > epsilon) & (k < 10000)){
    
  # while(L_un(b + t_it * dL_un(b)) < (L_un(b) + alpha * t_it * sum((dL_un(b))^2))){
  #    t_it <- t_it * beta
  #  }
    
    kmat <- rbind(kmat, b)
    
    random <- sample(1:nrow(X_un), 150, replace = F)
    update <- t_it*apply(dL_mat_un(b)[, random], 1, sum)

    b = b + as.vector(update) 
    error = abs(L_best - L_un(b))
    L_best <- L_un(b)
    k = k+1
    
   if (k >500){
     t_it <- t[i]/k *10
    }
    
  }
  if(k == 100000){
    print(paste("iteration size exceeds limitation"))
  }else{
    print(paste("step size:", t_it))
    print(paste("iteration number:", k))
    print(paste("intercept:", b[1]))
    print(paste("slope:", b[-1]))
  }
  contour(x = seq(-1, 2, length=100), y = seq(-1, 2, length=100), mat2)
  points(x = kmat[, 1], y = kmat[, 2], type = "b")
}
solution_un


