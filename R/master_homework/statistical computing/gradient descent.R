

# practice 1

## problem : f(x) = (10x_1^2 + x_2^2) / 2 ##

f1 <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  (10*x1^2 + x2^2)/2
}

f1_2 <- function(x1, x2){
  (10*x1^2 + x2^2)/2
}

df1 <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  c(10* x1, x2)
}

mat1 <- outer( seq(-20, 20, length=100),  
              seq(-20, 20, length=100), 
              Vectorize(f1_2) )


contour(x = seq(-20, 20, length=100), y = seq(-20, 20, length=100), mat1)


## gradient descent
par(mfrow = c(2,2))
epsilon <- 1e-4
t <- c(0.01, 0.1, 0.15, 0.2)

for(i in 1:4){
  error <- 1
  x_vec <- c(3, 20)
  k <- 0
  kmat <- c(NULL, NULL)
  while((error > epsilon) & (k < 10000)){
    kmat <- rbind(kmat, x_vec)
    update = t[i]*df1(x_vec)
    x_vec = x_vec - update 
    error = sqrt(sum((update)^2))
    k = k+1
  }
  
  if(k == 10000){
    print(paste("iteration size exceeds limitation"))
  }else{
    print(paste("step size:", t[i]))
    print(paste("iteration number:", k)) 
  }
  contour(x = seq(-20, 20, length=100), y = seq(-20, 20, length=100), mat1)
  points(x = kmat[, 1], y = kmat[, 2], type = "b")  
  
}


## Backtracking line search

par(mfrow = c(2,2))
epsilon <- 1e-4
t <- c(0.01, 0.1, 0.15, 0.2)
alpha <- 0.5
beta <- 0.90


for(i in 1:4){
  t_it <- t[i]
  error <- 1
  x_vec <- c(1, 20)
  k <- 0
  kmat <- c(NULL, NULL)
  while((error > epsilon) & (k < 10000)){
    
    while(f1(x_vec - t_it * df1(x_vec)) > (f1(x_vec) - alpha * t_it * sum((df1(x_vec))^2))){
      t_it <- t_it * beta
    }
    
    kmat <- rbind(kmat, x_vec)
    update = t_it*df1(x_vec)
    x_vec = x_vec - update 
    error = sqrt(sum((update)^2))
    k = k+1
  }
  
  if(k == 10000){
    print(paste("iteration size exceeds limitation"))
  }else{
    print(paste("step size:", t_it))
    print(paste("iteration number:", k)) 
  }
  contour(x = seq(-20, 20, length=100), y = seq(-20, 20, length=100), mat1)
  points(x = kmat[, 1], y = kmat[, 2], type = "b")  
  
}




# practice 2

## Problem : MLE of logistic regression with binomial dist. (exponential dispersion family)

Heart <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Heart.dat", header=T)

library(dplyr)
Heart$x <- recode(Heart$snoring, never=0, occasional=2, nearly_every_night=4, every_night=5)
Heart

fit <- glm(cbind(yes, no) ~ x, family=binomial(link=logit), data=Heart)
summary(fit)



X <- cbind(rep(1, 4), Heart$x)
y <- Heart$yes
n <- Heart$yes + Heart$no

L2 <- function(b){
  sum(y * X%*% b) - sum(n * log(1 + exp(X%*% b)))
}

L2_2 <- function(b0, b1){
  b <- c(b0, b1)
  sum(y * X%*% b) - sum(n * log(1 + exp(X%*% b)))
}

dL2 <- function(b){
  t(X) %*% (y - n * exp(X %*% b) / (1 + exp(X %*% b)))
}


mat2 <- outer( seq(-10, 0, length=100), seq(-5, 5, length=100), Vectorize(L2_2))
contour(x = seq(-10, 0, length=100), y = seq(-5, 5, length=100), mat2)


## gradient descent
par(mfrow = c(2,2))
epsilon <- 1e-6
t <- c(0.0001, 0.001, 0.0015, 0.002)

for(i in 1:4){
  error <- 1
  b <- vector(mode = "numeric", length = 2)
  k <- 0
  kmat <- c(NULL, NULL)
  while((error > epsilon) & (k < 10000)){
    kmat <- rbind(kmat, b)
    update = t[i]*dL2(b)
    b = b + as.vector(update) 
    error = sqrt(sum((update)^2))
    k = k+1
  }
  if(k == 10000){
    print(paste("iteration size exceeds limitation"))
  }else{
    print(paste("step size:", t[i]))
    print(paste("iteration number:", k))
    print(paste("intercept:", b[1], "slope:", b[2]))
  }
  contour(x = seq(-10, 0, length=100), y = seq(-5, 5, length=100), mat2)
  points(x = kmat[, 1], y = kmat[, 2], type = "b")
}
 




## Backtracking line search

par(mfrow = c(2,2))
epsilon <- 1e-6
t <- c(0.0001, 0.001, 0.0015, 0.002)
alpha <- 0.10
beta <- 0.90



for(i in 1:4){
  t_it <- t[i]
  error <- 1
  b <- vector(mode = "numeric", length = 2)
  k <- 0
  kmat <- c(NULL, NULL)
  while((error > epsilon) & (k < 10000)){
    
    while(L2(b + t_it * dL2(b)) < (L2(b) + alpha * t_it * sum((dL2(b))^2))){
      t_it <- t_it * beta
    }
    
    kmat <- rbind(kmat, b)
    update = t_it*dL2(b)
    b = b + as.vector(update) 
    error = sqrt(sum((update)^2))
    k = k+1
  }
  if(k == 10000){
    print(paste("iteration size exceeds limitation"))
  }else{
    print(paste("step size:", t_it))
    print(paste("iteration number:", k))
    print(paste("intercept:", b[1], "slope:", b[2]))
  }
  contour(x = seq(-10, 0, length=100), y = seq(-5, 5, length=100), mat2)
  points(x = kmat[, 1], y = kmat[, 2], type = "b")
}0


## choosing alpha value is matter

