# Dataset


data=read.table("prostate.txt")

#install.packages("glmnet")
library(glmnet)

# Q2 ------------------------------------------------
data


tr.X = data[data[, "train"], ][,1:8]
tr.y = data[data[, "train"], ][,9]

tr.Xs = scale(tr.X, center = T, scale = T)
tr.ys = scale(tr.y, center = T, scale = F)


n = nrow(tr.Xs)

y <- as.matrix(tr.ys)
X <- cbind(rep(1, n), as.matrix(tr.Xs))

## Lasso solution
lasso <- glmnet(X, y, alpha = 1, lambda = c(0.1, 0.25, 0.5, 1))

coef(lasso, s = c(0.1, 0.25, 0.5, 1))


L <- function(b, lambda){
  0.5 * t(y - X %*% b) %*% (y - X %*% b) + lambda * sum(abs(b))
}

b <- vector("numeric", length = 9)

L(b, 1)

sgn <- function(b, op = 0){
  b_sign <- vector("numeric", length(b))
  for (j in 1:length(b)){
    if(b[j] > 0){ 
      b_sign[j] = 1
    }else if(b[j] < 0){
      b_sign[j] = -1
    }else{
      b_sign[j] = op
    }
  }
  return(b_sign) 
}

Lg <- function(b, lambda, op = 0){
  -t(X) %*% (y - X %*% b) + lambda * sgn(b, op)
}


Lg(as.vector(coef(lasso, s = c(0.1)))[-2], 0.1)



B <- cbind(rep(0, length=100), seq(-2, 2, length=100),  seq(-2, 2, length=100), rep(0, length=100), rep(1.058062e-01, length=100), rep(1.722525e-01, length = 100), rep(0, length = 100), rep(0, 100), rep(6.583079e-02, 100))

mat2 <- outer(X = B, Y = rep(lambda, 100), Vectorize(L))
contour(x = seq(-10, 0, length=100), y = seq(-5, 5, length=100), mat2)


## subgradient descent
par(mfrow = c(2,2))
epsilon <- 1e-9
t <- c(0.001, 0.005, 0.01, 0.02)
lambda <- 0.1
op <- 0


for(i in 1:4){
  error <- 1
  beta <- vector(mode = "numeric", length = 9)
  best_L <- L(beta, lambda)
  k <- 0
  kmat <- c(NULL, NULL)
  while((error > epsilon) & (k < 100000)){
    
    t_it <- 1/(k+1) * t[i]
    kmat <- rbind(kmat, beta)
    update = t_it*Lg(beta, lambda, op)
    
    beta = beta - as.vector(update) 
    error = sqrt(sum((best - beta)^2))
    
    if(L(beta, lambda) < best_L){
      best_L = L(beta, lambda)
      best <- beta
    }
    k = k+1
  }
  if(k == 100000){
    print(paste("iteration size exceeds limitation"))
  }else{
    print(paste("step size:", t_it))
    print(paste("iteration number:", k))
    print(c(paste("intercept:", best[1]), paste( "slope:", best[-1])))
  }
  #contour(x = seq(-10, 0, length=100), y = seq(-5, 5, length=100), mat2)
  #points(x = kmat[, 1], y = kmat[, 2], type = "b")
}


