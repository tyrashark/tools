## Optimization

LL <- function(lambda, x){
  lambda3 <- 1 - sum(lambda)
  f1 <- dgamma(x, shape = 1/2, rate = 1/(2*lambda[1]))
  f2 <- dgamma(x, shape = 1/2, rate = 1/(2*lambda[2]))
  f3 <- dgamma(x, shape = 1/2, rate = 1/(2*lambda3))
  f <- (f1 + f2 + f3)/3
  
  # returnin -loglikelihood
  return(-sum(log(f)))
}

set.seed(543)
m <- 2000

lambda <- c(0.6, 0.25, 0.15) 

lam <- sample(lambda, size = m, replace = TRUE)

x<- rgamma(m, shape = 0.5, rate = 1/(2*lam))

opt <- optim(c(0.5, 0.3), LL, x=x)

theta <- c(opt$par, 1- sum(opt$par))

theta
opt

## EM algorithm

N <- 10000
## initial est. for lambdas
L <- c(.5, .4, .1)
tol <- .Machine$double.eps^0.5

L.old <- L + 1

L.old

for(j in 1:N){
  f1 <- dgamma(x, shape = 1/2, rate = 1/(2*L[1]))
  f2 <- dgamma(x, shape = 1/2, rate = 1/(2*L[2]))
  f3 <- dgamma(x, shape = 1/2, rate = 1/(2*L[3]))
  
  post1 <- f1 /(f1+f2+f3)
  post2 <- f2 /(f1+f2+f3)
  post3 <- f3 /(f1+f2+f3)
  
  # calculate MLE
  mu1 <- sum(x*post1) / sum(post1)
  mu2 <- sum(x*post2) / sum(post2)
  mu3 <- sum(x*post3) / sum(post3)
  L <- c(mu1, mu2, mu3)
  L <- L/sum(L)
  
  if (sum(abs(L - L.old)/L.old) < tol) break
  L.old <- L
}

print(list(lambda = L/sum(L), iter = j, tol = tol))

