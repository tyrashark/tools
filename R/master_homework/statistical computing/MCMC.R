n <- 10000
sigma <- 2
x <- vector("numeric", n)

# set a proposal disbribution as Normal(0,sigma)
x[1] <- rnorm(1, 0, sigma)
k <- 0

for(i in 2:n){
  xt <- x[i-1]
  y <- rnorm(1, xt, sigma)

  nu <- dnorm(xt, y, sigma)*dcauchy(y, 0, 1)
  den <- dnorm(y, xt, sigma)*dcauchy(xt, 0, 1)
  r <- nu/den
  
  if(runif(1, 0, 1) <= min(r,1)) x[i] <- y
  else{
    x[i] <- xt
    k <- k+1
  }
}


k/n
q <- seq(0.1, 0.9, 0.1)

rbind(quantile(x[5001:10000], probs = q), qcauchy(q, location = 0, scale = 1))




## Gibbs sampler

n <- 10000
X <- matrix(0, n, 2)

rho <- 0.75
mu1 <- 0
mu2 <- 2

sigma1 <- 1
sigma2 <- 0.5


s1 <- sqrt(1-rho^2)*sigma1
s2 <- sqrt(1-rho^2)*sigma2


#chain

X[1,] <- c(mu1, mu2)

for (i in 2:n){
  x2 <- X[i-1, 2]
  m1 <- mu1 + rho * (x2 - mu2) * sigma1 / sigma2
  X[i, 1] <- rnorm(1, m1, s1)
  x1 <- X[i, 1]
  m2 <- mu2 + rho * (x1 - mu1) * sigma2 / sigma1
  X[i, 2] <- rnorm(1, m2, s2)
}

index <- 1001:10000


plot(X[index, 1], X[index, 2])

cov(X[index,])

