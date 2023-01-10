

Gelman.Rubin <- function(psi){
  
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  
  
  
  psi.means <-apply(psi, 1, mean) 
  #between sequence variance
  B <- n*var(psi.means)
  
  psi.w <- apply(psi, 1, var)
  W <- mean(psi.w)
  v.hat <- W*(n-1)/n + B/n
  r.hat <- v.hat / W
  return(r.hat)
}

normal.chain <- function(sigma, N, X1){
  x <- vector("numeric", N)
  x[1] <- X1
  u <- runif(N)
  
  for(i in 2:N){
    xt <- x[i-1]
    
    y <- rnorm(1, xt, sigma)  ##proposal distribution
    
    nu <- dnorm(xt, y, sigma) * dnorm(y, 0, 1)
    dem <- dnorm(y, xt, sigma) * dnorm(xt, 0, 1)
    r <- nu/dem
    
    if(u[i] <= min(r, 1) ) x[i] <- y
    else{
      x[i] <- xt
    }
  }
  return(x)
}





sigma <- .2
k <- 4
n <- 15000
b <- 1000


# choose overdispersed initial values
x0 <- c(-10, -5, 5, 10)

# generate the chains
X <- matrix(0, nrow = k, ncol = n)

X
for(i in 1:k){
  X[i,] <- normal.chain(sigma, n, x0[i])
}

#Compute diagnostic statistics

psi <- t(apply(X, 1, cumsum))
psi
for(i in 1:nrow(psi)){
  psi[i, ] <- psi[i, ] / (1:ncol(psi))

}
print(Gelman.Rubin(psi))


# plot psi for the four chains
par(mfrow = c(2,2))
for(i in 1:k){
  plot(psi[i, (b+1):n], type = "l", xlab = i, ylab = bquote(psi))
}
dim(psi)

par(mfrow = c(1,1))

rhat <- rep(0, n)
for(j in (b+1):n){
  rhat[j] <- Gelman.Rubin(psi[, 1:j])
}

plot(rhat[(b+1):n], type = "l", xlab = "", ylab = "R")
abline(h=1.1, lty=2)
