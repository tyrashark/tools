#1.22
n <- c(10, 50, 100, 500)

theta = 2

for(i in 1:4){
x <- rbeta(n[i], shape1 = theta, shape2 = 1)
  
post_theta <- rgamma(1000, shape = n[i]+1, rate = 1-sum(log(x)))
  
print(paste0("n=", i))
print(mean(post_theta))
print(var(post_theta))
}

#1.23
n <- c(10, 50, 100, 500)

theta = 2

for(i in 1:4){
  x <- matrix(0, nrow = n[i], ncol = 1000)
  for(j in 1:1000){
    x[,j] <- rbeta(n[i], shape1 = theta, shape2 = 1)
  }
  mle_theta <- apply(x, 2, FUN = function(x) -n[i]/sum(log(x)))
  
  print(paste0("n=", i))
  print(mean(mle_theta))
  print(var(mle_theta))
}



#1.42

x <- matrix(1:8, nrow = 8, ncol = 10000, byrow = F)
sample <- apply(x, 2, FUN = function(y) sapply(y, FUN = function(i) rnorm(1, mean = 0, sd = (9-i)^2)))

Sk <- apply(sample, 2, cumsum)

Sn <- apply(sample, 2, sum)

a = 12
max_Sk <- apply(abs(Sk), 2, max)

a^2 * mean(max_Sk > a)
var(Sn)
