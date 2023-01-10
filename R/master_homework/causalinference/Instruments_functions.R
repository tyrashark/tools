if("tidyverse" %in% rownames(installed.packages()) == FALSE)
{install.packages("tidyverse")
}
library("tidyverse")


## Data generating functions

data.gen <- function(n, rho, sd=1, alpha, K=1, integer.W = FALSE, covariate=FALSE){
  
  if(length(alpha)!=K){
    print("K is needed to be the length of alpha")
    break
  }
  
  epsilon = rnorm(n, sd = sd)
  eta = rho^2 * epsilon + rnorm(n, sd = sd*sqrt(1-rho^2))
  Z = sample(0:K, size = n, replace = T)
  X = 0
  
  if(covariate == TRUE){
    X = sample(c(1,2,3,4,5), size = n, replace = T)
  }
  
  ZS = 0
  for(j in 1:K) ZS = ZS + alpha[j]*(Z==j)
  
  W = alpha0 + ZS + X + eta
  
  if(integer.W == TRUE){
    W = round(W, 0)
  }
  
  Y = beta0 + beta1*W + epsilon
  return(list(Y=Y, W=W, Z=Z, X=X))
}


data.gen.1 <- function(n, rho, sd=1, K=1, covariate=FALSE){
  epsilon = rnorm(n, sd = sd)
  eta = rho^2 * epsilon + rnorm(n, sd = sd*sqrt(1-rho^2))
  Z = sample(0:K, size = n, replace = T)
  X = 0
  if(covariate == TRUE){
    X = sample(c(1,2,3,4,5), size = n, replace = T)
  }
  W = alpha0 + alpha1 * Z + X + eta
  W = round(W,0)
  Y = beta0 + beta1*W + epsilon
  return(list(Y=Y, W=W, Z=Z, X=X))
}


data.gen.2 <- function(n, rho, sd=1, K=1, covariate=FALSE){
  epsilon = rnorm(n, sd = sd)
  eta = rho^2 * epsilon + rnorm(n, sd = sd*sqrt(1-rho^2))
  Z = sample(0:K, size = n, replace = T)
  X = 0
  if(covariate == TRUE){
    X = sample(c(1,2,3,4,5), size = n, replace = T)
  }
  W = alpha0 + X + alpha1 * Z + eta
  W = round(W, 0)
  Y = beta0 + beta1*W + epsilon
  return(list(Y=Y, W=W, Z=Z, X=X))
}

## Estimation methods (Wald estimators)

beta.k <- function(df, K){
  beta = c()
  for(k in 1:K){
    temp = (df.sum[df.sum["Z"]==k,"Ybar"] %>% unlist() - df.sum[df.sum["Z"]==(k-1),"Ybar"] %>% unlist()) / 
      (df.sum[df.sum["Z"]==k,"Wbar"] %>% unlist() - df.sum[df.sum["Z"]==(k-1),"Wbar"] %>% unlist())
    beta = c(beta, temp)
  }
  names(beta) = paste0("beta", 1:K, ",",0:(K-1))
  return(beta)
}

mu.k <- function(df, K){
  mu = c()
  E.W = df.sum[df.sum["Z"]==0,"Wbar.T"] %>% unlist()
  sum.pi.E.E = 0
  for (l in 0:K){
    pi.l = df.sum[df.sum["Z"]==l,"Pi"] %>% unlist()
    E.W.Zl = df.sum[df.sum["Z"]==l,"Wbar"] %>% unlist()
    prod.pi.E.E = pi.l * E.W.Zl * (E.W.Zl - E.W)
    sum.pi.E.E = sum(sum.pi.E.E, prod.pi.E.E)
  }
  for(k in 1:K){
    E.W.Zk = df.sum[df.sum["Z"]==k,"Wbar"] %>% unlist()
    E.W.Zk_1 = df.sum[df.sum["Z"]==k-1,"Wbar"] %>% unlist()
    temp = (E.W.Zk - E.W.Zk_1)
    weight = pi.k(k, K=K, E.W=E.W) / sum.pi.E.E
    mu = c(mu, temp*weight)
  }
  names(mu) = paste0("mu", 1:K, ",", 0:(K-1))
  return(mu)
}

pi.k <- function(k, K, E.W){
  sum.pi.E = 0
  for (l in k:K){
    pi.l = df.sum[df.sum["Z"]==l,"Pi"]%>%unlist()
    E.W.Zl = df.sum[df.sum["Z"]==l,"Wbar"]%>%unlist()
    prod.pi.E = pi.l * (E.W.Zl - E.W)
    sum.pi.E = sum(sum.pi.E, prod.pi.E)
  }
  return(sum.pi.E)
}

empirical.cdf.W <- function(df, z){
  
  minW = round(min(df["W"]), 0)
  maxW = round(max(df["W"]), 0)
  df.Z = df %>% filter(Z==z) %>% select(W) %>% round(0)
  
  cdf.W = c()
  for(j in (minW+1):maxW){
    temp = mean(df.Z < j)
    cdf.W = c(cdf.W, temp)
  }
  names(cdf.W) = paste0("j=", (minW+1):maxW)
  return(cdf.W)
}
