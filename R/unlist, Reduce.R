
library(tidyverse)
A <- vector(mode = "list", length = 10000)

for(i in 1:10000){
  A[[i]] <- matrix(rnorm(100), 10, 10)
}


## For
for_mun <- function(A){
  M <- matrix(0, 10, 10)
  for(i in 1:length(A)) M <- M + A[[i]]
  return(M)
}  


for_mun(A)

## Unlist
un_list <- function(A) A %>% unlist %>% matrix(nrow = 100) %>% apply(1, sum) %>% matrix(nrow = 10, ncol = 10)


## Reduce
Reduce("+", A)


library(microbenchmark)

microbenchmark("for mun" = for_mun(A), "unlist" = un_list(A), "Reduce" = Reduce("+", A))
