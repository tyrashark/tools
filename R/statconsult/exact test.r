# Simulated data
y <- c(rpois(50, lambda = 0) + 9,  9 +rpois(50, lambda = 0.2), rpois(50, lambda = 0.2)+9)
loc <- rep(1:3, each = 50)
df <- data.frame(y = y, loc = as.factor(loc)) # put a dataframe with real data instead of simulated data

# Contingency table
mat <-t(table(df))
mat

?fisher.test
fisher.test(mat)
fisher.test(mat, simulate.p.value = T)

library(RVAideMemoire)
RVAideMemoire::
multinomial.multcomp(mat)
