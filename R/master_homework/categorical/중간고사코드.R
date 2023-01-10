Heart <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Heart.dat", header=T)
library(dplyr)
Heart$x <- recode(Heart$snoring, never=0, occasional=2, nearly_every_night=4, every_night=5)
n <- Heart$yes + Heart$no
fit <- glm(yes/n ~ x, family=binomial(link = probit), weights=n, data=Heart)
fit

summary(fit)


Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=T)
head(Crabs, 3)

fit <- glm(sat ~ width, family=poisson(link=log), data=Crabs)
summary(fit)



d <- matrix(c(17, 62, 76, 34, 137, 108, 283, 779, 375), 3, 3)
d
sum(d)
pi_age <- apply(d, 1, sum) / sum(d)
pi_income <- apply(d, 2, sum) / sum(d)

ex_d <- pi_age %*% t(pi_income) * sum(d) 
ex_d

sum((d - ex_d)^2 / ex_d)


sum((1 - pi_age) %*% t(1-pi_income))


2 * sum(d * log(d / ex_d))


(d- ex_d) / sqrt(ex_d * (1 - pi_age) %*% t(1-pi_income))


z <- qnorm(0.975)
or1 <- (17 * 137) / (34 * 62)
log(or1)


nse_1 <- sqrt(sum(1/c(17, 137, 34, 62)))
log(or1) + z * nse_1 
log(or1) - z * nse_1

or2 <- (34 * 779) / (283 * 137)

nse_2 <- sqrt(sum(1/c(34, 779, 283, 137)))
log(or2) + z * nse_2 
log(or2) - z * nse_2


or3 <- (62 * 108) / (137 * 76)

nse_3 <- sqrt(sum(1/c(62, 108, 137, 76)))
log(or3) + z * nse_3 
log(or3) - z * nse_3

or4 <- (137 * 375) / (779 * 108)
nse_4 <- sqrt(sum(1/c(137, 375, 779, 108)))
log(or4) + z * nse_4 
log(or4) - z * nse_4

