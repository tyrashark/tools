
## conditional Odds

library(tidyverse)
students <- c(c(512, 313, 89,19), c(353, 207, 17, 8),
              c(120, 205, 202, 391), c(138, 279, 131, 244),
              c(53, 138, 94, 299), c(22, 351, 24, 317))
stu_yes_no <- matrix(students, 12, 2, byrow = T)
gender <- rep(rep(c("Male", "Female"), each = 1), 6)
department <- rep(c("A", "B", "C", "D", "E", "F"), each = 2)
df1 <- data.frame(as.factor(department), as.factor(gender), stu_yes_no)
colnames(df1) <- c("department", "gender", "yes","no")
df1 <- as_tibble(df1)
df1


cond_or <- function(x, D) {
  require(tidyverse)
  OR <- function(y, D) {
    or <- y[1,1] * y[2,2] / (y[1,2] * y[2,1])
    names(or) <- paste0("Odds ratio given Department = ", D)
    return(or)
  }
  x %>% filter(department == D) %>%
    select(!department & !gender) %>% as.matrix %>% OR(D)
}
for(i in 1:6) print(cond_or(df1, unique(department)[i]))


## Marginal Odds
df1 %>% group_by(gender) %>%
  summarise(yes = sum(yes), no = sum(no))
print(paste("AG Marginal odds ratio:",(1198 * 1278) / (557 * 1493)))



## glm

fit1 <- glm(cbind(yes, no) ~ gender + department ,
            family = binomial, data = df1)
summary(fit1)

genderMale <- fit1$coefficients[2]
print(paste("AG conditional odds ratio:", exp(genderMale)))


pearson_residual <- resid(fit1, type = "pearson")
### Test H_0: the model fit the data
sum(pearson_residual^2) > qchisq(0.05, df = 5, lower.tail = F)



## Data prep
M2 <- matrix(c(6,3,10,13) ,2,2)
M3 <- matrix(c(7,3,4,7) ,2,2)
M <- rbind(M2, M3)
M <- cbind(rep(c(2, 3), each = 2), M)
colnames(M) <- c("Z", "Yes", "No")
df <- as.data.frame(M)

## CMH
cmh <- function(x) {
  require(tidyverse)
  Z_2 <- c(2, 3)
  mu_n11k <- function(y) apply(y, 1, sum)[1] *
    apply(y, 2, sum)[1] / sum(y)
  deviations_n11k <- function(y) (y[1,1]- mu_n11k(y))
  variances_n11k <- function(y) prod(apply(y, 1, sum)) *
    prod(apply(y, 2, sum)) / (sum(y)^2 *(sum(y) - 1))
  temp1 <-0
  temp2 <-0
  for(k in 1:length(Z_2)){
    temp1 <- temp1 + x %>% filter(Z == Z_2[k]) %>%
      select(!Z) %>% deviations_n11k
    temp2 <- temp2 + x %>% filter(Z == Z_2[k]) %>%
      select(!Z) %>% variances_n11k
  }
  result <- temp1^2/temp2
  names(result) <- "CMH statistic"
  return(result)
}
cmh(df)
cmh(df) > qchisq(0.05, df=1, lower.tail = F)



## MH


### (ii)
mh <- function(x){
  cond_odds1 <- function(y) y[1,1] * y[2,2] / sum(y)
  cond_odds2 <- function(y) y[1,2] * y[2,1] / sum(y)
  temp1 <-0
  temp2 <-0
  for(k in 1:length(unique(penicillin))){
    temp1 <- temp1 + x %>%
      filter(penicillin == unique(penicillin)[k]) %>%
      select(!penicillin & !delay) %>% cond_odds1
    temp2 <- temp2 + x %>%
      filter(penicillin == unique(penicillin)[k]) %>%
      select(!penicillin & !delay) %>% cond_odds2
  }
  result <- temp1/temp2
  names(result) <- "MH estimate"
  return(result)
}


print(paste("MH estimate:", mh(df_4)))



## vglm
library(VGAM)

vglm(cbind(y.1, y.2, y.3) ~ factor + +, cumulative(parallel = T), data = )
