library(ISLR)
library(dplyr)
library(boot)
RNGkind(sample.kind = 'Rounding')
head(Portfolio)

alpha.fn <- function(data,index){
  X<-data$X[index]
  Y<-data$Y[index]
  result <- round((var(Y)-cov(X,Y)) /(var(X)+var(Y)-2*cov(X,Y)),3)
  return (result)
}

alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(100,100, replace = T))
set.seed(1)
boot(Portfolio, alpha.fn, R=1000)

boot.fn<- function(data,index){
  coef_val <- coef(lm(mpg~horsepower, data=data, subset=index))
  options(digits=3)
  return (coef_val)
}
set.seed(1)
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392,392,replace = T))
# compare the bootstrap method with linear regression method.
# The results is different. And bootstrap mode is better as no assumption made.

boot(Auto,boot.fn, 1000)
summary(lm(data=Auto, mpg~horsepower))$coef

# Using bootstrap with linear regression poly level2
# The result is same. As poly linear level2 has good fit.

boot.fn <- function(data, index){
  
  coe_val <- coefficients(
    lm(data=Auto, 
       mpg~horsepower + I(horsepower^2),
       subset=index)
    )
  return (coe_val)
}

set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(data=Auto, mpg~horsepower + I(horsepower^2)))$coef
