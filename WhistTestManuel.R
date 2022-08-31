food <- read.csv("C:/Users/nsolakoglu/Downloads/food.csv")
summary(food)

plot(food_exp~income, data=food)
plot(food$food_exp~food$income)
attach(food)
plot(food_exp~income)

reg1 <- lm(food_exp~income, data=food)
summary(reg1)

#visual check
res <- residuals(reg1)
food$res_sq <- res^2
food$yhat <- fitted(reg1)
inc_sq <- food$income^2
cor(food$food_exp, food$yhat)^2
cor(food$income, inc_sq)

plot(res_sq~income, data=food, xlab="gelir", ylab="res_sq")

#white test
areg <- lm(res_sq~income, data=food)
summary(areg)

T <- nobs(areg)
rsq <- summary(areg)$r.squared
q <- length(coef(areg))-1
LM <- T*rsq
kikare <- qchisq(0.95, q)
LM; kikare


#install.packages("lmtest")
library(lmtest)

bptest(reg1, ~income, data=food)

gqtest(reg1, order.by=~income, data=food, fraction=5)

# log almak

lf <- log(food$food_exp)
li <- log(food$income)

lreg <- lm(lf~li)
summary(lreg)

bptest(lreg, ~li)
gqtest(lreg, order.by=~li, fraction=5)

#install.packages("sandwich")
library(sandwich)
coeftest(reg1, vcov=vcovHC(reg1, "HC1"))