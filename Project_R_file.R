## Reading Data, library and loading

library(ISLR)

card <- read.csv("CarPrice_Assignment.csv")
coln <- colnames(card)
print(coln)
summary(card)


## Chappter 2


## Chapter 3

#
#card$classifier = as.factor(price>15000)

fix(card)
names(card)
attach(card)

lm.fit = lm(price~as.numeric(cylindernumber))
summary(lm.fit)

lm.fit = lm(price~horsepower)

lm.fit

names(lm.fit)

coef(lm.fit)

confint(lm.fit)

summary(lm.fit)

predict(lm.fit,data.frame(horsepower=(c(5,10,15))), interval="confidence")

predict(lm.fit,data.frame(horsepower=(c(5,10,15))), interval="prediction")

plot(horsepower, price)

lm.fit = lm(price~carlength+horsepower)
lm.fit
plot(carlength, price)


lm.fit = lm(price~., data =  card)
summary(lm.fit)


lm.fit = lm(price~highwaympg+horsepower+citympg+as.numeric(CarName), data =  card)
summary(lm.fit)


lm.fit1 = lm(price~CarName)
summary(lm(price~horsepower+CarName))


## chapter 5

# validation set approach

library(ISLR)
set.seed(1)
attach(card)

train =  sample(205, 190)

lm.fit = lm(price~peakrpm+carlength+fueltype+carbody, data = card, subset= train)

summary(lm.fit)

mean((card$price-predict(lm.fit,card))[-train]^2)


# preparing quadratic regression
lm.fit2 = lm(price~poly(peakrpm,2)+poly(carlength,2)+fueltype+carbody, data = card, subset= train)

summary(lm.fit2)
# Prediction with rest
mean((card$price-predict(lm.fit2,card))[-train]^2)

# preparing cubic regression
lm.fit3 = lm(price~poly(peakrpm,2)+poly(carlength,3)+fueltype+carbody, data = card, subset= train)

summary(lm.fit3)
# Prediction with rest
mean((card$price-predict(lm.fit3,card))[-train]^2)

# Leave one-out-cross validation
# used all continuous value predictor
glm.fit =  glm(price~peakrpm+carlength+fueltype+carbody, data=card)
coef(glm.fit)

#Library
library(boot)
glm.fit=  glm(price~peakrpm+carlength+fueltype+carbody, data=card)

cv.err = cv.glm(card, glm.fit)
cv.err$delta


# Polynomial 

cv.error = rep(0,5)

for (i in 1:5){
  glm.fit =  glm(price~poly(carlength, i)+carwidth+carbody+fueltype+peakrpm, data=card)
  cv.error[i] = cv.glm(card, glm.fit)$delta[1]
}
cv.error

# k fold cross-validation

set.seed(20)

cv.error.10 = rep(0,10)

for (i in 1:10){
  glm.fit = glm(price~poly(carlength, i)+carwidth+carbody+fueltype+peakrpm, data=card)
  cv.error.10[i] = cv.glm(card, glm.fit, K = 10)$delta[1]
}
cv.error.10

#Bootstrap


alpha.fn=function(data,index){
  X=data$carlength[index]
  Y=data$price[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(card,1:100)
set.seed(1)
alpha.fn(card,sample(100,100,replace=T))
boot(card,alpha.fn,R=1000)


boot.fn=function(data,index)
  return(coef(lm(price~carlength+carwidth+peakrpm,data=card,subset=index)))

boot.fn(card, 1:203)


set.seed(1)

boot.fn(card,sample(205,205,replace=T))

boot.fn(card,sample(205, 205,replace=T))

boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef


