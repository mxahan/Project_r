## Reading Data, library and loading

library(ISLR)

card <- read.csv("CarPrice_Assignment.csv")
coln <- colnames(card)
print(coln)
summary(card)


## Chappter 2


## Chapter 3

fix(card)
names(card)
attach(card)

lm.fit = lm(price~as.numeric(cylindernumber))

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
