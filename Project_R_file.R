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

summary(lm(price~poly(peakrpm,4)+aspiration+carlength+carheight+curbweight+fuelsystem+
             doornumber+wheelbase+
             enginetype+CarName
          , data =card))


lm.fit = lm(price~highwaympg+horsepower+citympg+as.numeric(CarName), data =  card)
summary(lm.fit)


lm.fit1 = lm(price~CarName)
summary(lm(price~horsepower+CarName))


## chapter 4



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

summary(lm(price~carlength,data=card))$coef


summary(lm(price~carlength+carwidth+peakrpm,data=card))$coef



boot.fn=function(data,index)
  coefficients(lm(price~carwidth+I(carwidth^2),data=card,subset=index))
set.seed(1)
boot(card,boot.fn,1000)
summary(lm(price~carwidth+I(carwidth^2),data=card))$coef

boot.fn=function(data,index)
  coefficients(lm(price~carlength+I(carlength^2),data=card,subset=index))
set.seed(1)
boot(card,boot.fn,1000)
summary(lm(price~carlength+I(carlength^2),data=card))$coef

## chapter 6
### Lab 1
dim(card)

card =  na.omit(card)

dim(card)  # this concludes no missin data. son no analysis for the missing data section

sum(is.na(card))

## Choosing the best feature set by BIC, Cp , AIC ...
library(leaps)

regfit.full =  regsubsets(price~fuelsystem+peakrpm+citympg+
                            enginesize+enginetype+carwidth+curbweight+carlength
                          , card) ## full features cause error

summary(regfit.full)

regfit.full = regsubsets(price~fuelsystem+peakrpm+citympg+
                           enginesize+enginetype+carwidth+curbweight+carlength
                         + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                         + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio, 
                         data = card, nvmax = 19)

reg.summary =  summary(regfit.full)

names(reg.summary)

reg.summary$rsq

par(mfrow = c(2,2))

plot(reg.summary$rss, xlab= "number of variables", ylab = "RSS")

plot(reg.summary$adjr2, xlab= "number of variables", ylab = "adjusted  Rsq")

which.max(reg.summary$adjr2) # return 18

points(18, reg.summary$adjr2[18], col ="red", cex = 2, pch =20)

plot(reg.summary$cp, xlab= "number of variables", ylab = "Cp")

which.min(reg.summary$cp) #18

points(18, reg.summary$cp[18], col ="red", cex = 2, pch =20)

which.min(reg.summary$bic) #13

plot(reg.summary$bic, xlab= "number of variables", ylab = "BIC")

points(13, reg.summary$bic[13], col ="red", cex = 2, pch =20)

plot(regfit.full, scale = "r2")

plot(regfit.full, scale = "adjr2")

plot(regfit.full, scale = "Cp")

plot(regfit.full, scale = "bic")

coef(regfit.full, 13)

## Foward and Backward stepwise selection
regfit.fwd  = regsubsets(price~fuelsystem+peakrpm+citympg
                         + enginesize+enginetype+carwidth+curbweight+carlength
                         + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                         + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio, 
                         data= card, nvmax =19, method = "forward")

summary(regfit.fwd)


regfit.bwd  = regsubsets(price~fuelsystem+peakrpm+citympg
                         + enginesize+enginetype+carwidth+curbweight+carlength
                         + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                         + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio, 
                         data= card, nvmax =19, method = "backward")

summary(regfit.bwd)


coef(regfit.full, 7)

coef(regfit.fwd, 7)

coef(regfit.bwd, 7)

## Validation Approach
set.seed(1)

train = sample(c(TRUE, FALSE), nrow(card), rep= TRUE)

test = (!train)

regfit.best  = regsubsets(price~fuelsystem+peakrpm+citympg
                         + enginesize+enginetype+carwidth+curbweight+carlength
                         + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                         + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio, 
                         data= card[train,], nvmax =19)

test.mat =  model.matrix(price~., data = card[test,])

val.errors = rep(NA, 19)

for (i in 1:19){
  coefi = coef(regfit.best, id = i)
  pred =  test.mat[, names(coefi)]%*%coefi
  val.errors[i] =  mean((price[test]-pred)^2)
}

val.errors

which.min(val.errors) # 12

coef(regfit.best, 12)

predict.regsubsets = function(object, newdata, id, ...){
  form  =  as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi =  coef(object, id  =id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi
}

regfit.best =  regsubsets(price~fuelsystem+peakrpm+citympg
                          + enginesize+enginetype+carwidth+curbweight+carlength
                          + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                          + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio, 
                          data= card, nvmax = 19)

coef(regfit.best, 12)

k = 10
set.seed(1)
folds = sample(1:k, nrow(card), replace =TRUE)

cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 5:k){
  best.fit =  regsubsets(price~fuelsystem+peakrpm+citympg
                         + enginesize+enginetype+carwidth+curbweight+carlength
                         + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                         + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio, 
                         data= card[folds!=j,], nvmax = 19)
  for (i in 1:19){
    pred = predict(best.fit, card[folds==j,], id = i)
    cv.errors[j, i]= mean((price[folds==j]-pred)^2)
  }
} # does not work with card[folds!=j]

mean.cv.errors = apply(cv.errors, 2, mean)

mean.cv.errors

par(mfrow = c(1,1))

plot(mean.cv.errors, type = 'b')

reg.best  =  regsubsets(price~fuelsystem+peakrpm+citympg
                        + enginesize+enginetype+carwidth+curbweight+carlength
                        + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                        + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio, 
                        data= card, nvmax = 19)

coef(reg.best, 11)
## part 2 LAB 2

x =  model.matrix(price~fuelsystem+peakrpm+citympg
                        + enginesize+enginetype+carwidth+curbweight+carlength
                        + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                        + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio, 
                        data= card)[-1]


y = price

## ridge regression

library(glmnet)

grid  = 10^seq(10,-2,length=100)

ridge.mod = glmnet(x,y, alpha = 0, lambda = grid)

price


## Chapter 7





## Chapter 8


## Chapter 9  
