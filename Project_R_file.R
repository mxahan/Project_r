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

plot(mean.cv.errors)

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
                        data= card)[,-1]


y = price

## ridge regression

library(glmnet)

grid  = 10^seq(10,-2,length=100)

ridge.mod = glmnet(x,y, alpha = 0, lambda = grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]

ridge.mod$lambda[60]

sqrt(sum(coef(ridge.mod)[-1, 60]^2))

predict(ridge.mod, s = 50, type= "coefficients")[1:20,]

set.seed(1)

train = sample(1:nrow(x), nrow(x)/2)

test = (-train)

y.test = y[test]

ridge.mod =  glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)

ridge.pred =  predict(ridge.mod, s =4, newx = x[test,])

mean((ridge.pred - y.test)^2)

mean((mean(y[train])-y.test)^2)

ridge.pred =  predict(ridge.mod, s = 1e10, newx = x[test,])

mean((ridge.pred-y.test)^2)

ridge.pred = predict(ridge.mod, s =0, newx = x[test,], exact = T)

mean((ridge.pred-y.test)^2)

lm(y~x, subset =  train)

predict(ridge.mod, s=0,  newx = x[test,])[1:20,]

set.seed(1)

cv.out = cv.glmnet(x[train, ], y[train], alpha = 0)

plot(cv.out)

bestlam = cv.out$lambda.min

bestlam

ridge.pred = predict(ridge.mod, s =bestlam, newx = x[test,])

mean((ridge.pred - y.test)^2)

out =  glmnet(x,y, alpha = 0)

predict(out, type = "coefficients", s = bestlam)[1:20,]

### Lasso
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)

plot(lasso.mod)

set.seed(1)

cv.out =  cv.glmnet(x[train,], y[train], alpha=1)

plot(cv.out)

bestlam = cv.out$lambda.min

lasso.pred = predict(lasso.mod, s =bestlam, newx = x[test,])

mean((lasso.pred-y.test)^2)

out = glmnet(x,y, alpha =1, lambda =grid)

lasso.coef = predict(out, type ="coefficients", s= bestlam)[1:20,]

lasso.coef

lasso.coef[lasso.coef != 0]

lasso.coef[lasso.coef!=0]

#### lab 3 part 3

library(pls)

set.seed(2)

pcr.fit = pcr(price~fuelsystem+peakrpm+citympg
              + enginesize+enginetype+carwidth+curbweight+carlength
              + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
              + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
              data = card, scale = TRUE)

summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")

set.seed(1)

pcr.fit = pcr(price~fuelsystem+peakrpm+citympg
              + enginesize+enginetype+carwidth+curbweight+carlength
              + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
              + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
              data = card, scale = TRUE)

summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")

pcr.pred = predict(pcr.fit, x[test,], ncomp =7)

mean((pcr.pred - y.test)^2)

pcr.fit =  pcr(y~x, scale= TRUE, ncomp = 7)

summary(pcr.fit)

set.seed(1)

#partial least square

pls.fit = plsr(price~fuelsystem+peakrpm+citympg
               + enginesize+enginetype+carwidth+curbweight+carlength
               + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
               + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
               data = card,  scale = TRUE)

summary(pls.fit)

validationplot(pls.fit, val.type = "MSEP")

pls.pred = predict(pls.fit, x[test,], ncomp = 2)

mean((pls.pred - y.test)^2)

pls.fit =  plsr(price~fuelsystem+peakrpm+citympg
                + enginesize+enginetype+carwidth+curbweight+carlength
                + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
                data = card,  scale = TRUE, ncomp =2)

summary(pls.fit)

#ch ended


## Chapter 7

# Polynomial regression

fit = lm(price~poly(enginesize, 4), data = card)

coef(summary(fit))


fit2 = lm(price~poly(enginesize, 4, raw=T), data = card)

coef(summary(fit))

fit2a = lm(price~enginesize+I(enginesize^2)+I(enginesize^3), data=card)

coef(fit2a)

fit2b =  lm(price~cbind(enginesize, enginesize^2, enginesize^3))

coef(fit2b)

engsrange =  range(enginesize)

engs.grid =  seq(from=engsrange[1], to = engsrange[2])

preds= predict(fit, newdata = list(enginesize=engs.grid), se=TRUE)

se.bands =  cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se)

#plot
par(mfrow=c(1,2), mar = c(4.5, 4.5,1,1), oma=c(0,0,4,0))
plot(enginesize, price, xlim= engsrange, cex=0.5, col="darkgrey")
title("Degree 3  polynomial", outer=T)
line(engs.grid, preds$fit, lwd=2, col="blue")
matlines(engs.grid, se.bands, lwd=1, col="blue", lty=3)

preds2= predict(fit2, newdata=list(enginesize=engs.grid), se=TRUE)
max(abs(preds$fit- preds2$fit))

fit.1 = lm(price~enginesize, data=card)
fit.2 = lm(price~poly(enginesize,2), data=card)
fit.3 = lm(price~poly(enginesize,3), data=card)
fit.4 = lm(price~poly(enginesize,4), data=card)
fit.5 = lm(price~poly(enginesize,5), data=card)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

coef(summary(fit.5))


fit.1 = lm(price~enginesize+carwidth, data=card)
fit.2 = lm(price~poly(enginesize,2)+carwidth, data=card)
fit.1 = lm(price~poly(enginesize,3)+carwidth, data=card)
anova(fit.1, fit.2, fit.3)


fit = glm(I(price>15000)~poly(enginesize, 4), data=card, family = binomial)

preds = predict(fit, newdata = list(enginesize=engs.grid), se=T)

pfit = exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

preds = predict(fit, newdata = list(enginesize=engs.grid), type="response", se=T)


plot(enginesize, I(price>15000), xlim=engsrange, type ="n", ylim= c(0,0.2))
points(jitter(enginesize), I((price>15000)/5), cex=0.5, pch="|", col="darkgrey")
lines(engs.grid, pfit, lwd=2, col="blue")
matlines(engs.grid, se.bands, lwd = 1, col="blue", lyt=3)


table(cut(enginesize, 4))

fit =lm(price~cut(enginesize, 4), data=card)
coef(summary(fit))

# spline
library(splines)

fit = lm(price~bs(enginesize, knots = c(25,40,60)), data=card)
pred = predict(fit, newdata = list(enginesize=engs.grid), se=T)
plot(enginesize, price, col="grey")
lines(engs.grid, pred$fit, lwd=2)
lines(engs.grid, pred$fit+2*pred$se, ity="dashed")
lines(engs.grid, pred$fit-2*pred$se, ity="dashed")

dim(bs(enginesize, knots=c(25,40,60)))
dim(bs(enginesize, df=6))
attr(bs(enginesize, df=6), "knots")


fit2 = lm(price~ns(enginesize, df=4), data=card)

pred2= predict(fit2, newdata = list(enginesize=engs.grid), se=T)
lines(engs.grid, pred2$fit, col="red", lwd=2)

plot(enginesize, price, xlim= engsrange, cex=0.5, col="darkgrey")
title("smoothing Spline")
fit = smooth.spline(enginesize, price, df=16)
fit2 = smooth.spline(enginesize, price, cv=TRUE)
fit2$df
lines(fit,col="red", lwd=2)
lines(fit2,col="blue", lwd=2)
legend("topright", legend = c("16 DF", "6.8 DF"), col=c("red", "blue"), lty=1, lwd=2, cex=0.8)


plot(enginesize, price, xlim = engsrange, cex=.5, col="darkgrey")
title("local regerssion")
fit = loess(price~enginesize, span=.2, data=card)
fit2 = loess(price~enginesize, span=.5, data=card)
lines(engs.grid, predict(fit, data.frame(enginesize=engs.grid)), col="red", lwd=2)
lines(engs.grid, predict(fit2, data.frame(enginesize=engs.grid)), col="blue", lwd=2)
legend("topright", legend = c("Span 0.2", "Spna 0.5"), col=c("red", "blue"), lty=1, lwd=2, cex=0.8)

#GAM

gam1 = lm(price~ns(carwidth, 4)+ns(enginesize,5)+curbweight, data=card)


### couldn't install gam!!

library(mgcv)  #gam in the book and change the poly
gam.m3 <-gam(price~s(poly(enginesize,4))+carwidth, data=card)

par(mfrow = c(1,3))
plot(gam.m3, se=TRUE, col='blue')

plot.gam(gam1, se=TRUE, col="red")

gam.m1= gam(price~s(poly(enginesize,5))+carwidth, data=card)

gam.m2= gam(price~s(poly(enginesize,5))+carwidth+curbweight, data=card)


anova(gam.m1, gam.m2, gam.m3, test="F")


summary(gam.m3)

preds =predict(gam.m2, newdata=price)

gam.lo= gam(wage~s(enginesize, df=4)+lo(carwidth, span = 0.7)+curbweight, data=card)

plot.gam(gam.lo, se=TRUE, col="green")

gam.lo= gam(wage~lo(enginesize+carwidth, span = 0.7)+curbweight, data=card)

library(akima)

plot(gam.lo.i)

gam.lr = gam(I(price>15000)~carwidth+s(enginesize, df=5)
             +curbweight, family = binomial, data=card)

par(mfrow = c(1,3))

plot(gam.lr, se=T, col="green")

table(curbweight, I(price>15000))

gam.lr.s = gam(I(price>15000)~carwidth+s(enginesize, df=5)
               +curbweight, family = binomial, data=card)

plot(gam.lr.s, se =T, col ="green")

## Chapter 8
library(tree)
high = ifelse(price<=15000, "No", "Yes")

card = data.frame(card, high)

attach(card)

tree.card =  tree(high~fuelsystem+peakrpm+citympg
                  + enginesize+enginetype+carwidth+curbweight+carlength
                  + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                  + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
                  data = card)


summary(tree.card)

plot(tree.card, pretty=0)

tree.card

set.seed(2)

train =  sample(1:nrow(card), 150)
card.test  =  card[-train,]
high.test =  high[-train]
tree.card = tree(high~fuelsystem+peakrpm+citympg
                 + enginesize+enginetype+carwidth+curbweight+carlength
                 + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                 + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
                 data = card, subset=train)

tree.pred = predict(tree.card, card.test, type = "class")

table(tree.pred, high.test)

set.seed(3)

cv.card = cv.tree(tree.card, FUN=prune.misclass)
names(cv.card)
cv.card

par(mfrow= c(1,2))
plot(cv.card$size, cv.card$dev, type="b")
plot(cv.card$k, cv.card$dev, type="b")

prune.card = prune.misclass(tree.card, best=4)
plot(prune.card)
text(prune.card, pretty = 0)
tree.pred = predict(prune.card, card.test, type = "class")
table(tree.pred, high.test)

prune.card = prune.misclass(tree.card, best=3)
plot(prune.card)
text(prune.card, pretty = 0)
tree.pred = predict(prune.card, card.test, type = "class")
table(tree.pred, high.test)

# Fitting regression
set.seed(1)

train = sample(1:nrow(card), nrow(card)/2)
tree.card = tree(price~fuelsystem+peakrpm+citympg
                 + enginesize+enginetype+carwidth+curbweight+carlength
                 + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                 + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
                 data = card, subset=train)

summary(tree.card)

plot(tree.card)
text(tree.card, pretty = 0)

cv.card = cv.tree(tree.card)
plot(cv.card$size, cv.card$dev, type ="b")

prune.card = prune.tree(tree.card, best = 4)
plot(prune.card)
text(prune.card, pretty = 0)

yhat = predict(tree.card, newdata= card[-train,])
card.test = card[-train, "price"]
plot(yhat, card.test)
abline(0,1)
mean((yhat-card.test)^2)

# Bagging and Random forest
library(randomForest)

set.seed(1)
bag.card = randomForest(price~fuelsystem+peakrpm+citympg
                        + enginesize+enginetype+carwidth+curbweight+carlength
                        + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                        + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
                        data = card, subset=train, mtry =13, importance =TRUE)
bag.card

yhat.bag = predict(bag.card, newdata = card[-train,])
plot(yhat.bag, card.test)
abline(0,1)
mean((yhat.bag - card.test)^2)

set.seed(1)
rf.card = randomForest(price~fuelsystem+peakrpm+citympg
                       + enginesize+enginetype+carwidth+curbweight+carlength
                       + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                       + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
                       data = card, subset=train, mtry = 6, importance=TRUE)
yhat.rf = predict(rf.card, newdata = card[-train, ])
mean((yhat.rf - card.test)^2)

importance(rf.card)
varImpPlot(rf.card)

# boosting

library(gbm)
set.seed(1)
boost.card = gbm(price~fuelsystem+peakrpm+citympg
                 + enginesize+enginetype+carwidth+curbweight+carlength
                 + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                 + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
                 data = card[-train,], distribution = "gaussian", n.trees = 5000, 
                 interaction.depth = 4)
summary(boost.card)

par(mfrow = c(1,2))
plot(boost.card, i ="carlength")
plot(boost.card, i= "enginesize")

boost.card = gbm(price~fuelsystem+peakrpm+citympg
                 + enginesize+enginetype+carwidth+curbweight+carlength
                 + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                 + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
                 data = card[-train,], distribution = "gaussian", n.trees = 5000, 
                 interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost =  predict(boost.card, newdata = card[-train,], n.trees = 5000)
mean((yhat.boost - card.test)^2)

## Chapter 9  

set.seed(1)
x= matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1,10), rep(1,10))
x[y==1,] = x[y==1,] + 1
plot(x, col = (3-y))

cardshort = data.frame(x = x, y =as.factor(y))


high = ifelse(price<=15000, 0, 1)
y = high
cutlen = 180 # upto 205


x =  matrix( c(curbweight[1:cutlen], enginesize[1:cutlen]),ncol = 2, nrow  = cutlen) #very important

y = high[1:cutlen]

cardshort = data.frame(x = x, y = as.factor(y))

attach(cardshort)

library(e1071)
svmfit = svm(y~., data = cardshort, kernel = "linear", cost = 100, scale = FALSE)


"
svmfit = svm(high~fuelsystem+peakrpm+citympg
             + enginesize+enginetype+carwidth+curbweight+carlength
             + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
             + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
             data = card, kernel = "linear", cost = 0.1, scale = FALSE)
"


plot(svmfit, cardshort)

svmfit$index

summary(svmfit)
'''
svmfit = svm(high~fuelsystem+peakrpm+citympg
             + enginesize+enginetype+carwidth+curbweight+carlength
             + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
             + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
             data = card, kernel = "linear", cost = 0.1, scale = FALSE)
'''


svmfit = svm(y~., data = cardshort, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit, cardshort)

svmfit$index

set.seed(1)

'''
tune.out = tune(svm,high~fuelsystem+peakrpm+citympg
                + enginesize+enginetype+carwidth+curbweight+carlength
                + highwaympg+ boreratio+ stroke + wheelbase + drivewheel
                + enginelocation+ aspiration+ doornumber+ horsepower+ compressionratio,
                data = card, ranges = list(cost =c(0.001, 0.01, 0.1, 1 ,5 ,10, 100)) )
                
                '''
tune.out = tune(svm,y~.,
                data = cardshort, ranges = list(cost =c(0.001, 0.01, 0.1, 1 ,5 ,10, 100)) )


summary(tune.out)

bestmod = tune.out$best.model
summary(bestmod)

xtest =  matrix(c(curbweight[(cutlen+1):205], enginesize[(cutlen+1):205] ), 
                ncol = 2, nrow  =205- cutlen)
ytest = high[(cutlen+1):205]

cardshorttest = data.frame(x = xtest, y = as.factor(ytest))

ypred = predict(bestmod, cardshorttest)

table(predict = ypred, truth = cardshorttest$y)



svmfit = svm(y~., data = cardshort, kernel = "linear", cost = 1, scale = FALSE)
ypred = predict(svmfit, cardshorttest)

table(predict = ypred, truth = cardshorttest$y)


svmfit = svm(y~., data = cardshort, kernel = "linear", cost = 1e05, scale = FALSE)
summary(svmfit)
plot(svmfit, cardshort)


# SVM

plot(x, col =y)


train = sample(180,100)


svmfit = svm(y~., data = cardshort[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, cardshort[train,])

summary(svmfit)

svmfit = svm(y~., data = cardshort[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, cardshort[train,])

summary(svmfit)


set.seed(1)
tune.out = tune(svm,y~., data = cardshort[train,], kernel = "radial",
                ranges = list(cost =c(0.1, 1, 10, 100, 1000)) )
summary(tune.out)

table(truc = cardshort[-train, "y"], pred = predict(tune.out$best.model, 
                                                    newdata = cardshort[-train,]))


# ROC curve
library(ROCR)

rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)
}

svmfit.opt = svm(y~., data = cardshort[train,], 
                 kernel = "radial", gamma = 2, cost = 1, decision.values =T)


fitted = attributes(predict(svmfit.opt, cardshort[train,], decision.value = T))$decision.values

par(mfrow =c(1,2))

rocplot(fitted, cardshort[train, "y"], main = "Training Data")



svmfit.flex = svm(y~., data = cardshort[train,], 
                 kernel = "radial", gamma = 2, cost = 50, decision.values =T)


fitted = attributes(predict(svmfit.flex, cardshort[train,], decision.value = T))$decision.values

rocplot(fitted, cardshort[train, "y"], add =T, col ="red")



fitted = attributes(predict(svmfit.opt, cardshort[-train,], decision.value = T))$decision.values

rocplot(fitted, cardshort[-train, "y"], add =T, col ="red")


fitted = attributes(predict(svmfit.flex, cardshort[-train,], decision.value = T))$decision.values

rocplot(fitted, cardshort[-train, "y"], add =T, col ="red")

# SVM with Multiclass