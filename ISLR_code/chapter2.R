library(ISLR)  # making all dataset available

x <- c(1,6,2)
y <- c(1,4,3)

x-y       #x+y, x*y

length(x) # length(y)

ls() # check existing variables

rm(x,y) # remove variables

rm(list=ls())


##
x <- matrix(data=c(1,2,3,4), nrow=2, ncol=2)


matrix(c(1,2,3,4),2,2, byrow=TRUE)

x^2
sqrt(x)

x <- rnorm(50)
y <- x + rnorm(50, mean=50, sd=1)
cor(x,y)

###

set.seed(1303)
rnorm(50)

set.seed(3)
y <- rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)


##### END OF SECTION 1 ######

##### SECTION 2: Graphics ######


x <- rnorm(100)
y <- rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x-axis", ylab="this is the y-axis", main="x vs y")

#To save the image of the plots we create, there are several options
pdf("figure 1: basic ass scatterplot.pdf")
plot(x,y,col="green")
dev.off() #This function tells R that we are done creating the plot

#seq(a,b,length=.) is a function that allows us to generate a sequence of numbers between a and b with a specified length
x <- seq(1,10)
x <- 1:10 #a:b is shorthand for seq(1,10)
x <- seq(-pi, pi, length=50)

#Let's try to graph three-dimensional data using contour()
y <- x 

f <- outer(x,y,function(x,y) cos(y)/(1+x^2)) #We write a cosine function whose domain is x,y and range is f
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)

fa=(f-t(f))/2 #Transpose our range matrix and transform it
contour(x,y,fa,nleveles=15)

#image() works in the same way, except that it adds color (much like a heatmap)
image(x,y,fa)

#persp() will give us a three-dimensional plot, with options theta and phi to rotate the viewing angles
persp(x,y,fa)
persp(x,y,fa, theta=30)
persp(x,y,fa, theta=30, phi=20)
persp(x,y,fa, theta=30, phi=40)
persp(x,y,fa, theta=30, phi=70)

##### END OF SECTION 2 #####

##### SECTION 3: Loading and Indexing Data #####
A <- matrix(1:16, 4,4)

#Square brackets are used for indexing and selecting subsets of data
#The first number in the bracket refers to the row, and the second number refers to the column
#In order to grab an entire row or an entire column, use a comma in place of a number
#A negative sign indicates to R that everything but the listed numbers will be grabbed

A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[,2]
A[2,]
A[-c(1,3),]

#dim() outputs the number of rows followed by the number of columns
dim(A)

#We begin by loading the Auto dataset taken from the ISLR website; it is saved in csv format so we must use read.csv()
auto <- Auto
fix(auto) #quick visual inspection
dim(auto) #get an idea of how large this dataset is
names(auto) #check out the names of the variables in this data

#Deal with missing observations by omitting them
auto <- na.omit(auto)

#Let's begin visualizing some of this data
attach(auto) #attach() allows the user to call on the column names of auto directly
plot(cylinders, mpg)

cylinders <- as.factors(cylinders) #This variable is more appropriately interpreted as a categorical variable
plot(cylinders, mpg) #Now that cylinders is a factor, plot() will output a boxplot
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)

#hist() can be used to plot a histogram
hist(mpg)
hist(mpg, col=2) #note that col=2 is equivalent to col="red"
hist(mpg, col=2, breaks=15)

#pairs() creates a scatterplot matrix for every pair of variables in a given data set; this can also be for a subset of those variables
pairs(auto)
pairs(~ mpg + displacement+ horsepower + weight + acceleration, auto)

#The identify() function lets you manually select points on a plot to identify
plot(horsepower, mpg)
identify(horsepower, mpg, name)

#summary() gives a numerical summary of each variable in a data set
summary(auto)

