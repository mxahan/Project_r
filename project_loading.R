hw <- read.csv("framingham.csv")
class(hw)
coln <- colnames(hw) ## class(hw$age); hw[coln[2]], dim(hw)

vage <- hw$age ## converting as vector... important line

dfage <- hw["BMI"]

for (i in coln){print(i)}

head(hw[,c(coln[2],"male")])


plot(hw$age, hw$BMI)


card <- read.csv("CarPrice_Assignment.csv")

leda <- read.csv("Life Expectancy Data.csv")
