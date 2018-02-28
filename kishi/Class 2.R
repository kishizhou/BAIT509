library("tidyverse")
library("ISLR")
library("knitr")

genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}

genreg(1000) 
#1. Generate Data

dat<- genreg(1000)
#2 
dat <- mutate(dat, yhat = 5, yhat1 = 5-x1, yhat2 = 5+2*x2, yhat3=5-x1 + 2*x2)
dat
mse <- mean((dat$yhat - dat$y)^2)
mse1 <- mean((dat$yhat1 - dat$y)^2)
mse2 <- mean((dat$yhat2 - dat$y)^2)
mse12 <- mean((dat$yhat3 - dat$y)^2)

mse
mse1
mse2
mse12


#Oracle Classification
## 1. Probability when X=1
proA=0.2
proB1=0.8/(1+exp(-1))
proB1
proC1=1-proA-proB1
proC1
#LOOK at the model, we predict the classisfication is B when x=-1 because it has the highest probability 

##  probability when x=-2
proA=0.2
proB2=0.8/(1+exp(2))
proB2
proC2=1-proA-proB2
proC2
#LOOK at the model, we predict the classisfication is C when x= 2 because it has the highest probability 

#Have problem with plotting the graph?
install.packages("ggplot2")
library(ggplot2)
qplot(aes(proA,proB1,proC1))
         geom_histogram())

#2.If we look at the graph, when x>0, we say we fit the classification B
#If x<0, we fit the classification of C
#If x=0, we can not fit any of them, but we can further do the other machine laearning models

#3. generate the orginal data
gencla.c <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x)))
  tibble(x=x, y=y)
}  
data2<-gencla.c(1000)
data2

#4.Make predicitions on Y from X

data2<-mutate(dat2,
             yhat= sapply(x,function(x)
               if(x<0)"C"else"B"))
               
data2
#5. Calculating the error rate 
1-mean(data2$yhat==data2$y)
