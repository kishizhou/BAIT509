library("tidyverse")
library("ISLR")
library("knitr")

set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))
#1
# 2.7 is the mean of Y at X =0 


#kNN
dat$d <- abs(dat$x-0)
dat$d
dat.sub1<-arrange(dat,d)
dat.sub<-arrange(dat,d)[1:5,]
dat.sub
y.predict<-mean(dat.sub$y)
y.predict
#Loess
dat.sub2<-filter(dat.sub1,d<1)
y.pre.loess<-mean(dat.sub2$y)
y.pre.loess

# r is small, no observations exist, can not do prediction 
#k or r 
#r is small, overfitting the data, low bias but high variance; 
#if r is big, high bias but low variance;  


###############################
####### Exercise 2 ###########
xgrid <- seq(-5, 4, length.out=1000)
kNN_estimates <- map_dbl(xgrid, function(x){
dat$d <- abs(dat$x-x)
dat$d
dat.sub1<-arrange(dat,d)
dat.sub<-arrange(dat,d)[1:5,]
dat.sub
yhat<-mean(dat.sub$y)
yhat
return(yhat)})
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for kNN from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)

loess_estimates <- map_dbl(xgrid, function(x){
dat$d <- abs(dat$x-x)
dat$d
dat.sub1<-arrange(dat,d)
dat.sub2<-filter(dat.sub1,d<2)
yhat<-mean(dat.sub2$y)
return(yhat)})
  
  
## Note: The variable "x" here is a single value along the grid.
## Hint1: Extend your code for loess from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)

est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y), colour="orange") +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()

# change window to 100
# This is underfitting, we did not predict the correct trend 

#Use the window of 1
#We can see KNN method is overfitting

