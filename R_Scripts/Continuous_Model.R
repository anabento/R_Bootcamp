##################################################
## Author: Your name                           ###
## Date: 08.18.17                              ###
## Purpose: Simulations Continuous model        ###
##################################################


rm(list=ls()) # clear


## Continous model logistic growth

install.packages("deSolve") # instal package
library(deSolve) ## load the functions into the current R environment

## first, we need to define the function that calculates the derivative
## 'ode' requires that the function have three arguments, in this order: 
## t: the time points that you want to record the output
## n: the initial value of the variable(s)
## p: the parameters
cont.logistic = function(t, n, p) { 
  r = unname(p['r'])
  K = unname(p['K'])
  ## calculate the rate of change
  dndt = r * n * (1 - n / K)
  ## return the rate of change as a list object (required by ode)
  list(c(dndt)) ## if you were calculating more than one derivative, you would have c(dn1dt,dn2dnt,etc.)
}

## what times do you want to record the output?
times = seq(0, 10, by=1) ## creates a vector running from 0 to 10 in steps of 1

## what are the parameter values?
p = c(r=0.5, K=100)

## what is the initial population size?
n0 = 1

## call the function 'ode' to do the numerical integration. ode requires four arguments (see ?ode to see other options)
## y: the initial value vector
## times: the vector of time points for recording the output (note that this doesn't affect the accuracy at all)
## func: the function specifying the derivatives
## parms: the parameters required by the function specified by 'func'
n <- as.data.frame(ode(y=n0, times=times, func=cont.logistic, parms=p)) ## can replace ode with lsoda

names(n)<-c("time", "N")


## You can see that what is returned by the function 'ode' is a matrix with two columns: the first column gives the time points, and the second column gives the population size. You can easily plot this:
plot(x=n[,1], y=n[,2], type='l', xlab='Time', ylab='Population size', main='Continuous time logistic model')