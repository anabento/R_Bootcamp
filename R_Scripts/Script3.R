####################################
## Author: Your name             ###
## Date: 08.18.17                ###
## Purpose: Simulations          ###
###################################


rm(list=ls()) # clear


## Discrete model logistic growth

## We want to define a function that calculates the right hand side of the discrete time logistic model. Such a function will take two 'arguments': the current population size and the parameter values. 
##Let 'n' be the current population size, and 'p' be a parameter vector containing two terms, the growth rate (r) and the carrying capacity (K)

## First specify the initial population size and the parameters
n0 = 1
p = c(r = 0.5, K=100) ## c() is a function that simply concatenates the two entries, creating a vector with two entries
p[1] ## prints the first entry in the parameter vector
##   r 
## 0.5
p[2] ## prints the second entry in the parameter vector
##   K 
## 100
## Note that the entries of p are "named" r and K
names(p)
## [1] "r" "K"
## Naming gives us a second way to refer to each parameter
p['r']
##   r 
## 0.5
p['K']
##   K 
## 100
## Naming the parameters is really useful, especially when simulating models with lots of parameters, because it keeps you from having to remember which entry in the vector corresponds to each parameter.

## Now we can define our function
disc.logistic = function(n, p) { ## here are just saying that 'disc.logistic' is a function that takes two arguments
  r = unname(p['r']) ## set the growth rate (dropping the parameter name with 'unname')
  K = unname(p['K']) ## set the carrying capacity
  n1 = n + r * n * (1 - n / K) ## calculate the new population size
  return(n1) ## and return it 
}

## So, to calculate the population size at time t=1, 
n1 = disc.logistic(n0, p) ## call the function with the population size at t=0 and the parameters
n1
## [1] 1.495
## To calculate the population size at t=2, call the function with n=n1 and the parameters
n2 = disc.logistic(n1, p)
n2
## [1] 2.231
## You can continue in this fashion forever, but of course that is pretty annoying because you have to specify the value of 'n' anew each time. A better way to do it would be to create a function that only required you to pass in the initial population size, the parameters, and how many time steps you want to simulate.
disc.logistic.2 = function(n, p, T) { ## T is the total number of time steps
  ## Create a new variable that is a vector of length T, so that N[1] is the initial population size (t=0), N[2] is the population size at time t=1, N[3] = pop'n size at t=2, ..., N[T] = pop'n size at t=T-1 and N[T+1] = pop'n size at t=T
  N = vector(length=T+1)
  N[1] = n ## set the initial value
  ## Create a loop that will increment time by one time unit, calculate the new population size, and save it in our vector
  for (t in 1:T) { 
    N[t+1] = disc.logistic(N[t], p) ## we can call the function we wrote above to calculate the new population size
  }
  return(N) ## return the vector of population sizes
}
n = disc.logistic.2(n0, p, 10) ## calculate just the first 10 time steps
n
##  [1]  1.000  1.495  2.231  3.322  4.928  7.271 10.641 15.396 21.909 30.463
## [11] 41.055
## With this function, we can simulate the model for any number of timesteps we wish. We might also want to plot the results. This can be done with the function plot(). Type ?plot in the R console to see all of the available options for plotting. The best way to plot is to specify both the x and y values. In this case, the x-axis is time, so the x values are the time steps, and the y-axis is population size. 0:10 generates a sequence of integers from 0 to 10 (our time steps). type='l' tells R to make a line plot (as opposed to a dot plot), and the xlab and ylab options sets the x- and y-axis labels.
plot(x=0:10, y=n, type='l', xlab='Time', ylab='Population size', main='Discrete time logistic model')





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
n = ode(y=n0, times=times, func=cont.logistic, parms=p)
n
##    time      1
## 1     0  1.000
## 2     1  1.638
## 3     2  2.672
## 4     3  4.331
## 5     4  6.945
## 6     5 10.957
## 7     6 16.866
## 8     7 25.065
## 9     8 35.546
## 10    9 47.624
## 11   10 59.986
## You can see that what is returned by the function 'ode' is a matrix with two columns: the first column gives the time points, and the second column gives the population size. You can easily plot this:
plot(x=n[,1], y=n[,2], type='l', xlab='Time', ylab='Population size', main='Continuous time logistic model')