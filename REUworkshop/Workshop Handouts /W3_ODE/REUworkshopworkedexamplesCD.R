

### Hi everyone, here are the solutions to the various exercises from the modeling workshop on 6/10/2015

#########################################
### Exponential growth and exercises 1-2
#########################################

require(deSolve)

ExponentialGrowth <- function(t, y, params) {

# Computes the rate of change of the variable y over time
#
# Args:
#   t: vector of time points in the integration
#   y: vector of variables in the differential equation
#   params: vector of parameter values
#
# Returns:
#   The rate of change of the variable y.

  N<-y #create local variable N

  with(as.list(c(params)), { 

#this argument to "with" lets us use the variable names

    dN <- r*N     # Right hand side of the differential equation
    res <- c(dN) #combine results into a single vector dx
    list(res) #return list of results

    }
  )
}                         

times <- seq(0,2,by=0.5) #function seq returns a sequence
params <- c(r=0.75) #function c "c"ombines values into a vector`
Nstart <- c(N=1) #initial conditions`

out <-  as.data.frame(lsoda(Nstart, times, ExponentialGrowth, params)) 
out
#

### Exercise 1. What does the population look like after 10 hours?

times.ex.1 <- seq(1,10,by=1)
ex.1 <- as.data.frame(lsoda(Nstart,times.ex.1,ExponentialGrowth,params)) ### can recycle prior inputs, but update times
with(ex.1,plot(N~time,type="b"))

### Exercise 2. Solve the initial value problem; dN/dt = -0.5N, N0 = 100
params.ex.2 <- c(r = -0.5) 
Nstart.ex.2 <- c(N=100)
times.ex.2 <- seq(1,10,by=1)

ex.2 <- as.data.frame(lsoda(Nstart.ex.2,times.ex.2,ExponentialGrowth,params.ex.2)) ### need to use new parameter values
with(ex.2,plot(N~time,type="b"))


#### SIR EXAMPLE, with plots
ClosedSIRModel <- function (t, x, params) {   
  
  # Computes the rate of change of the variables S, I and R over time
  #
  # Args:
  #   t: vector of time points in the integration
  #   x: vector of variables in the differential equation
  #   params: vector of parameter values
  #
  # Returns:
  #   The rate of change of the variables S, I and R.
  
  
  S <- x[1]                               #create local variable S, the first element of x
  I <- x[2]                               #create local variable I
  R <- x[3]                               #create local variable R
  
  
  with(                                   #we can simplify code using "with"
    as.list(params),                   #this argument to "with" lets us use the variable names
    {                                  #the system of rate equations
  
    N <- S + I + R
  
    dS <- -beta*S*I*(1/N)      ### make sure susceptibles are removed here (i.e. -beta*S*I/N)
    dI <- beta*S*I*(1/N)-gamma*I
    dR <- gamma*I
    dx <- c(dS,dI,dR)                #combine results into a single vector dx
    list(dx)                         #return result as a list,
  #note order must be same as the state variables
    }
  )
}

#We now state the times at which we want solutions:
  
times.SIR <- seq(0,120,by=5)    #function seq returns a sequence

#We also require numerical values for each constant (parameter) in the SIR equation. The parameters are the contact rate $\beta$ and the per-capita recovery rate $\gamma$. Here we assign some values to the parameters: 
  
params.SIR <- c(beta=0.3,gamma=1/7)  #function c "c"ombines values into a vector`


#Finally we specify the initial conditions, the values of the state variables $S$, $I$, and $R$ at the beginning of the simulation: here we will define them as proportions of the overall population size (what would happen if we changed that?)

xstart.SIR <- c(S=9999/10000,I=1/10000,R=0)     #initial conditions`


#We are now ready to solve the SIR model for each time in the `times` vector with the `lsoda` command:
out.SIR <- as.data.frame(lsoda(xstart.SIR,times.SIR,ClosedSIRModel,params.SIR))  #result stored in dataframe`

with(out.SIR,plot(S~time,type="b",col="black",ylim=c(0,1),ylab="S (black), I (red), R (purple)"))
with(out.SIR,lines(I~time,type="b",col="red"))
with(out.SIR,lines(R~time,type="b",col="purple"))


### Exercise 3; sketch a demographically open SIR model - we'll implement it in exercise 7
## Here's what the transitions turn into:
# dS <- b*S - beta*S*I*(1/N) - mu*S # assuming b = per capita birth rate, mu = per capita death rate
# dI <- beta*S*I*(1/N) - mu*I - gamma*I # assuming death rate for I is same as S. If additional mortality... call it 'v' and make the last term " - (v + mu)*I
# dR <- gamma*I - mu*R


### Exercise 4

times.e4 <- seq(0,300,by=1)    #function seq returns a sequence`

params.e4a <- c(beta=0.3,gamma=1/7)  #function c "c"ombines values into a vector`

xstart.e4a <- c(S=(9000/10000),I=(100/10000),R=900/10000)     #initial conditions`

#Finally we specify the initial conditions, the values of the state variables $S$, $I$, and $R$ at the beginning of the simulation: here we will define them as proportions of the overall population size (what would happen if we changed that?)

out.e4a <- as.data.frame(lsoda(xstart.e4a,times.e4,ClosedSIRModel,params.e4a))  #result stored in dataframe`


par(fig=c(0,0.5,0,1),mar=c(4,4,1,1),mfrow=c(1,2))
with(out.e4a,plot(S~time,type="l",col="black",ylim=c(0,1),ylab="S (black), I (red), R (purple)"))
with(out.e4a,lines(I~time,type="l",col="red"))
with(out.e4a,lines(R~time,type="l",col="purple"))

with(out.e4a,plot(I~S,xlab="proportion susceptible",ylab="proportion infected",type="p"))
par(fig=c(0,0.5,0,1),mar=c(4,4,1,1),mfrow=c(1,1))

#################
### Deriving thresholds
################

#### R0 = beta/gamma (when proportion of susceptibles approximately equals the total population)
R0 <- params.e4a["beta"]/params.e4a["gamma"]
# Here, R0 = 2.1

#### When R0 > 1, a disease can spread through a population. When R0 < 1, diseases do not spread
#### To get R0 below 1... we need to make dI/dt negative
#### dI/dt = beta*S*I/N - gamma*I # assuming no mortality/birth
### 0 < beta*S*I/N - gamma*I # divide by I
### 0 < beta*S/N - gamma # when the population is almost entirely susceptible, S = N (approximately). So...
### 0 < beta - gamma
### So, with frequency-dependent transmission, a disease will increase if transmission is greater than recovery (makes some sense)

### Without altering the recovery or transmission rates (which is presumably hard for real systems), how do we make R0 less than 1?
### Let's break the assumption that everyone is susceptible
### If a fraction of the population is immune (or 'recovered' as we currently have things), S =/= N at the onset

### Then...
### 0 < beta*S/N - gamma
### gamma < beta*S/N
### gamma/beta <> S/N
### so, if the proportion of susceptibles (S/N) is greater than gamma/beta, a disease can increase
### if S/N < gamma/beta, it will decrease. Let's see that with the equations

### define the 'invasion threshold' as (1/R0) # convince yourself that that's the same as the proportion of susceptibles necessary for increase
invasion.threshold <- 1/R0

### and then convince yourself that 1- this quantity is the proportion of individuals you'd have to vaccinate for this particular disease
proportion.to.vaccinate <- 1 - (1/R0)


xstart.e4b <- c(S=0.48,I=0.01,R=0.51)     #initial conditions, S/N > gamma/beta
out.e4b <- as.data.frame(lsoda(xstart.e4b,times.e4,ClosedSIRModel,params.e4a))
xstart.e4c <- c(S=0.46,I=0.01,R=0.53)     #initial conditions, S/N < gamma/beta
out.e4c <- as.data.frame(lsoda(xstart.e4c,times.e4,ClosedSIRModel,params.e4a))

R0.e4b <- (params.e4a["beta"] * (xstart.e4b["S"])) / params.e4a["gamma"] ### because not all the population is susceptible, we adjust the equation for R0
R0.e4c <- (params.e4a["beta"] * (xstart.e4c["S"])) / params.e4a["gamma"]

#set graphical parameters

par(mfrow=c(1,3))
plot(I~time,data=out.e4b,type="b",ylab="I",xlab="time",col="red") ### here, S/N was just larger than the invasion threshold (and R0 was = 1.03)
points(I~time,data=out.e4c,type="b",ylab="I",xlab="time",col="blue") ### here, S/N was just smaller than the threshold (R0 = 0.966)

plot(I~time,data=out.e4b,type="b",ylab="I",xlab="time",col="red",xlim=c(0,10),ylim=c(0.0095,0.0105)) ### here, S/N was just larger than the invasion threshold (and R0 was = 1.03)
abline(a=xstart.e4b["I"],b=0)
points(I~time,data=out.e4c,type="b",ylab="I",xlab="time",col="blue") ### here, S/N was just smaller than the threshold (R0 = 0.966)
### So in this second panel (which is just zoomed in on the beginning of the first panel), we see that the 'just over the S/N threshold case does have some TINY increase (i.e. red dots compared to black line)
### But! the disease quickly dropped S/N below the threshold, which is why it decreases so quickly
### play around with the initial proportions of S and R and replot (just make sure to adjust your x and y limits with xlim=c(min,max) and ylim=c(min,max))

plot(I~S,data=out.e4a,type="l",xlab='S',col="black")
points(I~S,data=out.e4b,type='l',xlab='S',col="red")      #plot phase portrait
points(I~S,data=out.e4c,type='l',xlab='S',col="blue")      #plot phase portrait

par(mfrow=c(1,1))                                                     #re-set graphical parameters

#### Exercise 5; 

times.e5 <- seq(0,150,by=1)    #function seq returns a sequence`

params.e5a <- c(beta=0.3,gamma=1/7)  #function c "c"ombines values into a vector`
params.e5b <- c(beta=0.6,gamma=1/7)  #function c "c"ombines values into a vector`
params.e5c <- c(beta=0.3,gamma=1/14)  #function c "c"ombines values into a vector`
params.e5d <- c(beta=0.6,gamma=1/14)  #function c "c"ombines values into a vector`

xstart.e5 <- c(S=0.99,I=0.01,R=0)     #initial conditions

out.e5a <- as.data.frame(lsoda(xstart.e5,times.e5,ClosedSIRModel,params.e5a))
out.e5b <- as.data.frame(lsoda(xstart.e5,times.e5,ClosedSIRModel,params.e5b))
out.e5c<- as.data.frame(lsoda(xstart.e5,times.e5,ClosedSIRModel,params.e5c))
out.e5d <- as.data.frame(lsoda(xstart.e5,times.e5,ClosedSIRModel,params.e5d))

par(mfrow=c(2,2))
### baseline
plot(I~time,data=out.e5a,type="l",ylab="I",xlab="time",col="red",ylim=c(0,1),main="baseline")
points(R~time,data=out.e5a,type="l",ylab="I",xlab="time",col="purple")
points(S~time,data=out.e5a,type="l",ylab="I",xlab="time",col="black")

## increase transmission rate
plot(I~time,data=out.e5b,type="l",ylab="I",xlab="time",col="red",ylim=c(0,1),main="high transmission")
points(R~time,data=out.e5b,type="l",ylab="I",xlab="time",col="purple")
points(S~time,data=out.e5b,type="l",ylab="I",xlab="time",col="black")

## same initial transmission rate, but decreased recovery rate
plot(I~time,data=out.e5c,type="l",ylab="I",xlab="time",col="red",ylim=c(0,1),main="low recovery")
points(R~time,data=out.e5c,type="l",ylab="I",xlab="time",col="purple")
points(S~time,data=out.e5c,type="l",ylab="I",xlab="time",col="black")

## increased  transmision rate and decreased recovery rate
plot(I~time,data=out.e5d,type="l",ylab="I",xlab="time",col="red",ylim=c(0,1),main="high beta and low gamma")
points(R~time,data=out.e5d,type="l",ylab="I",xlab="time",col="purple")
points(S~time,data=out.e5d,type="l",ylab="I",xlab="time",col="black")


par(mfrow=c(2,2))  

plot(I~S,data=out.e5a,type="b",xlab='S',col="black",xlim=c(0,1),ylim=c(0,1),main="baseline")
plot(I~S,data=out.e5b,type='b',xlab='S',col="black",xlim=c(0,1),ylim=c(0,1),main="high transmission")      #plot phase portrait
plot(I~S,data=out.e5c,type='b',xlab='S',col="black",xlim=c(0,1),ylim=c(0,1),main="low recovery")      #plot phase portrait
plot(I~S,data=out.e5d,type='b',xlab='S',col="black",xlim=c(0,1),ylim=c(0,1),main="high beta and low gamma")      #plot phase portrait

par(mfrow=c(1,1))  

### Overall, increasing transmission increases the size of epidemics (because it also increases R0)
### Similarity, decreasing the recovery rate increases the size of epidemics (again, increases R0)
### The recovery rate is the reciprocal of the infectious period of the pathogen. So, when gamma = 1/7, the infectious period is 7 (days, or weeks, or whatever unit you are using)
### this is why, in the 'low recovery' scenario, we see a peak epidemic size ~0.4 (similar to the high transmission scenario),
#### BUT the duration of the epidemic is longer - because we've made the average infected individual stick around for longer (i.e. 14 'days' instead of '7')


#### Exercise 6.
require(deSolve)

sis.model.closed <- function (t, x, params) {   
      #here we begin a function with one arguments
      I <- x[1]                               #create local variable , the first element of x                             #create local variable R
      with(                                   #we can simplify code using "with"
        as.list(params),                   #this argument to "with" lets us use the variable names
          {                                  #the system of rate equations
            dI <- beta*(N-I)*I/N-gamma*I ### here N - I = S. Still frequency-dependent transmission
            dx <- dI              #combine results into a single vector dx
            list(dx)                         #return result as a list
          }
          )
}


times.e6 <- seq(0,120,by=5)                    #function seq returns a sequence
params.e6 <- c(beta=0.3,gamma=1/7, N=10000)             #function c "c"ombines values into a vector
xstart.e6 <- c(I=1) #initial conditions

#Next, we simulate a model trajectory with the \code{lsoda} command:

out.SIS <- as.data.frame(lsoda(xstart.e6,times.e6,sis.model.closed,params.e6))  #result stored in dataframe

op <- par(fig=c(0,0.5,0,1),mar=c(4,4,1,1))                  #set graphical parameters
plot(I~time,data=out.SIS,type='b',col="red")                              #plot the I variable against time
par(fig=c(0.5,1,0,1),mar=c(4,4,1,1),new=T)                  #re-set graphical parameters
plot(params.e6['N']-I~time,data=out.SIS,type='b', ylab = 'S')      
par(op)                                                     #re-set graphical parameters

### SIS model goes to an endemic equilibrium - as long as the infection is able to invade the population, then loss of immunity guarantees long term persistence. And not everyone will get infected!
### In fact, you already calculated the equilibrium proportion of susceptibles (can you think of what it is?)
tail(out.SIS)
### it's 1 - (1/R0) - because the pathogen only has positive growth up until this point
### and, because it's an SIS model, 1/R0 is the equilibrium prevalence. So with no recovery, about 47% of our simulated population would be infected with this simulated pathogen


### Exercise 7

#### SIR with births, deaths
OpenSIRModel <- function (t, x, params) {   
  
  # Computes the rate of change of the variables S, I and R over time
  #
  # Args:
  #   t: vector of time points in the integration
  #   x: vector of variables in the differential equation
  #   params: vector of parameter values
  #
  # Returns:
  #   The rate of change of the variables S, I and R.
  
  
  S <- x[1]                               #create local variable S, the first element of x
  I <- x[2]                               #create local variable I
  R <- x[3]                               #create local variable R
  
  
  with(                                   #we can simplify code using "with"
    as.list(params),                   #this argument to "with" lets us use the variable names
{                                  #the system of rate equations
  
  N <- S + I + R
  
  ### Note! The formulation that I went through in the disease workshop is not identical to the classical derivation
  ### Generally, the birth rate (b) is assumed a constant - i.e. it doesn't depend on the Susceptible population
  ### The constant (b) is akin to immigration from outside the population, rather than generation of new susceptibles from within
  ### It is illustrative to explore both forms; run the model with the same b and mu (birth and death values),
  ### but switch the 'birth term' between "b" to "b*S"
  ### What do you notice as the difference? When might one formulation be preferred to the other?
  
  dS <- (b) - (beta*S*I*(1/N)) - (mu*S)      ### or, dS <- (b*S) - (beta*S*I*(1/N)) - (mu*S)
  dI <- (beta*S*I*(1/N)) - (gamma*I) - (mu*I)
  dR <- (gamma*I) - (mu*R)
  dx <- c(dS,dI,dR)                #combine results into a single vector dx
  list(dx)                         #return result as a list,
  #note order must be same as the state variables
}
  )
}

#We now state the times at which we want solutions:

times.SIR.open <- seq(0,500,by=5)    #function seq returns a sequence`

#We also require numerical values for each constant (parameter) in the SIR equation. The parameters are the contact rate $\beta$ and the per-capita recovery rate $\gamma$. Here we assign some values to the parameters: 

params.SIR.open <- c(beta=0.3,gamma=1/7,b=0.01,mu=0.01)  #function c "c"ombines values into a vector`


#Finally we specify the initial conditions, the values of the state variables $S$, $I$, and $R$ at the beginning of the simulation: here we will define them as proportions of the overall population size (what would happen if we changed that?)

xstart.SIR.open <- c(S=0.99,I=0.01,R=0)     #initial conditions`


#We are now ready to solve the SIR model for each time in the `times` vector with the `lsoda` command:
out.SIR.open <- as.data.frame(lsoda(xstart.SIR.open,times.SIR.open,OpenSIRModel,params.SIR.open))  #result stored in dataframe`

#str(out.SIR.open)
par(mfrow=c(1,1))
with(out.SIR.open,plot(S/(S+I+R)~time,type="b",col="black",ylim=c(0,1),ylab="S (black), I (red), R (purple)"))
with(out.SIR.open,lines(I/(S+I+R)~time,type="b",col="red"))
with(out.SIR.open,lines(R/(S+I+R)~time,type="b",col="purple"))

#### Point is that, with births in the model, disease can come back when proportion of susceptibles becomes high enough again
#### but what's the new equilibrium number of susceptibles?
### long-term eq(S) should be ~ 1- (gamma+mu) /  beta:
abline(b=0,a=1-(((1/7)+0.01)/0.3)) 
### Essentially, things that remove infecteds (recovery, death) / things that add infecteds (transmission)


### So please mess around with the initial proportion of S, I, and R - see what happens! Change beta and gamma, and convince yourself that you can still calculate equilibria


##########################
#### Stochastic models
##########################

### Exercise 8. 
dbinom(6,10,0.5) ### dbinom(# of successes, # of trials, probability of event happening)


### single run of reed-frost model
alpha <- 0.96

s0 <- 100
i <- 1
t <- 0
out <- c(t, s0,i)

while(i>0){ ### this while command is great here, because it keeps running the simulation until there are no more infected individuals, then stops!
  
  s <- rbinom(1, s0,alpha^i) #binomial random variable
  i <- s0 - s #calculate i_t+1 from s_t and and s_t+1 
  t <- t+1 #update time vector
  out <- rbind(out, c(t, s,i)) # appends the new row of data to the output matrix
  s0 <- s # use s to calculate new number of susceptibles in the simulation
}

colnames(out) <-c('time', 'S', 'I')

plot(out[,'I']~out[,'time'],type='b')
head(out)


####### to generate rf.nsims simulates with which to explore the distribution of epidemic durations

rf.nsims <- 5000 ### how many simulations do you want to run?
end.time <-numeric(rf.nsims) ### initialize a vector of numbers, the length of the number of simulations you want to run

for(k in 1:rf.nsims){ ### start a for loop, which will run the RF model rf.nsims number of times
  alpha <- 0.96
  s0 <- 100 ### s0, i, t, and out are all INSIDE the for-loop because they need to RESET each time an epidemic ends (i.e. each time the 'while loop' exits)
  i <- 1 
  t <- 0
  out <- c(t, s0,i)
  
  while(i>0){ ### same while loop we saw previously
    s <- rbinom(1, s0,alpha^i) #binomial random variable
    i <- s0 - s #calculate i_t+1 from s_t and and s_t+1 
    t <- t+1 #update time vector
    out <- rbind(out, c(t, s,i)) # appends the new row of data to the output matrix
    s0 <- s # use s to calculate new number of susceptibles in the simulation
  }
  
  end.time[k] <- out[dim(out)[1],1] ### each time the 'while loop' stops, take the last entry of the first column. In this case, that's the time when the epidemic stopped
}
hist(end.time,breaks=22)
mean(end.time)


### example of how to store multiple epidemics

n.epi <- 100 ### number of epidemics you want
total.out <- matrix(NA, nrow=10*n.epi,ncol=4) # create output matrix with (10*n.epi) number of rows. Because we know most epidemics end before 8-9 time steps, this should produce too many rows. We'll cut extras out later

j <- 1 ### j is going to be our counter

for (k in 1:n.epi){ ### start a for loop, just like above. k will end up being our 'label' for each epidemic
  alpha <- 0.96
  s0 <- 100 
  i <- 1
  t <- 0
  out <- c(t, s0,i,k)
    while(i>0){
      s <- rbinom(1, s0,alpha^i) #binomial random variable
      i <- s0 - s #calculate i_t+1 from s_t and and s_t+1 
      t <- t+1 #update time vector
      out <- rbind(out, c(t, s,i,k)) # appends the new row of data to the output matrix
      s0 <- s # use s to calculate new number of susceptibles in the simulation
    }

  total.out[j:(j+length(out[,1]) - 1),1:4] <- out ### this code fills the four columns of the total.out matrix
                                                  ### from row j to row (j + the length of the epidemic) 
                                                  ### The - 1 is because we started with j = 1
                                                  ### so if the first epidemic took 7 'days', total.out would fill up from rows 1 to 7.
  
  j <- j + length(out[,1]) ### now we need to update our counter, so we can tell ourselves where
                           ### to start the input of the NEXT epidemic. If the first epidemic took 7 'days',
                           ### j goes from 1 (which we specified at the outset) to 1 + 7 = 8.
                           ### Thus, the next epidemic will be input starting on row 8
}

stoch.epi <- as.data.frame(subset(total.out,!is.na(total.out[,1]))) ### this code removes the NA's still left in total.out (we put extra in because we didn't know how long the matrix would need to be, because each epidemic is stochastic)
colnames(stoch.epi) <- c("time","S","I","ID") ### add the columns
str(stoch.epi) ### str stands for 'structure' and is a very useful command. it tells you what objects are!

mean.epi <- aggregate(stoch.epi$I,by=list(stoch.epi$time),FUN=mean) ### the aggregate command passes a function to your data. Here we get the mean number of infecteds by each time point
colnames(mean.epi) <- c("time","mean.I") ### and label the new object
str(mean.epi)
### plotting

#install.packages("ggplot2") ### I'm bad at base R graphics, which is because/why I use a separate package called ggplot
library(ggplot2) ### install it if you need to, then load

### ggplot2 is a little different than base graphics
### in the first row of the call to 'ggplot', we'll give it a data argument (data =...) and
### what's called an 'aesthetic' or 'aes' argument. This is where we put our x and y variables
### the next inputs are called 'geoms' and stand for geometric objects. There are tons. We'll use
### geom_line() for, you guessed it, lines.

ggplot(data=stoch.epi,aes(x=time,y=I))+ ### enter data, x, y
  geom_line(colour="gray75",aes(group=factor(ID)))+ ### create a separte line for each 'epidemic' ID
  geom_line(data=mean.epi,aes(x=time,y=mean.I),colour="black",size=2)+ ### add another line for the 'mean' we calculated earlier
  #stat_smooth(se=TRUE,type="loess",colour="black",size=2,fill="blue")+ ### see for yourself what this does. it's an example of an alternate object ggplot2 can add
  theme_bw() ### the default ggplot has a grey background; this removes that.

### and here's how to do the same/similar thing with base graphics:
with(stoch.epi,plot(I~time,type="l",col="gray75"))
lines(mean.epi$mean.I~mean.epi$time,type="l",col="black",lwd=3)

### for something like this, there's not too much difference between ggplot2 and base R graphics
### where ggplot can be useful is when you want to split things up by factors, like an experimental treatment or an "id" variable
