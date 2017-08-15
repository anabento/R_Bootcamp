####################################
## Author: Your name             ###
## Date: 08.18.17                ###
## Purpose: Simple SIR           ###
###################################


rm(list=ls()) # clear



require(deSolve) # package load


#### SIR EXAMPLE, with plots
SIRModel <- function (t, x, params) {   
  
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

params.SIR <- c(beta=0.3,gamma=1/7)  #function c "combines values into a vector`


#Finally we specify the initial conditions, the values of the state variables $S$, $I$, and $R$ at the beginning of the simulation: here we will define them as proportions of the overall population size (what would happen if we changed that?)

xstart.SIR <- c(S=9999/10000,I=1/10000,R=0)     #initial conditions`


#We are now ready to solve the SIR model for each time in the `times` vector with the `lsoda` command:
out.SIR <- as.data.frame(lsoda(xstart.SIR,times.SIR,SIRModel,params.SIR))  #result stored in dataframe`

with(out.SIR,plot(S~time,type="o",col="blue",ylim=c(0,1),ylab="S (blue), I (red), R (green)"), pch=16)
with(out.SIR,lines(I~time,type="o",col="red"), pch=16)
with(out.SIR,lines(R~time,type="o",col="green"), pch=16)

## Making the plots snazier

library(ggplot2)

library(gridExtra) # load package



p1<-ggplot(data=out.SIR,aes(x=time,y=I))+ ### 
  geom_line(colour="red")+ 
  theme_bw()+labs(x = "time", y = "Population Infected")
p2<-ggplot(data=out.SIR,aes(x=time,y=R))+ ### 
  geom_line(colour="green")+ 
  theme_bw()+labs(x = "time", y = "Population Recovered")
p3<-ggplot(data=out.SIR,aes(x=time,y=S))+ ### 
  geom_line(colour="blue")+ 
  theme_bw()+labs(x = "time", y = "Population Susceptible")

grid.arrange(p1, p2, p3, nrow=1)
## all together

library(reshape) # load package

mdata <- melt(out.SIR, id=c("time"))# this will reshape the data
names(mdata)<-c("time", "Comp", "value")
mycols<-c( "blue","red","green")

p <- ggplot(data = mdata, mapping = aes(x = time, y = value, color = Comp))
p <- p + theme_bw() + labs(x = "time", y = "Population")
p<- p + scale_colour_manual(values = mycols)
p <- p + geom_line() 
p

