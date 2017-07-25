#Function Scramble


#1. Round a value, this function uses the floor function to do what the round function does
Rounder <- function(x){
  x <- x +0.5
  floor(x) 
}

#2. Discrete logistic, size of population at time t, and starting with N0 individuals and growth rate lambda

discreteN <- function(N0,lambda, t){
  Nt <- N0*(lambda^t) 
  return(Nt)
} 


#3 A simple function that needs functions from the ape library to run
# ape is a library of functions for analysis of evolutionary relationships:

RandTreePlotter <- function(x){
    #this function creates a plot of a random evolutionary tree
   #of size x
    require(ape)
    tree <- rcoal(x)
    plot(tree)
}

#4. Test if value is equal to 10. 

Isitaten <- function(x){ 
  if(x==10)
    print("It's a ten!")
  else print("Not a ten") 
}

#5. Rounder with error handling
Rounder2  <-  function(x){
  if (is.numeric(x)==FALSE){
    stop("x is not of type numeric")
  }
  x <- x +0.5
  y <- floor(x)
  return(y)
}


#6. Filter flight data to find most delayed departed flight for a specific month
MostDelayedFlight <- function(x){
  require(nycflights13)
  y <- flights[which(flights$month==x),] #pull out flights from month of choice
  y <- y[which(!is.na(y$dep_delay)),] #remove NA values
  latest <- y[which(y$dep_delay==max(y$dep_delay)),]
  return(latest)
}


#7. Exponential Growth ODE 

ExponentialGrowth<- function(t, y, params) {

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
  })
}                     