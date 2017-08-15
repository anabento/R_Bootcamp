################################
## Author: Your name        ####
## Date: 08.18.17           ####
## Purpose: My first script ####
################################


rm(list=ls())


### chunk 1

for(i in 1:10){ # for loop run from 1 to 10
  print("Hello world!") # write Hello world 10 times
  print(i*i) # also print each number multiplied by itself 
}



### chunk 2

# A simple function that needs functions from the ape library to run
# ape is a library of functions for analysis of evolutionary relationships:
Tree <- function(x){
  #this function creates a plot of a random evolutionary tree
  #of size x
  require(ape)
  tree <- rcoal(x)
  plot(tree)
}
Tree(30)

### chunk 3

Isit <- function(x){
  if (x == 10)
    print("It's a ten!")
  else print("Not a ten...")
}


Isit(10)

Isit(1)
