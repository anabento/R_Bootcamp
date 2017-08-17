#####################################
## Author: Your name             ####
## Date: 08.18.17                ####
## Purpose: Exploring NYCflights ####
#####################################

#This code should be completed with a partner, and will walk students through common data exploration tasks. 
#Some lines are complete, while others are missing pieces or will have an error that needs fixing. 
#As you move through the script, the prompts will become more open ended.  

#Before we can explore the data, we must load it into R. 
library(nycflights13)

#1. Exploring Data Structure
#####

#What is the structure of the data?
str(flights)

#What are the names of the different columns?
colnames(flights)

#How many flights are recorded?
length(flights)
  #This doesn't return exactly what we want try either:
#a. modifying the arguement given to length
#b. using the function dim()
#c. using the function nrow()

#What are the airports in NYC?
unique(flights$origin)

#What other things would you want to know? 





#2. Exploring Data with Summary Statistics
#####
library(dplyr) #useful data manipulation package that allows you to:
  #join different dataset: inner_join() left_join()
  #group data by a variable: group_by()
  #select data with variable value: select()

#What is the mean delayed time? 
mean(flights$dep_delay, na.rm = TRUE) #return the mean distance, what does na.rm mean? 

#What is the mean time AA flights are delayed? 
#(using base R only; this could also be done using dplyr)

  AAflights <- flights[which(flights$carrier=="AA"),]
  mean(AAflights$dep_delay, na.rm = TRUE)

  #Calculate the average delayed time for each airline
  aircarrier <- unique(flights$carrier) #vector of carriers
  CarrierDelay <- c() #create empty vector to store mean temp

  for ( i in 1:length(aircarrier)){ #start a loop to calculate mean for each airline
    tmp <- flights[which(flights$carrier==aircarrier[i]),] #subset by carrier i in vector
    tmp.mean <- mean(tmp$dep_delay, na.rm = TRUE) #calculate mean for subsetted data
    CarrierDelay <- c(CarrierDelay, tmp.mean) #save mean
    }

#Use the summary function to create summary statistics for all of the columns
summary()





#3. Exploring Data with Basic Visualizations
####

#Plot average delay by carrier
barplot(CarrierDelay, #y values to plot
        names.arg = aircarrier,  cex.names =0.65,  #x axis labels and size
        ylab="Ave time delayed", xlab="Airline") #axis labels


#What is the distribution of flight departure?
hist(flights$dep_time) 
  


#What is the relationship between 2 variables?
plot(x=, y=, col="blue") #fill in the x and y values to make a scatter plot. 
points(x=,y=, col=2) # add a different set of values of a different color. 
legend() #add a legend to describe the 2 different colors

#How does the distribution of temperature by month?
boxplot(weather$temp ~ weather$month ) #instead of assigning x and y values, you can use the syntax y ~ x




