#This script was created by Reni Kaul 7/22/17 for the IDEAS Data Clinic. This code should be completed with a partner, and will walk students through common data exploration tasks. Some lines are complete, while others are missing pieces or will have an error that needs fixing. 
#The data is stored in XXX.csv. 

#Before we can explore the data, we must load it into R. 
#load survey results into an object called cohort
cohort <- read.csv("CohortData.csv") 


#1. Exploring Data Structure
#What is the structure of the data?
str(Cohort)

#What are the names of the different columns?
colname(cohort)

#How many people responsed to the survey?
length(cohort)
  #This doesn't return exactly what we want try either:
#a. modifying the arguement given to length
#b. using the function dim()
#c. using the function nrow()


#2. Exploring Data with Summary Statistics

#What is the max, min and mean distance to the students favplace? 
mean(cohort$favplace) #return the mean distance
min(cohort$place) #return the min distance
XXX(cohort$favplace) #return the max distance

#Use the apply function to calculate the mean, min and max for all the interger columns 
apply(cohort[,1:4], 2, max)

#Use the summary function to create summary statistics for all of the columns
summary


#3. Exploring Data with Basic Visualizations

#What is the distribution of the no. of siblings in the cohort?
hist(cohort)
  
#Does birth order correlate with no. of siblings?
plot(y=cohort$sibling, ylim=c(0,10), col="black", pch=19, xlab="birth order", ylab="no. of siblings")

#How do the quartiles of the different variables relate to eachother?
boxplot(cohort)

#How does the distribution of no. of siblings, birth order and birth date relate?
boxplot(cohort[1:3,])


