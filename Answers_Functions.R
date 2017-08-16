####################################
# Answers for Functions exercises ##
####################################

#Create a function that will return the sum of 2 integers
f.sum <- function (x, y) {
  r <- x + y
  r
}
f.sum(5, 10)


#Fill in this function to computes descriptive stats
standard.error <- function(x) {
  sqrt(var(dat)/length(dat))
}
standard.error(dat)



# Uncomment and fill in a simple function with a nested if statement will print alternative outcomes



Isit <- function(x){
 if (x == 10)
print("It's a ten!")
else print("Not a ten!")
}

Isit(1)

Isit(10)


#How do you calculate the sum of each row using apply?
apply(X, 1, sum)