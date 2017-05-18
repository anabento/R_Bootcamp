### R code from vignette source 'intro.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: intro.Rnw:26-27
###################################################
options(keep.source=TRUE)


###################################################
### code chunk number 2: intro.Rnw:111-115
###################################################
print("Hello world.")
4+5
4^5
8*(7+3)


###################################################
### code chunk number 3: intro.Rnw:126-130
###################################################
# R will completely ignore this line of text
3+4 #R will execute the first part of this line and ignore the rest
# if you need to include a long note with several lines of text,
# be certain that each line is preceded by a # 


###################################################
### code chunk number 4: intro.Rnw:138-139
###################################################
3*4 # This is a comment 


###################################################
### code chunk number 5: intro.Rnw:164-165
###################################################
?cor.test  # this will open up the help file for the "cor.test" function


###################################################
### code chunk number 6: intro.Rnw:184-185
###################################################
library(deSolve)  # this will load the deSolve package 


###################################################
### code chunk number 7: intro.Rnw:190-191
###################################################
?deSolve   # this accesses the main page of the deSolve help files if deSolve is loaded. 


###################################################
### code chunk number 8: intro.Rnw:209-214
###################################################
4 * 6
5 ^ 4
119 / 10
119 %/% 10
119 %% 10


###################################################
### code chunk number 9: intro.Rnw:241-243
###################################################
4 + 2 * 3 + 4
(4 + 2) * (3 + 4)


###################################################
### code chunk number 10: intro.Rnw:248-249
###################################################
119/10; 119 %/% 10; 119 %% 10


###################################################
### code chunk number 11: intro.Rnw:270-274
###################################################
4 == 6
6 == 6
4 != 6
4 > 6


###################################################
### code chunk number 12: intro.Rnw:314-319
###################################################
floor(3.4)
ceiling(3.4)
log(304)
log10(304)
abs(44)


###################################################
### code chunk number 13: intro.Rnw:362-364
###################################################
bob <- 20
bob


###################################################
### code chunk number 14: intro.Rnw:388-390
###################################################
infectious.period <- 20  # infectious period in units of days
infectious.period


###################################################
### code chunk number 15: intro.Rnw:395-397
###################################################
log10(bob)
bob*2


###################################################
### code chunk number 16: intro.Rnw:402-404
###################################################
bob <- 40
bob


###################################################
### code chunk number 17: intro.Rnw:409-411
###################################################
bob <- bob+2
bob


###################################################
### code chunk number 18: intro.Rnw:416-417
###################################################
str(bob)


###################################################
### code chunk number 19: intro.Rnw:432-434
###################################################
x <- c(1,3,5,7,2,8,4,4,10)
x


###################################################
### code chunk number 20: intro.Rnw:439-442
###################################################
1:10
x <- c(1:10)
x


###################################################
### code chunk number 21: intro.Rnw:447-450
###################################################
rep(2, 5)
x <- rep(2,10)
x


###################################################
### code chunk number 22: intro.Rnw:455-459
###################################################
x <- rnorm(10)
x
x <- rnorm(10, mean = 5)
x


###################################################
### code chunk number 23: intro.Rnw:464-468
###################################################
x <- rbinom(10, 1, 0.5)
x
x <- rbinom(10, 4, 0.5)
x


###################################################
### code chunk number 24: intro.Rnw:473-475
###################################################
x <- c(1,3,5,7, 1:8, rep(2,3))
x


###################################################
### code chunk number 25: intro.Rnw:480-486
###################################################
x <- c(1:10)
x
x <- append(x, 11)
x
x <- append(x, x)
x


###################################################
### code chunk number 26: intro.Rnw:500-503
###################################################
x <- c(1:20)
x[4]
x[4:15]


###################################################
### code chunk number 27: intro.Rnw:508-511
###################################################
x[-4:-10]
x <- x[-4]
x


###################################################
### code chunk number 28: intro.Rnw:516-520
###################################################
x <- c(1:20)
y <- c(3, 5, 12)
x[y]
x[-y]


###################################################
### code chunk number 29: intro.Rnw:525-530
###################################################
x <- c(1:5,2,2,2,2,2)
x[x > 4]
x[x < 4]
which(x < 4)
which(x == 2)


###################################################
### code chunk number 30: intro.Rnw:545-553
###################################################
 x <- c(1:10)

 x/2
 
 x-2
 
 log10(x)
 


###################################################
### code chunk number 31: intro.Rnw:559-566
###################################################
 x <- c(1:10)
 y <- c(1:5,2,2,2,2,2)

 x+y

 x/y



###################################################
### code chunk number 32: intro.Rnw:582-593
###################################################
 x <- c(1,3,5,7,20,2,8,4,4,10,2,2,1,1,15,3)

 head(x)

 max(x)

 unique(x)

 sort(x)

 mean(x)


###################################################
### code chunk number 33: intro.Rnw:623-624
###################################################
length(unique(x))


###################################################
### code chunk number 34: intro.Rnw:629-634
###################################################
 x <- c(1:20)

 sample(x)

 sample(x, replace = T)


###################################################
### code chunk number 35: intro.Rnw:652-654
###################################################
 y <- c("Escherichia","Chlamydia","Bacillus","Brachyspira","Leptonema")
 y


###################################################
### code chunk number 36: intro.Rnw:659-663
###################################################

 x <- c(1:5)
 names(x) <- y
 x


###################################################
### code chunk number 37: intro.Rnw:668-671
###################################################

 names(x) <- c("Escherichia","Chlamydia","Bacillus","Brachyspira","Leptonema")
 x


###################################################
### code chunk number 38: intro.Rnw:687-690
###################################################
x <- c(1:25)
y <- matrix(x, nrow=5)
y


###################################################
### code chunk number 39: intro.Rnw:695-697
###################################################
z <- matrix(x, byrow=T, nrow=5)
z


###################################################
### code chunk number 40: intro.Rnw:708-709
###################################################
z[2,5]


###################################################
### code chunk number 41: intro.Rnw:714-716
###################################################
z[2,]
z[,5]


###################################################
### code chunk number 42: intro.Rnw:733-737
###################################################
z
z*2
z+2
z*z


###################################################
### code chunk number 43: intro.Rnw:742-744
###################################################
z%*%z
z%/%y


###################################################
### code chunk number 44: intro.Rnw:749-753
###################################################
max(z)
mean(z)
mean(z[2,])
sample(z[2,], replace=T)


###################################################
### code chunk number 45: intro.Rnw:758-761
###################################################
dim(z)
t(z)  #transpose
colMeans(z)


###################################################
### code chunk number 46: intro.Rnw:802-807
###################################################
sums <- rowSums(z)
z2 <- rbind(z, sums)
z2
z2 <- cbind(z, sums)
z2


###################################################
### code chunk number 47: intro.Rnw:812-818
###################################################
data <- matrix(c(0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1), nrow=5)
cols <- c("Escherichia","Chlamydia","Bacillus","Brachyspira","Leptonema")
rows <-c("Ovis", "Canis", "Felis", "Capra", "Sus")
colnames(data) <- cols
rownames(data) <- rows
data


###################################################
### code chunk number 48: intro.Rnw:829-839
###################################################
#first create some data vectors
disease <- c("Cpox","ADisentary","Malaria","Flu","Plague")
agent <- c("Viral","Protozoan","Protozoan","Viral","Bacterial")
trans <- c(1,0,0,1,0)
vector <- c(0,0,1,0,1)
vir <- c(4,4,3,2,5)

#now combine them into a data frame
diseases <- data.frame(disease,agent,trans,vector,vir)
diseases


#now combine them into a data frame, this time assigning names
diseases <- data.frame(name=disease,type=agent,tmode=trans,vecborne=vector,virulence=vir)
diseases


###################################################
### code chunk number 49: intro.Rnw:844-845
###################################################
str(diseases)


###################################################
### code chunk number 50: intro.Rnw:850-853
###################################################
diseases[,2]

diseases$type


###################################################
### code chunk number 51: intro.Rnw:863-865
###################################################
stuff <- list(1:10, z2, diseases)
str(stuff)


###################################################
### code chunk number 52: intro.Rnw:870-872
###################################################
x <- stuff[[1]]
x


###################################################
### code chunk number 53: intro.Rnw:877-880
###################################################
stuff <- list(numbers=1:10, matrix=z2, dis=diseases)
stuff[[3]]
stuff$dis


###################################################
### code chunk number 54: intro.Rnw:898-906
###################################################
Rounder <- function(x){
   #this function uses the floor function to 
   #do what the round function does
   x <- x+0.5
   floor(x)
 }

ls()


###################################################
### code chunk number 55: intro.Rnw:912-915
###################################################
Rounder(3.1)
Rounder(3.7)
Rounder(z2*3.8)


###################################################
### code chunk number 56: intro.Rnw:926-946
###################################################
#The same function as above written following Google style guidelines:

Rounder2 <- function(x){
   #this function uses the floor function to 
   #do what the round function does
   #
   #Args:
   #    x: x must be of type numeric
   #
   #Returns:
   #   x or all elements of x rounded to the nearest integer
   #
   #Error handling
   if (is.numeric(x)==FALSE){
     stop("x is not of type numeric")
   }
   x <- x+0.5
   y <- floor(x)
   return(y)
 }


###################################################
### code chunk number 57: intro.Rnw:962-973
###################################################
# A simple function that needs functions from the ape library to run
# ape is a library of functions for analysis of evolutionary relationships:

RandTreePlotter <- function(x){
   #this function creates a plot of a random evolutionary tree
   #of size x
   require(ape)
   tree <- rcoal(x)
   plot(tree)
 }
RandTreePlotter(20)


###################################################
### code chunk number 58: intro.Rnw:992-999
###################################################
x <- 8

if (x < 9) print("x is less than nine")
 
x <- 10

if (x < 9) print("x is less than nine")


###################################################
### code chunk number 59: intro.Rnw:1009-1019
###################################################
Isitaten <- function(x){
 if (x == 10)
 print("It's a ten!")
 else print("Not a ten")
}


Isitaten(11)

Isitaten(10)


###################################################
### code chunk number 60: intro.Rnw:1030-1033
###################################################
for (i in 10:20) print(2*i)
y <- c(2.3, 4.4, 3.7, 1.2)
for (i in y) print(2*i)


###################################################
### code chunk number 61: intro.Rnw:1045-1052
###################################################
y <- 1
while(y < 10){
 print(y)
 y <- y+1
}

print("Thank goodness that's done")


