####################################
## Author: Your name             ###
## Date: 08.18.17                ###
## Purpose: Simple SIR plotting  ###
####################################

#the plotting code assumes the SIR simulation was ran 
#out.SIR <- as.data.frame(lsoda(xstart.SIR,times.SIR,SIRModel,params.SIR))  

with(out.SIR,plot(S~time,type="o",col="blue",ylim=c(0,1),ylab="S (blue), I (red), R (green)"), pch=16)
with(out.SIR,lines(I~time,type="o",col="red"), pch=16)
with(out.SIR,lines(R~time,type="o",col="green"), pch=16)

## Making the plots snazier

library(ggplot2)

library(gridExtra) # load package

library(magrittr)

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

mdata <- out.SIR %>% melt(id=c("time"))# this will reshape the data using pipping
names(mdata)<-c("time", "Comp", "value")
mycols<-c( "blue","red","green")

p <- ggplot(data = mdata, mapping = aes(x = time, y = value, color = Comp))
p <- p + theme_bw() + labs(x = "time", y = "Population")
p<- p + scale_colour_manual(values = mycols)
p <- p + geom_line() 
p

