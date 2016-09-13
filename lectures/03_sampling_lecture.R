################################################
### R Code for Lecture 3 - Sampling populations
###   
###
### Last Modified 9/7/2012
###
### Changelog:
### 
################################################

## @knitr lecture3prep
library(ggplot2)

########------------------------
### Populations and Samples
########------------------------

## @knitr population
#setup a 'population' for visualization
popMax<-100
set.seed(30)
popData<-data.frame(x=runif(popMax,1,popMax), 
                    y=runif(popMax,1,popMax), 
                    colorSize1 = runif(popMax,.01,popMax/10),
                    colorSize2 = runif(popMax,.01,popMax/10))


aPop<- qplot(x, y, size=colorSize1, colour=colorSize2, data=popData) + 
  theme_void() +xlab("")+ylab("") +
  scale_color_gradientn(guide=FALSE, colours = c("grey", rainbow(15), "black")) + 
  scale_size_continuous(guide=FALSE, range=c(2,10))

aPop

## @knitr sample
# A population with one "quadrat"
aPop+geom_rect(aes(xmin=4, xmax=34, ymin=4, ymax=34), colour="red", lwd=1, fill=NA)

## @knitr sampleSpread
# A population with one "quadrats" sampling
aPop +geom_rect(aes(xmin=4, xmax=24, ymin=4, ymax=24), colour="red", lwd=1, fill=NA) +
      geom_rect(aes(xmin=8, xmax=28, ymin=56, ymax=76), colour="red", lwd=1, fill=NA) +
      geom_rect(aes(xmin=45, xmax=65, ymin=75, ymax=95), colour="red", lwd=1, fill=NA) +
      geom_rect(aes(xmin=74, xmax=94, ymin=35, ymax=55), colour="red", lwd=1, fill=NA)


## @knitr spatialBias
#create pattern of association with a little noise
set.seed(45)
extraNoise<-runif(popMax, 0,popMax/10)
spatialPlot<-qplot(x, colorSize1*10+extraNoise, size=colorSize1, colour=colorSize2, data=popData) + 
  theme_void() +xlab("")+ylab("") +
  scale_color_gradientn(guide=FALSE, colours = c("grey", rainbow(15), "black")) + 
  scale_size_continuous(guide=FALSE, range=c(1,10))

spatialPlot

## @knitr spatialSample
spatialPlot+geom_rect(aes(xmin=4, xmax=34, ymin=4, ymax=34), colour="red", lwd=1, fill=NA)

## @knitr spatialSample2
#many plots
spatialPlot+geom_rect(aes(xmin=4, xmax=24, ymin=4, ymax=24), colour="red", lwd=1, fill=NA) +
  geom_rect(aes(xmin=8, xmax=28, ymin=56, ymax=76), colour="red", lwd=1, fill=NA) +
  geom_rect(aes(xmin=45, xmax=65, ymin=75, ymax=95), colour="red", lwd=1, fill=NA) +
  geom_rect(aes(xmin=74, xmax=94, ymin=35, ymax=55), colour="red", lwd=1, fill=NA)


## @knitr stratified
#many plots
spatialPlot+geom_rect(aes(xmin=40, xmax=60, ymin=0, ymax=20), colour="red", lwd=1, fill=NA) +
  geom_rect(aes(xmin=40, xmax=60, ymin=25, ymax=45), colour="red", lwd=1, fill=NA) +
  geom_rect(aes(xmin=40, xmax=60, ymin=50, ymax=70), colour="red", lwd=1, fill=NA) +
  geom_rect(aes(xmin=40, xmax=60, ymin=75, ymax=95), colour="red", lwd=1, fill=NA)


## @knitr colorSize
#create a pattern of color-size association
csPop<-qplot(x, y, size=colorSize1, colour=colorSize1, data=popData) + 
  theme_void() +xlab("")+ylab("") +
  scale_color_gradientn(guide=FALSE, colours = c("grey", rainbow(15), "black")) + 
  scale_size_continuous(guide=FALSE, range=c(3,10))

csPop

## @knitr samplePath
#sort popData by y
popDataSort<-popData[sort(popData$x, index.return=T)$ix,]
aPop + geom_line(data=popDataSort[which(popDataSort$y>50),][1:20,], aes(x=x, y=y), colour="black", lwd=1)

## @knitr samplePath2
#sort popData by y
popData<-popData[sort(popData$x, index.return=T)$ix,]
aPop + geom_line(data=popData[seq(1,100,5),], aes(x=x, y=y), colour="black", lwd=1)


########------------------------
### Describing Your Sample
########------------------------

## @knitr mean1
mean(c(1,4,5,10,15))

## @knitr mean2
mean(runif(n = 500, min = 0, max = 100))

## @knitr mean3
set.seed(5000)
population<-runif(400,0,100)
mean( sample(population, size=50) )


## @knitr exerciseMean1
set.seed(5000)
population<-runif(n = 400, min = 0, max = 100)
mean( sample(population, size=3) )
mean( sample(population, size=3) )


## @knitr exerciseMean2
mean( sample(population, size=100) )
mean( sample(population, size=100) )

## @knitr exerciseMean3
mean (c(mean( sample(population, size=3) ),
        mean( sample(population, size=3) ),
        mean( sample(population, size=3) ),
        mean( sample(population, size=3) ) ))

mean (c(mean( sample(population, size=50) ),
        mean( sample(population, size=50) ),
        mean( sample(population, size=50) ),
        mean( sample(population, size=50) ) ))



## @knitr exerciseSD
        sd( sample(population, size=3) )
        sd( sample(population, size=3) )
        sd( sample(population, size=3) )
        sd( sample(population, size=3) )

        sd( sample(population, size=50) )
        sd( sample(population, size=50) )
        sd( sample(population, size=50) )
        sd( sample(population, size=50) )
