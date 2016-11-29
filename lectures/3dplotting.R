
#just create a range of values
grdVals <- function(x, bound=0.01) seq(range(x)[1]-bound, range(x)[2]+bound, length.out=50)

#instantiate a new persp object for a scatterplot
makeScatterBox <- function(x,y,z, bound=0, ...){
  x <- grdVals(x, bound=bound)
  y <- grdVals(y, bound=bound)
  z <- grdVals(z, bound=bound)
  z <- matrix(rep(z, 50), nrow=50)
  p <- persp(x,y,z, col=NA, border=NA, ...)
  
  p
  
}

#make a scatterplot of points
#and either create a new persp object
#or use one already existing
scatterPlot3d <- function(x,y,z, pch=19, cex=1, col="black", bound=1, background=NA, ...){
  if(!is.na(background[1])){
    p <- background
  }else{
    p <- makeScatterBox(x,y,z, background=background, ...)
  }
  points(trans3d(x,y, z, p), pch=pch, col=col, cex=cex)
  
  
}

#Create a surface from an LM or GLM with no more than 2 predictors
#need to add an a, b, c
abcSurf <- function(obj, type="response", xlab=names(obj$model)[2], ylab=names(obj$model)[3], zlab=names(obj$model)[1], 
                    ticktype="detailed", ...){
  x <- grdVals(obj$model[2])
  y <- grdVals(obj$model[3])

  ndf <- expand.grid(x,y)
  names(ndf) <- names(obj$model)[-1]
  
  z <- predict(obj, ndf)
  z <- matrix(z, nrow=length(x))
  
  persp(x,y,z, xlab=xlab, ylab=ylab, zlab=zlab,  ticktype=ticktype, ...)
}

# 
# ###### Demo Code
# x <- 1:100
# y <- runif(100, 0,100)
# z <- rnorm(100, x*3+y+x*y)
# 
# scatterPlot3d(x,y,z)
# 
# myFit <- lm(z ~ x*y)
# 
# abcSurf(myFit) -> p
# scatterPlot3d(x,y,z, add=T, background=p)


