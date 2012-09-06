### Exercise 3 script

setwd("/Users/bob/work/courses/annenberg/data/")
library(car)

###  Read data, look it over

Cars <- read.table("newer_cars.txt", header=TRUE)

names(Cars); dim(Cars)

attach(Cars)

###  Histograms
par(mfrow=c(1,3))
  hist(citympg); hist(weight); hist(hp)
par(mfrow=c(1,1))
 
###  Smoothed histograms
par(mfrow=c(1,3))
  plot(density(citympg)); rug(citympg);
  plot(density(weight)); rug(weight);
  plot(density(hp)); rug(hp);
par(mfrow=c(1,1))

### scatterplot matrix

dim(cbind(citympg, weight, hp))  # make a matrix of the variables

pairs( cbind(citympg, weight, hp) )  # R, put the variables together
scatterplot.matrix( cbind(citympg, weight, hp) ) # car version

###  transformation
gal.per.1000mi <- 1000/citympg

scatterplot.matrix( cbind(gal.per.1000mi, weight, hp) ) # car version

### first simple regression
eq.1 <- gal.per.1000mi ~ weight
plot(eq.1)
summary(model.1 <- lm(eq.1)); abline(model.1, col=2)

### a way to think about R2... start with crummy predictor
noise <- rnorm (93)
eq.weak <- gal.per.1000mi ~ noise
plot(eq.weak)
summary(model.weak <- lm(eq.weak)); abline(model.weak, col=2)

# compare variation in data to that from the model
par(mfrow=c(2,1))
  plot(density(gal.per.1000mi), xlim=c(10,80)); var(gal.per.1000mi)
  plot(density(fitted.values(model.weak)), xlim=c(10,80)); var(fitted.values(model.weak))
par(mfrow=c(1,1))

# now for the real predictor
par(mfrow=c(2,1))
  plot(density(gal.per.1000mi), xlim=c(10,80)); var(gal.per.1000mi)
  plot(density(fitted.values(model.1)), xlim=c(10,80)); var(fitted.values(model.1))
par(mfrow=c(1,1))
  
# visual look at the Se
par(mfrow=c(2,1))
  plot(density(gal.per.1000mi)); var(gal.per.1000mi)
  plot(density(residuals(model.1))); var(residuals(model.1))
par(mfrow=c(1,1))
	
### two-predictor model
eq.2 <- gal.per.1000mi ~ weight + hp
# plot(eq.2)
summary(model.2 <- lm(eq.2)); 

###  check for constant variance
plot( fitted.values(model.2),residuals(model.2) ); abline(h=0, col=2)

# who are the outliers
identify( fitted.values(model.2),residuals(model.2) ,labels=model)
# normal?
abline(h=8.4, col=5); abline(h=-8.4, col=5); 
qq.plot(residuals(model.2))

# diagnostic plot for predictors, R version
par(mfrow=c(1,2))
  av.plot( model.2, weight )
  av.plot( model.2, hp )
par(mfrow=c(1,1))

# from car
par(mfrow=c(1,2))
  av.plots( model.2, labels=model)
par(mfrow=c(1,1))
# zoom in
av.plot( model.2, hp ,  labels=model)\

# standard residual plots
par(mfrow=c(2,2))
  plot(model.2)
par(mfrow=c(1,1))

# check for curvature in the multiple regression
par(mfrow=c(1,2))
  cr.plots( model.2, labels=model)
par(mfrow=c(1,1))

# conditioning plots
coplot( gal.per.1000mi ~ weight | hp    , row=1)
coplot( gal.per.1000mi ~ ho     | weight, row=1)


