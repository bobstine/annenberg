### Annenberg Lecture 3

setwd("/Users/bob/work/courses/annenberg/data/")


#--------------------------------------------------
# Anscombe
#--------------------------------------------------

Anscombe <- read.table("anscombe.txt", header=TRUE)

summary(lm(Y1 ~ X1))
summary(lm(Y2 ~ X2))
summary(lm(Y3 ~ X3))
summary(lm(Y4 ~ X4))

par(mfrow=c(2,2))
  plot(Y1 ~ X1); abline(3, .5)
  plot(Y2 ~ X2); abline(3, .5)
  plot(Y3 ~ X3); abline(3, .5)
  plot(Y4 ~ X4); abline(3, .5)
par(mfrow=c(1,1))



#--------------------------------------------------
# Utopian data
#--------------------------------------------------

n <- 25000
x <- rnorm(n, mean=100, sd=10)
e <- rnorm(n, mean=0  , sd=15)
y <- 10 + 3 * x + e

plot(x,y)

# fit the regr, then add a line to the plot
regr <- lm(y ~ x); summary(regr)
abline(regr, col=2)

# generate a residual plot by hand by extracting from model
names(regr)
residuals(regr)
plot(x, residuals(regr))
abline(h=0, col=2)

# now let R do it automatically (lots of plots, including quantile)
par(mfrow=c(2,2))
  plot(regr)
par(mfrow=c(1,1))

# use Fox's car package
library(car)
qq.plot(regr$residuals)


#--------------------------------------------------
# Nonlinear model for Canadian Occ data
#--------------------------------------------------

data(Prestige)
colnames(Prestige)
attach(Prestige)

par(mfrow=c(1,2)) # 1 row of two plots
  hist(prestige); hist(income)
par(mfrow=c(1,1)) # reset

plot(income, prestige)
abline(lm(prestige~income), lty=2)  # black, dashed

# fit lines to subsets
abline(lm(prestige~income, data=Prestige[income<10000,]), col=2)
abline(lm(prestige~income, data=Prestige[income>11000,]), col=3)

par(mfrow=c(1,2)) 
  hist(prestige); hist(log(income))
par(mfrow=c(1,1)) 

# fit model using log income, add to plot
plot(log(income), prestige)
identify(log(income), prestige, labels=rownames(Prestige))

log.regr<- lm(prestige~log(income))
abline(log.regr)
plot(log.regr)

# add fit to plot on original scales as blue points
points(income, log.regr$fitted.values, col=4)

# add the curve
plot(income, prestige)
x<- seq(min(income), max(income), 100)  # values to connect
lines(x,-139.86 + 21.56*log(x), col=3)

# fancy way to add the curve plots a function 
plot(function(x)(-139.86 + 21.56*log(x)), xlim=range(income), col=2)
points(income, prestige)


# reciprocal model is better?
plot(1/income, prestige)             # two very serious outliers
recip <- 1/income
recip.regr<- lm(prestige~recip)
abline(recip.regr)





#--------------------------------------------------
#  Display example: another example of logs
#--------------------------------------------------

Display <- read.table("display.txt", header=TRUE)

plot(display.feet, sales)



#--------------------------------------------------
# Real-estate data
#--------------------------------------------------

RealEstate <- read.table("real_estate.txt", header=TRUE)
names(RealEstate)
attach(RealEstate)

par(mfrow=c(1,2))  
  hist(rent.total); hist(sqft)
par(mfrow=c(1,1))

plot(sqft, rent.total)
regr <- lm(rent.total ~ sqft)
summary(regr)
abline(regr, col=2)

plot(sqft, regr$residuals); abline(h=0, col=2)  # by hand
plot(regr)           # built in regr diagnostics

# use average costs instead
plot(1/sqft, rent.total/sqft)
identify(1/sqft, rent.total/sqft)  # who are the outliers 
edit(RealEstate)

RealEstate[c(168, 170),]


outliers <- c(168,170)
rent.avg <- (rent.total/sqft)[-outliers] # negative index excludes outliers
recip.sqft <- 1/sqft[-outliers]
plot(recip.sqft, rent.avg)

regr.out <- lm(rent.avg ~ recip.sqft)
summary(regr.out)
abline(regr.out, col=2)

plot(recip.sqft, residuals(regr.out)); abline(h=0, col=2)

qq.plot(regr.out$residuals)


