### Annenberg Lecture 5

# Set the path and load Fox's tools here at the start

setwd("/Users/bob/work/courses/annenberg/data/")
library(car) 

#--------------------------------------------------
# Review examples from handouts
#--------------------------------------------------

Advertising <- read.table("ad_source.txt", header=TRUE)

edit (Advertising)  # skim over, then summarize
dim(Advertising)
summary(Advertising)

# Fox version does a lot more with the diagonal than 'pairs'
scatterplot.matrix(Advertising[,c(4,2,3,1)]) # put Y first, time last
cor(Advertising) #  short for correlations

# Regression models   Foxscatterplot is more embellished than plot
attach(Advertising)

eqn.1 <- sales ~ tv.adv; scatterplot(eqn.1)
regr.1 <- lm(eqn.1); 
summary(regr.1)

eqn.2 <- sales ~ tv.adv + print.adv
regr.2 <- lm(eqn.2); 
summary(regr.2)

# coplot view
coplot(sales ~ tv.adv | print.adv, data=Advertising, rows=1)
coplot(sales ~ print.adv | tv.adv, data=Advertising, rows=1, panel=panel.car)

# clean up
detach(Advertising)

###  Second Example

PDA <- read.table("pda.txt", header=TRUE)

edit (PDA)  # skim over, then summarize
dim(PDA)
summary(PDA)

# arrange to to put Y first
scatterplot.matrix(PDA[,c(3,1,2)])
cor(PDA)

# fit the models compactly  (never attached data either)
summary(lm(rating~age), data = PDA)
summary(lm(rating~income), data = PDA)
summary(lm(rating~age+income), data = PDA)

# coplot view to explain
coplot(rating~age | income, data=PDA, row=1, panel=panel.car)



#--------------------------------------------------
# Cars
#--------------------------------------------------

Cars <- read.table("cars.txt", header=TRUE)
names(Cars)
edit(Cars)

attach(Cars)

# effect of changing scales
par(mfrow=c(1,3))
	plot(weight,1/mpg.city)
	plot(weight,1000/mpg.city)
	plot(weight/2000,1000/mpg.city)
par(mfrow=c(1,1))


# initial histograms
par(mfrow=c(1,3))
  hist(1000/mpg.city); hist(weight); hist(hp)
par(mfrow=c(1,1))


# scatterplot matrix, fox version is scatterplot.matrix
eqn <- 1000/mpg.city ~ weight + hp
plot(eqn)
pairs(eqn)  # plot both, one at a time

scatterplot.matrix((1000/mpg.city), weight, hp) #dies
scatterplot.matrix(cbind((1000/mpg.city), weight, hp))


# correlation matrix; option forces to use only complete cases
cor(cbind(1000/mpg.city , weight , hp), use="complete.obs")


# initial simple regression
eqn.1 <- 1000/mpg.city ~ weight
plot(eqn.1)
regr.1 <- lm(eqn.1) ; abline(regr.1, col=2)

summary(regr.1)

plot(weight, residuals(regr.1)) ; abline(h=0, col=2)
identify(weight, residuals(regr.1), labels=model)


# now the multiple regression
eqn <- 1000/mpg.city ~ weight + hp
regr.2 <- lm(eqn)

# coplots of fit
gpm <- 1000/mpg.city

coplot(gpm ~ weight | hp,     row=1)
coplot(gpm ~ hp     | weight, row=1)

# overall fit and associated plot
summary(regr.2)
plot(fitted.values(regr.2), 1000/mpg.city); abline(0,1,col=2)

# usual regression diagnostics based on residuals and influence
plot(regr.2)

### Diagnostics for the coefficients
# added variable plots
par(mfrow=c(1,2))
	av.plots(regr.2)
par(mfrow=c(1,1))

# component+residual plots
par(mfrow=c(1,2))
	cr.plots(regr.2)
par(mfrow=c(1,1))

# residuals on predictors

par(mfrow=c(1,2))
	plot(weight, residuals(regr.2)); abline(h=0, col=2)
	plot(hp    , residuals(regr.2)); abline(h=0, col=2)
par(mfrow=c(1,1))

detach(Cars)

#--------------------------------------------------
# Duncan
#--------------------------------------------------

data(Duncan); attach(Duncan)

scatterplot.matrix(Duncan[,c(4,2,3)]) # omit categorical term

attach(Duncan)
eqn <- prestige ~ income + education

regr <- lm(eqn); summary(regr)
par(mfrow=c(2,2))
	plot(regr)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
	av.plots(regr)  # s gets the fox version
par(mfrow=c(1,1))

rownames(Duncan)[c(6,16,27)]

Duncan.subset <- Duncan[-c(6,16,27),]

summary(lm(eqn, data=Duncan.subset))



