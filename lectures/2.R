### Annenberg Lecture 2

setwd("/Users/bob/work/courses/annenberg/data/")

#--------------------------------------------------
# Osteoporosis, Two Sample Analysis
#--------------------------------------------------

# read the data

osteo.data <- read.table("osteo.txt", sep=""),header=TRUE)
attach(osteo.data)

hip.estrogen <- hip[estrogen=="yes"]
hip.none     <- hip[estrogen=="no"]

# basic plots

boxplot(hip.estrogen, hip.none, names = c("yes", "no"))

par(mfrow=c(1,2))
	hist(hip.estrogen); hist(hip.none)
par(mfrow=c(1,1))

# test

t.test(hip.estrogen, hip.none)


#--------------------------------------------------
# Osteoporosis, Much larger sample
#--------------------------------------------------

detach (osteo.data)
osteo.data <- read.table("osteo_1109.txt", sep=""),header=TRUE)
attach(osteo.data)

# repeat prior analysis, just with more data

hip.estrogen <- hip[estrogen=="yes"]
hip.none     <- hip[estrogen=="no"]

# plots are basically the same, just more detail

boxplot(hip.estrogen, hip.none, names = c("yes", "no"))

par(mfrow=c(1,2))
	hist(hip.estrogen); hist(hip.none)
par(mfrow=c(1,1))

# test

t.test(hip.estrogen, hip.none)

#--------------------------------------------------
# Bivariate normal example for covariance
#--------------------------------------------------


x <- rnorm(100, mean=10, sd = 5)
y <- 100 + 2 * x + rnorm(100, mean=0, sd=10)
plot(x,y)

abline(h=mean(y), col=2, lty=3)
abline(v=mean(x), col=3, lty=4)

cov(x,y)
cor(x,y)

#--------------------------------------------------
# Aggregation effect on correlation
#--------------------------------------------------

# simulate the 'micro' level data
x <- rnorm(100)
y <- x + rnorm(100)  # var of y = 1 + 1, half from x, half from noise

cor(x,y); plot(x,y)

# define towns based on similar values of x
town <- factor(round(rank(x)/5))

# find (mean(x), mean(y)) for each town
x.mean <- sapply(split(x,town),mean)
y.mean <- sapply(split(y,town),mean)

# plot points and 'towns'
plot(x,y)
points(x.mean, y.mean, cex=3, col=2)

cor(x.mean, y.mean)

# measurement error
ex <- rnorm(100);
ey <- rnorm(100)
cor(x+ex, y+ey)
cor(x, y)
#--------------------------------------------------
# Canadian prestige data from Fox (part of CAR)
#--------------------------------------------------

library(car);
data(Prestige)

dimnames(Prestige)
attach(Prestige)

plot(income, prestige)   # x and y
cor(income, prestige)

identify(income, prestige)

plot(income, prestige)   # add better labels
identify(income, prestige, labels=rownames(Prestige))

# smoothing with lowess

lines(lowess(income, prestige), f=2/3)  # very smooth

lines(lowess(income, prestige, f=1/5), col=2) # rougher

lines(lowess(income, prestige, f=1/20), col=3) # roughest

# fox's version in car package
scatterplot(income, prestige)


#--------------------------------------------------
# Sahlins
#--------------------------------------------------

library(car)     # make sure this package is loaded
data(Sahlins)    # load the data frame
edit(Sahlins)

attach(Sahlins)
plot(consumers, acres)

lines(lowess(consumers, acres, f=1/2), col=2) 

cor(consumers, acres)

# force to be a line
lines(lowess(consumers, acres, f=1), col=3) 

# add a line, the least squares line
plot(consumers, acres)

eqn <- acres ~ consumers
plot(eqn, xlim=c(0,2.4))
regr <- lm(eqn)

names(regr)
regr   # type its name to get a short summary
abline(regr, col=2)

# change units

sqft <- 43560 * acres
new.eqn <- sqft ~ consumers
plot(new.eqn)
new.regr <- lm(new.eqn)
abline(new.regr)
new.regr
cor(sqft, consumers)


#--------------------------------------------------
# Diamonds
#--------------------------------------------------

Diamonds <- read.table("singapore_diamonds.txt", header=TRUE)

attach(Diamonds)

plot(weight,price)

# fit a line
regr <- lm(price ~ weight); summary(regr)

abline(lm(price ~ weight), col=2)
regr  # prints summary

plot(weight,price, xlim=c(0, .35), ylim=c(-260, 1200))


#--------------------------------------------------
# Robey analysis with a function
#--------------------------------------------------

my.regr.plot <- function(x, y) {
	par(mfrow=c(2,1));  # arrange plot regions
	plot(x,y)
	regr<- lm(y~x)
	abline(regr, col=2)
	plot(x,residuals(regr))
	abline(h=0, col=2)
	par(mfrow=c(1,1))    # reset plot regions
	}

data(Robey)
attach(Robey)
my.regr.plot(contraceptors, tfr)
detach(Robey)

Diamonds <- read.table("singapore_diamonds.txt", header=TRUE)
attach(Diamonds)
my.regr.plot(weight, price)

