### Annenberg Lecture 1

#--------------------------------------------------
# Simulation of weekly running poll
#--------------------------------------------------

pop.prop.dem <- 0.6
sample.size <- 50

# count.dem <- rbinom (20,size=sample.size,prob=pop.prop.dem)
count.dem <- c(33,31,24,32,33,35,34,33,21,30,28,29,31,25,24,26,36,32,29,28)

prop.dem <- count.dem/sample.size
plot(prop.dem, xlab="Week", ylab="Proportion Dem", ylim=c(0,1))

# now focus the plot

plot(prop.dem, xlab="Week", ylab="Proportion Dem", ylim=c(0.4,0.75))
abline(0.6,0, col=2, lty=3)

# visual test for simplicity: random rearrangement of the data

indices <- sample(1:20,20)
plot(prop.dem[indices], xlab="Week", ylab="Proportion Dem", ylim=c(0.4,0.75))
abline(0.6,0, col=2, lty=3)

# add a histogram to the plot

hist(prop.dem, ylim=c(0,6)); rug(prop.dem); 
boxplot(prop.dem, add=TRUE, horizontal=TRUE, at=5.5)

# draw the normal density

xax <- seq(-4,4,0.1)
plot(xax, dnorm(xax),type="l", col=2,
	 main="Normal Density", xlab="x", ylab="Density")

# add boundaries

plot(prop.dem, xlab="Week", ylab="Proportion Dem", ylim=c(0.3,0.9))
abline(0.6-  0.07,0, col=2, lty=3)
abline(0.6+  0.07,0, col=2, lty=3)
abline(0.6-2*0.07,0, col=3, lty=4)
abline(0.6+2*0.07,0, col=3, lty=4)
abline(0.6-3*0.07,0, col=4, lty=2)
abline(0.6+3*0.07,0, col=4, lty=2)


#--------------------------------------------------
# Quick introduction to R
#--------------------------------------------------

# Command-line interpreter

2+4
sqrt(10)
2:30

# Start with a blank slate

ls()

# Generate a sample from a normal population

norm <- rnorm(30, mean=10, sd=2)
norm
ls()

# Basic summary statistics

summary(norm)          # "five-number" summary
mean(norm)
sd(norm)               # R tends to use abbreviated names
hist(norm)             # Need to open graphical window


# simulation of coin tossing for sampling distribution

n <- 400
x<-rbinom(2000, size=n, prob=pop.prop.dem)/n
hist(x, xlim=c(0.5,0.7), breaks=30, probability=TRUE,
            main=paste("Sampling Distribution, n = ",n), 
            xlab="sample prop")
            
# draw normal

xax <- seq(0.5, 0.7, 0.005)            
lines(xax, dnorm(xax, mean=0.6, sd=sqrt(0.6*0.4/400)), col=2)

#--------------------------------------------------
# Osteoporosis Test Example
#--------------------------------------------------

setwd("/Users/bob/work/courses/annenberg/data/")

# read the data

osteo.data <- read.table("osteo.txt", header=TRUE)
summary(osteo.data)
edit(osteo.data)

attach(osteo.data)

# do a one-sample test

t.test(hip, mu=-1)

# default normal quantile plot
hist(hip, breaks=10, probability=TRUE)

xax<- seq(-6,2,.2)
lines(xax, dnorm(xax, mean=mean(hip), sd=sd(hip)), col=2, lty=3)

qqnorm(hip)

# How close is close enough?

norm <- rnorm(30, mean=10, sd=2)
qqnorm(norm)                 # Careful! qnorm does something different
abline(mean(norm),sd(norm))  # Draw line with intercept and slope

norm <- rnorm(30, mean=10, sd=2)
qqnorm(norm)                 
abline(mean(norm),sd(norm)) 

# get fox code for better normal quantile plot

library(car)
qq.plot(hip)

# more examples with simulated data

qq.plot(rnorm(50,mean=20,sd=5))

skew <- rgamma(50,shape=1)
qq.plot(skew)
hist(skew)
# look at both together
par(mfrow=c(1,2))
    qq.plot(skew)
    hist(skew)
par(mfrow=c(1,1))
 

#--------------------------------------------------
# Inverted test confidence interval (Wilson, 1927)
#--------------------------------------------------

data <- c(rep(1,40), rep(0,10))

p0 <- 0.9
se.p0 <- sqrt(p0 * (1 - p0) / 50)

p.hat<- mean(data)
se.hat <- sqrt(p.hat * (1-p.hat) / 50)

(p.hat-p0)/se.p0

(p.hat-p0)/se.hat
p.hat + c(-2,2) * se.hat

scoreci <- function(x,n,lev=0.95)  {
  zalpha <- abs(qnorm((1-lev)/2))
  phat <- x/n
  bound <- (zalpha*((phat*(1-phat)+(zalpha**2)/(4*n))/n)**(1/2))/
           (1+(zalpha**2)/n)
  midpnt <- (phat+(zalpha**2)/(2*n))/(1+(zalpha**2)/n)
  uplim <- round(midpnt + bound,digits=4)
  lowlim <- round(midpnt - bound,digits=4)
  results <- data.frame(lowlim,uplim)
  cat("\n")
  cat("With confidence level",lev," and sample proportion",
     round(phat,digits=4),
     " \nthe lower and upper limits for the score confidence interval are: \n")
  cat("\n")
  print(results)
  cat("\n")
  }