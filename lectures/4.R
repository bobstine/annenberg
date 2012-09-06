### Annenberg Lecture 4

setwd("/Users/bob/work/courses/annenberg/data/")
library(car)

#--------------------------------------------------
# Philadelphia data
#--------------------------------------------------

Phila <- read.table("phila.txt", header=TRUE)
names(Phila)
attach(Phila)

edit(Phila)

par(mfrow=c(1,2))
  hist(crime.rate); hist(house.price)
par(mfrow=c(1,1))

# can use equations to organize analysis

eqn <- house.price ~ crime.rate
plot(eqn)

identify(crime.rate, house.price, labels=name)

scatterplot(eqn)    #  from car package

# fit line with and without this county
plot(eqn, data=Phila)
regr <- lm(eqn, data=Phila); abline(regr, col=2)
identify(crime.rate, house.price, labels=name)
names[70]

regr.out <- lm(eqn,data=Phila[-70,]); 
abline(regr.out, col=3)

# plot residuals
plot(crime.rate, residuals(regr))
abline(h=0, col=2)

# plot regression diagnostics
par(mfrow=c(2,2))
  plot(regr)
par(mfrow=c(1,1))

# plot data without outlier
plot(eqn, data=Phila[-70,])
regr.subset <- lm(eqn, data=Phila[-70,])
abline(regr.subset, col=2)

scatterplot(eqn, data=Phila[-70,])
plot(regr.subset)

# plot these residuals
plot(crime.rate[-70], residuals(regr.subset)); abline(h=0,col=2)

# try a model with reciprocal
Phila.nocc <- Phila[-70,]
attach(Phila.nocc)
people <- 1/crime.rate

plot(people, house.price)
regr <- lm(house.price ~ people)
abline(regr)
summary(regr)

detach(Phila.nocc)

attach(Phila)
plot(crime.rate, house.price)
x <- seq(1, 370, .1)
lines(x, 98120 + 1298243 * 1/x, col=5)


#--------------------------------------------------
# Duncan's occupation data
#--------------------------------------------------

data(Duncan)        # in car package
names(Duncan)
rownames(Duncan)

attach(Duncan)

par(mfrow=c(1,3))
  hist(prestige); hist(income); hist(education)
par(mfrow=c(1,1))

eqn <- prestige ~ income + education
plot(eqn)

par(mfrow=c(1,2))
  plot(eqn)
par(mfrow=c(1,1))

pairs(eqn)
cor(Duncan, use="complete.obs")
cor(Duncan[,c(4,2,3)], use="complete.obs")

scatterplot.matrix(Duncan[,c(4,2,3)])

# two simple regressions
regr.ed <- lm (prestige ~ education); summary(regr.ed)
regr.in <- lm (prestige ~ income)   ; summary(regr.in)


# conditioning plots
coplot(prestige ~ income    | education, row=1)
coplot(prestige ~ education | income   , row=1)


# glue together into one multiple regression
regr <- lm(eqn)
summary(regr)

# anova summary of the regression
anova(regr)
aov(regr) # yuk

# plot for the overall F
plot(fitted.values(regr), prestige); 
abline(0,1, col=2)
cor(fitted.values(regr), prestige)^2  # this is R^2

# residual plots
plot(regr)

summary(  lm(prestige ~ fitted.values(regr))  )
plot(fitted.values(regr), residuals(regr)); abline(h=0, col=2)

par(mfrow=c(1,2))
  plot(income, residuals(regr)); abline(h=0, col=2)
  plot(education, residuals(regr)); abline(h=0, col=2)
par(mfrow=c(1,1))

# normal quantile plot
qq.plot(residuals(regr))

# use fox's tools for added-variable (aka leverage) plots
par(mfrow=c(1,2))
	av.plots(regr, labels=rownames(Duncan))
par(mfrow=c(1,1)) 
# by hand
res.prestige  <- residuals( lm(prestige ~income) )
res.education <- residuals( lm(education~income) )
plot(res.prestige ~ res.education)
summary(lm(res.prestige ~ res.education))

# standard residual plots
plot(regr)

# and component plus residual plots
par(mfrow=c(1,2))
	cr.plots(regr)
par(mfrow=c(1,1))


# Are the slopes significantly different?
summary(regr)

linear.hypothesis(regr, c(0,1,-1))

# alternatively reformulate the model so that you can see the
# difference as a coefficient
ed.in <- education + income
summary(
	regr.alt <- lm(prestige ~ education + ed.in))
	
par(mfrow=c(1,2))
	av.plots(regr.alt)
par(mfrow=c(1,1))

# remove the minister occupation
summary( regr.nomin <- lm(prestige ~ education + income, data=Duncan[-6,]) )
linear.hypothesis(regr.nomin, c(0,1,-1))


# component plus residual plots, back to original model
par(mfrow=c(1,2))
	cr.plots(regr, labels=rownames(Duncan))
par(mfrow=c(1,1))

# see any outliers?