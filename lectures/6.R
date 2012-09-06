### Annenberg Lecture 6

# usual startup to set path, load car library

setwd("/Users/bob/work/courses/annenberg/data/")
library(car)

#--------------------------------------------------
# Bonnie Fox's data on labor force
#--------------------------------------------------

data(Bfox)
names(Bfox)
edit(Bfox)

cor(Bfox)
scatterplot.matrix(Bfox[,c(1,2,4,6)])

# add year as "data column"
Bfox$year<-as.numeric(row.names(Bfox))

scatterplot.matrix(Bfox[,c(1,2,4,6,7)])

attach(Bfox)

# zoom in on time trends... ugly perspective
par(mfrow=c(2,1))
  plot(year, tfr);
  plot(year, parttime);
par(mfrow=c(1,1))

# show together by standardizing
plot(year, scale(tfr), ylim=c(-2.2,2.2))
points(year, scale(parttime), col=3)

# initial simple regression
eqn.1 <- partic ~ womwage
plot(eqn.1); lines(womwage, partic)
regr.1 <- lm(eqn.1)
abline(regr.1, col=2)

summary(regr.1)

# sequence plot of residuals
plot(year, residuals(regr.1)); abline(h=0, col=2)
lines(year, residuals(regr.1))

# two-predictor regression
eqn.2 <- partic ~ womwage + menwage

par(mfrow=c(1,2))
  plot(eqn.2); 
par(mfrow=c(1,1))

regr.2 <- lm(eqn.2); summary(regr.2)

# added variable plots (car version)
par(mfrow=c(1,2))
	av.plots(regr.2)
par(mfrow=c(1,1))

# component+residual plots (car version)
par(mfrow=c(1,2))
	cr.plots(regr.2)
par(mfrow=c(1,1))

###  Conditioning plots, uncorrelated predictors

# make up some data, using normal samples with no collinearity
x1 <- rnorm(100)
x2 <- rnorm(100)
y <- 10 + 1 * x1 + 1 * x2 + 1 * rnorm(100)
sim.regr <- lm(y ~ x1 + x2); summary(sim.regr)

coplot(y ~ x1 | x2, rows=1)

# note that there's no collinearity, so just as much variation in
# y on x1 for various values of x2
plot(x2,x1); 
abline(h= 1-.2, col=2); abline(h= 1+.2, col=2)
abline(h=-1-.2, col=4); abline(h=-1+.2, col=4)

###  Conditioning plots, Fox's data

# marginal view... lots of variation in predictor
plot(partic ~ womwage)

# partial view... its gone!
coplot(partic ~ womwage | menwage, rows=1)

###  Big model with all predictors

summary(big.regr <- lm(partic ~ tfr+menwage+womwage+debt+parttime))
vif(big.regr)

# rearrange the deck chairs
diff <- menwage-womwage;
avg  <- (menwage+womwage)/2;
summary(alt.regr <- lm(partic ~ tfr+diff+avg+debt+parttime))
vif(alt.regr)

# the use of . on right side of formula sayes to use all 
# possible predictors in the specified data frame

summary(bigger.regr <- lm(partic ~ ., data =Bfox))
vif(bigger.regr)


###  Try out a ridge regression from the MASS library

library(MASS)
?lm.ridge

# this output is not so interesting
r.regr <- lm.ridge(partic ~ tfr+menwage+womwage+debt+parttime)
summary(r.regr)

# draws the ridge trace
r.ridge <- lm.ridge(partic ~ tfr+menwage+womwage+debt+parttime,lambda=seq(0,3,.05))plot()


#----------------------------------------------------------------
#  Salary data 
#----------------------------------------------------------------

Salary <- read.table("salary.txt", header=TRUE)

summary(Salary)
attach(Salary)

# two-sample t-test (note == to test equality, not one '=')
boxplot(salary ~ sex)  # comparison boxplots for each level of sex
mean(salary[sex=="female"]) - mean(salary[sex=="male"])
t.test(salary[sex=="female"], salary[sex=="male"])

# check for being a confounding factor
par(mfrow=c(1,2))
	boxplot(mgmt.level ~ sex)  # comparison boxplots for each level of sex
	plot(salary ~ mgmt.level)
par(mfrow=c(1,1))

# t test on restricted subset
boxplot(salary ~ sex, data=Salary[mgmt.level==4,])
t.test(salary[sex=="female" & mgmt.level==4], 
       salary[sex=="male" & mgmt.level==4])

# separate lines & colors for the two groups(scatterplot adds legend)
scatterplot(salary ~ mgmt.level, groups=sex)

regr.female<-lm(salary ~ mgmt.level, data=Salary[sex=="female",])
regr.male  <-lm(salary ~ mgmt.level, data=Salary[sex=="male",])

regr.female; regr.male

summary(regr.female); summary(regr.male)



# conditioning plots
coplot(salary ~ mgmt.level | sex, row=1)
coplot(salary ~ sex | mgmt.level, row=1, panel=panel.car)



# contrasts show how the R encodes a categorical term
contrasts(sex) # default is dummy coding (aka, treatment coding in R)

contrasts(sex) <- contr.treatment(levels(sex), base=2) # move base
contrasts(sex) 

contrasts(sex) <- contr.sum(levels(sex)) # effect coding
contrasts(sex)

contrasts(sex) <- contr.treatment(levels(sex), base=1) # reset to default
contrasts(sex) 

# only a dummy predictor

summary(lm(salary ~ sex))               # only categorical

summary(lm(salary ~ sex + mgmt.level))  # mix

# fit with effect coding
contrasts(sex) <- contr.sum(levels(sex)) # effect coding
contrasts(sex)

summary(lm(salary ~ sex + mgmt.level))  # mix

# fit with interaction, dummy coding

contrasts(sex) <- contr.treatment(levels(sex), base=1) # reset to default

# with interaction (separate names by colon)
summary(fit<- lm(salary ~ sex + mgmt.level + sex:mgmt.level))
plot(residuals(fit) ~ sex)

# different at 4 if interaction?
linear.hypothesis(fit, c(0,1,0,4))

linear.hypothesis(fit, c(0,1,0,-2))

linear.hypothesis(fit, c(0,1,0,0))

plot(residuals(fit) ~ factor(mgmt.level))
anova(aov(residuals(fit) ~ factor(mgmt.level)))

#---------------------------------------------------------------------------
# Angell's data
#---------------------------------------------------------------------------

data(Angell); names(Angell)

attach(Angell)

contrasts(region) <- contr.treatment(levels(region), base=4)

contrasts(region) <- contr.sum(levels(region))
summary(
  regr <- lm(moral ~ hetero + mobility + region)  )

anova(regr)

# build a subset model to test for the addition of region
summary(
  regr.sub <- lm(moral ~ hetero + mobility)  )

anova(regr.sub, regr)
