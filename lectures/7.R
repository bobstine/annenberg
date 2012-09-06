### Annenberg Lecture 7

# usual startup to set path, load car library

setwd("/Users/bob/work/courses/annenberg/data/")
library(car)

#--------------------------------------------------
# Osteoporosis (review, exercise)
#--------------------------------------------------

Osteo <- read.table("osteo_1109.txt", header=TRUE)
names(Osteo)
summary(Osteo)

attach(Osteo)

# two-sample t-test (== to test equality)
boxplot(hip ~ estrogen)  # comparison boxplots
mean(hip[estrogen=="yes"]) - mean(hip[estrogen=="no"])
t.test(hip[estrogen=="yes"],hip[estrogen=="no"])
t.test(hip[estrogen=="yes"],hip[estrogen=="no"], var.equal=TRUE) # pooled

# check for age to be a confounding factor
par(mfrow=c(1,2))
	boxplot(age ~ estrogen)
	plot(hip ~ age)
par(mfrow=c(1,1))

# separate lines for the two groups(scatterplot adds legend)
scatterplot(hip ~ age, groups=estrogen)

regr.yes<-lm(hip ~ age, data=Osteo[estrogen=="yes",])
regr.no <-lm(hip ~ age, data=Osteo[estrogen=="no",])

regr.yes; regr.no

# another way to do the same two regressions, as nested model.  
# -1 removes overall intercept so that you get one for each group

nested <- lm(hip ~ estrogen/age - 1); nested

# only a dummy predictor (comp to two-sample t-test)

summary(lm(hip ~ estrogen))      
summary(lm(salary ~ estrogen + age)) 

# fit with interaction, dummy coding

contrasts(estrogen) <- contr.treatment(levels(sex), base=1) # default
summary(fit<- lm(hip ~ estrogen + age + estrogen:age)) # with interaction
summary(fit<- lm(hip ~ estrogen*age))                  # same

plot(residuals(fit) ~ estrogen)


#--------------------------------------------------
# Philadelphia region data
#--------------------------------------------------

Phila <- read.table("phila.txt", header=TRUE)
names(Phila)
summary(Phila)

# add a variable to the data frame *before* its attached
Phila$people.crime <- 1/Phila[,"crime.rate"]

attach(Phila)
names(Phila)

par(mfrow=c(1,4))
  hist(house.price); hist(people.crime); hist(miles.phila); hist(pop.chg)
par(mfrow=c(1,1))

# ignore the county information
scatterplot(people.crime, house.price)  # all data, pretty linear
summary( regr.base<-lm(house.price ~ people.crime) )
anova(regr.base)

# coded scatterplots, using colors to combine
scatterplot(people.crime, house.price, groups=county) # each


# conditioning plots work well with one categorical, one continuous
coplot(house.price ~ people.crime | county, row=1)

# decorate using Fox's panel fuinction
coplot(house.price ~ people.crime | county, row=1, panel=panel.car)


# fit within county, an easy way to get all 5 simple regressions
#  Note... this is why you cannot use a variable like 1/x
#          the 1/x denotes nesting in the model equation, not division
summary(lm(house.price ~ county/people.crime -1)) 

# fit with dummy coding representing the counties (parallel)
contrasts(county) <- contr.treatment(levels(county)); contrasts(county)
regr.parallel<-lm(house.price ~ county + people.crime)
summary(regr.parallel)


# be careful with these SS (they're not right for what we want)
anova(regr.parallel)
# these are the ones we want
anova(regr.base, regr.parallel)

# Check out effect coding
contrasts(county) <- contr.sum(levels(county)); contrasts(county)
regr.parallel.2<-lm(house.price ~ county + people.crime)
summary(regr.parallel.2)

# fit with dummy coding and an interaction  (that's what the colon does)
contrasts(county) <- contr.treatment(levels(county)); contrasts(county)
regr.interact <- lm(house.price ~ county + people.crime + county:people.crime )
summary(regr.interact)

# a more compact notation
summary(lm(house.price ~ county * people.crime ))

# test the improvement
anova(regr.interact)  # don't use these!!!
anova(regr.base, regr.parallel, regr.interact) # use these

# plot residuals, color coded
scatterplot(fitted.values(regr.parallel), residuals(regr.parallel),
			groups = county)  # eek, missing data!
# even though there's missing data, none shows up in the residuals 
residuals(regr.parallel)

?na.action
?na.exclude

# make holes for the missing data (same fit)
regr.parallel<-lm(house.price ~ county + people.crime, na.action="na.exclude")
summary(regr.parallel)
residuals(regr.parallel)

# plot residuals, color coded
scatterplot(people.crime, house.price ,		groups = county) 

scatterplot(fitted.values(regr.parallel), residuals(regr.parallel),
			groups = county) 
abline(h=0)

boxplot(residuals(regr.parallel) ~ county)

par(mfrow=c(1,2))
hist(residuals(regr.parallel)); qq.plot(residuals(regr.parallel))
par(mfrow=c(1,1))

?bartlett.test
bartlett.test(residuals(regr.parallel) ~ county)

fligner.test(residuals(regr.parallel) ~ county)

#--------------------------------------------------
# Angell
#--------------------------------------------------

data(Angell); names(Angell)

attach(Angell)

scatterplot.matrix(Angell)


# two-way conditioning plots get "busy" (too few poitns)
coplot(moral ~ hetero | region * mobility)


# Initial model
regr.simple <- lm(moral ~ hetero + mobility)
regr <- lm(moral ~ hetero + mobility + region)
summary(regr)
anova(regr.simple, regr)

# Different base group
contrasts(region) <- contr.treatment(levels(region), base=1)
regr.4 <- lm(moral ~ hetero + mobility + region)
summary(regr.4)
anova(regr.simple, regr)




#--------------------------------------------------
# Duncan's data
#--------------------------------------------------

data(Duncan)
attach(Duncan)
names(Duncan)

# look at scatterplots, colored by groups
scatterplot(prestige ~ income, groups=type)
scatterplot(prestige ~ education, groups=type)

# analogous conditioning plots
coplot(prestige ~ income    | type, row=1)
coplot(prestige ~ education | type, row=1)

# condition on both group and other factor
coplot(prestige ~ income | type + education)

# fit a first, "saturated model"

summary(
	regr.all <- lm(prestige ~ education*type + income*type) )

regr.ed <- lm(prestige ~ education      + income*type       ) 
regr.in <- lm(prestige ~ education*type + income            )
regr.no <- lm(prestige ~ education      + income      + type)
regr    <- lm(prestige ~ education      + income            )

# compare nested sequence of models
anova(regr.no, regr.all) 
anova(regr, regr.no, regr.in,          regr.all) 
anova(regr, regr.no,          regr.ed, regr.all) # only nested


# continous interaction
sanova(regr.no, regr.2)

# diagnostics
scatterplot(fitted.values(regr.2), residuals(regr.2), group=type)

boxplot(residuals(regr.2) ~ type)


### weighted least squares
?lm

# need variance for resids in each group
var(residuals(regr.2)[type=="wc"])
var(residuals(regr.2)[type=="prof"])
var(residuals(regr.2)[type=="bc"])

wts <- rep(0, length(type))
wts[type=="wc"]   <- var(residuals(regr.2)[type=="wc"])
wts[type=="prof"] <- var(residuals(regr.2)[type=="prof"])
wts[type=="bc"]   <- var(residuals(regr.2)[type=="bc"])
wts

wregr.2 <- lm(prestige ~ education * income  + type, weights = wts)
summary(wregr.2)

boxplot(residuals(wregr.2) ~ type)

