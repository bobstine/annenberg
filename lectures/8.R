### Annenberg Lecture 8

setwd("/Users/bob/work/courses/annenberg/data/")
library(car)

#--------------------------------------------------
# Real Estate Model
#--------------------------------------------------

Leases <- read.table("real_estate.txt", header=TRUE)        # from web
names(Leases)
summary(Leases)

attach(Leases)

# inital scatterplot views, colored by location
scatterplot(sqft, rent.total, groups=location)  # unequal variance

### add variables to data frame, reattach
Leases$ave.cost  <- rent.total/sqft      # ADD Variable
Leases$recip.sqft<- 1/sqft               # ADD Variable
detach(Leases); attach(Leases)

scatterplot(ave.cost ~ recip.sqft, groups=location)

# find the outliers, remove the outliers
identify(ave.cost, recip.sqft)

detach(Leases)
CleanLeases <- Leases[-c(168, 170),]
attach(CleanLeases)
search()


scatterplot(ave.cost ~ recip.sqft, groups=location)

# residuals from this regression have a clear pattern with red above fit, others below
regr.1 <- lm(ave.cost ~ recip.sqft); summary(regr.1)
scatterplot(fitted.values(regr.1), residuals(regr.1),groups=location)
abline(h=0, lty=2)

par(mfrow=c(1,2))
	hist(residuals(regr.1))
	qq.plot(residuals(regr.1)) # not normal, too early for too much concern
par(mfrow=c(1,1))


# add location, check for interaction
regr.2 <- lm(ave.cost ~ recip.sqft * location); summary(regr.2)
regr.2s<- lm(ave.cost ~ recip.sqft + location); summary(regr.2)
anova(regr.2s, regr.2)


# correlate residuals with other factors
for (j in c(1:23)) {
	cat(names(CleanLeases)[j],                           #  print name
	    cor(residuals(regr.2),CleanLeases[,j]), "\n") }  # add new line

par(mfrow=c(1,2))	
	plot( residuals(regr.2) ~ occupancy)
	plot( residuals(regr.2) ~ parking)
par(mfrow=c(1,1))	
	
# transform parking so can interpret
parking.per.sqft <- parking/sqft                 # ADD Variable
par(mfrow=c(1,2))	
	plot( residuals(regr.2) ~ occupancy)
	plot( residuals(regr.2) ~ parking.per.sqft)  # high leverage points?
par(mfrow=c(1,1))	
	
# add to the model
regr.3 <- lm(ave.cost ~ recip.sqft * location + occupancy + parking.per.sqft);
summary(regr.3)

# added var plots for the added predictors, adjusting for others in model
# huge leverage for some leases
par(mfrow=c(1,2))
	av.plot(regr.3, occupancy)
	av.plot(regr.3, parking.per.sqft)
par(mfrow=c(1,1))	


# add interaction for parking
regr.4<- lm(ave.cost ~ recip.sqft * location + occupancy 
			+ parking.per.sqft*location );
summary(regr.4)


# remove interaction for fixed costs
regr.5<- lm(ave.cost ~ recip.sqft + location + occupancy 
			+ parking.per.sqft*location );
summary(regr.5)


# try some other factors
regr.6<- lm(ave.cost ~ recip.sqft + location + occupancy 
			+ parking.per.sqft*location + lease.length + dist.city+dist.airport);
summary(regr.6)

# better?
anova(regr.5, regr.6)


# try some other categorical factors
regr.7 <- lm(ave.cost ~ recip.sqft + location + occupancy 
			+ parking.per.sqft*location + lease.length 
			+ wiring + exercise);
summary(regr.7)


# add just wiring, removing the exercise factor and keeping lease length
regr.8 <- lm(ave.cost ~ recip.sqft + location + occupancy 
			+ parking.per.sqft*location + lease.length + wiring );
summary(regr.8)

# added var plots for some added continuous predictors, adjusting for others in model
par(mfrow=c(2,2))
	av.plots(regr.8, recip.sqft)
	av.plots(regr.8, lease.length)
	av.plots(regr.8, occupancy)
	av.plots(regr.8, parking.per.sqft)  # high leverage
par(mfrow=c(1,1))	



regr.9 <- lm(ave.cost ~ recip.sqft + location + occupancy 
			+ parking.per.sqft*location + lease.length + wiring + recip.sqft:wiring );
summary(regr.9)


# overall model plot, moved closer together
par(mfrow=c(2,1), mar=c(5,4,1,1))  # bottom left top right
	plot(fitted.values(regr.8), ave.cost); abline(0,1,col=2)
    plot(fitted.values(regr.8), residuals(regr.8)); abline(h=0,col=2)
par(mfrow=c(1,1), mar=c(5,4,4,2))


###  Try out stepwise modeling, starting from the previous model


# lower is the starting model, upper is maximum collection to search
# m is the number of predictors that might be added
#    (size of predictors in upper formula) - (size in lower)


lower.eqn <- formula(regr.8)
upper.eqn <- ave.cost ~ (renovation+lease.length+age+dist.city+dist.airport+drive+location+occupancy+floor.bldg+sqft.bldg+elevator+restaurant+wiring+exercise+dist.hosp+firm+floors.lease+renewable+recip.sqft+parking.per.sqft)^2

scope <- list(lower = lower.eqn,upper = upper.eqn)   # all possible interactions
m    <- 200                                          # approximate good enough

# cannot tell to 'use Bonferroni', but this setting for k is similar
model <- step(regr.8, direction="forward", trace=1, steps=200, k=sqrt(2*log(m)), scope=scope)
summary(model)

# only first step passes from bonferroni point of view, so limit to one step
model <- step(regr.8, direction="forward", trace=1, steps=1, k=sqrt(2*log(m)), scope=scope)
summary(model)


# start from empty model with just a constant
# need to supply a scale estimate (your guess of final residual sd... yuk)
# take the scale from prior model
empty.model <- lm(ave.cost ~ 1); summary(empty.model)
scope <- list(lower = ave.cost ~ 1,    # just a constant
              upper = upper.eqn)       # all possible interactions
m    <- 200                            # approximate

regr.step <- step(empty.model, direction="forward", trace=1, steps=200, k=sqrt(2*log(m)),
	      scope=scope, scale = 0.85, data=CleanLeases)
summary(regr.step)


# ... use diagnostics with stepwise as well

par(mfrow=c(2,2))
	av.plots(regr.step, dist.city)
	av.plots(regr.step, recip.sqft)
	av.plots(regr.step, occupancy)
	av.plots(regr.step, parking.per.sqft)  # high leverage
par(mfrow=c(1,1))	


par(mfrow=c(2,2))
	plot(regr.step)   # high leverage point at 139
par(mfrow=c(1,1))	




