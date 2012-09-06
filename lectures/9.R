### Annenberg Lecture 9

setwd("/Users/bob/work/courses/annenberg/data/")
library(car)

#--------------------------------------------------
# Duncan data
#--------------------------------------------------

data(Duncan); attach(Duncan); names(Duncan)

# the types are not balanced
summary(type)

# comparison boxplots
plot(prestige ~ type)

# simple one way anova 
summary( model.aov <- aov(prestige ~ type) )

# fit as regression, dummy coding
summary( model.regr <- lm(prestige ~ type) )

# regression with effect coding
contrasts(type) <- contr.sum(levels(type))
summary( model.regr <- lm(prestige ~ type) )


# to see the means by group, use split command
sapply( split(prestige,type), mean )

# multiple comparisons using Tukey-Kramer
tk <- TukeyHSD(model.aov, ordered=TRUE)
plot(tk)

# residual plots, by group and then as qq plot
par(mfrow=c(1,2))
  plot (residuals(model.aov) ~ type)
  qq.plot (residuals(model.aov))
par(mfrow=c(1,1))

# formal test of equality of variances (not robust)
bartlett.test(residuals(model.aov) ~ type)


# simple one way anova (effect coding)
contrasts(type) <- contr.sum(levels(type))

summary( model.1 <- lm(prestige ~ type) )
anova(model.1)




#--------------------------------------------------
# 2000 Election party survey
#--------------------------------------------------

Survey <- read.table("party_survey_2000.txt", header=TRUE, sep=",")
summary(Survey)
names(Survey)

attach(Survey)

# counts and means
summary (factor(region))
tapply( party.dummy, region, mean )

# one way anova, by region as a factor (its numerically coded)
summary ( region.aov <- aov(party.dummy ~ region)  ) # big oops

# corrected
anova ( region.aov <- aov(party.dummy ~ factor(region))  )

# tukey intervals
tk <- TukeyHSD( region.aov, ordered=TRUE )


###  now by state

summary (state)
format( sapply( split(party.dummy, state), mean) , digits=2)

# one way anova
anova ( state.aov <- aov(party.dummy ~ state)  )

# tukey intervals
tk <- TukeyHSD( state.aov, ordered=TRUE )

intervals <- data.frame(tk[1])
intervals
hist(intervals[,2], probability=TRUE) # lower endpoints, pos would mean signif
lines(density(intervals[,2]))

prop.texas <- mean(party.dummy[state=="TX"])
prop.calif  <- mean(party.dummy[state=="CA"])

t.test(party.dummy[state=="TX"], party.dummy[state=="CA"])

sort( sapply( split(party.dummy, state), mean ))


#---------------------------------------------------------
#  Guyer & Fox experiment
#---------------------------------------------------------

?Guyer

data(Guyer)
summary(Guyer)

attach(Guyer)

# counts in the design  Its not balanced or these would be equal
record <- factor(condition, levels=c("A", "P"))
table(record, sex)


# means of the data, in a table
mean.table <- tapply(cooperation, list(record, sex), mean)

# profile plot, t is for transpose
matplot(t(mean.table)); matlines(t(mean.table))


# plot for all categories... outlier?
boxplot(cooperation ~ record * sex)


# analysis of variance
model.aov <- aov(cooperation ~ record * sex)
summary(model.aov)

# remove a main effect
model.aov2 <- aov(cooperation ~ record + sex)
summary(model.aov2)

boxplot(residuals(model.aov) ~ record * sex); abline(h=0, col=2, lty=2)
bartlett.test(residuals(model.aov) ~ record * sex)



# compare to linear models: effect coding
contrasts(record) <- contr.sum(levels(record));
contrasts(sex)    <- contr.sum(levels(sex));

model.regr <- lm(cooperation ~ record * sex)
summary(model.regr)
vif(model.regr)

model.regr2 <- lm(cooperation ~ record + sex)
model.regr2

# same as in the anova
anova(model.regr); anova(model.regr2)

par(mfrow=c(2,2))
  plot(model.aov)
par(mfrow=c(1,1))

TukeyHSD(model.aov, ordered=TRUE)

#---------------------------------------------------------
#  Moore and Krupat experiment 
#---------------------------------------------------------

?Moore

data(Moore)

summary(Moore)

attach(Moore)

# change the ordering in plots, tables
partner.status <- factor(partner.status, levels=c("low", "high"))
authority      <- factor(fcategory     , levels=c("low", "medium", "high"))

table(partner.status,authority)


# means of the data, in a table
mean.table <- tapply(conformity, list(partner.status, authority), mean)

# profile plot
matplot(t(mean.table)); matlines(t(mean.table))

# can you get table from the margins?
tapply(conformity, partner.status, mean)
tapply(conformity, authority     , mean)

# plot for all categories
boxplot(conformity ~ authority * partner.status)


# analysis of variance
model.aov <- aov(conformity ~ authority * partner.status)
summary(model.aov)

# without the interaction
model.aov2 <- aov(conformity ~ authority + partner.status)
summary(model.aov2)



# As a multiple regression
contrasts(authority)      <- contr.sum(levels(authority))
contrasts(partner.status) <- contr.sum(levels(partner.status))

model.regr <- lm(conformity ~ authority * partner.status)
summary(model.regr)

model.regr.no <- lm(conformity ~ authority + partner.status)

anova(model.regr.no, model.regr)

vif(model.regr)


# diagnostics

boxplot(residuals(model.aov) ~ authority * partner.status)

bartlett.test(residuals(model.aov) ~ authority * partner.status)

par(mfrow=c(2,2))
  plot(model.aov)
par(mfrow=c(1,1))


# convert to a one-way and get multiple comparisons

groups <- factor(paste(format(authority),".",format(partner.status), sep=""))
summary(groups)

one.way <- aov(conformity ~ groups)
summary(one.way)

tk <- TukeyHSD(one.way, ordered=TRUE); tk

plot(tk)


