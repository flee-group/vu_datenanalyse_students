# Worksheet 2.1
# Hydropsyche
rm(list=ls(all=TRUE))

# set your working directory
#setwd("~/LEHRE_UIBK/24W/Datenanalyse/vu_datenanalyse/unit_2")

hydrop <- read.table("data/Hydropsyche.txt", header = TRUE)
nrow(hydrop)

# consider running next two lines to see how the regression graphs turn out with lower n
#x<-sort(sample(size=25,c(1:130)))
#hydrop<-hydrop[x,]

# 1. explore
names(hydrop)
head(hydrop)
str(hydrop)

plot(weight~width,data=hydrop)

hist(hydrop$width)
hist(hydrop$weight)
boxplot(scale(hydrop))

# can recognize considerable skewness of weight
# relationship seems to be *not* linear, linear regression likely not appropriate

# 2. dumb model fitting

mod1 <- lm(weight ~ width, data = hydrop)
coef(mod1)
summary(mod1)
anova(mod1)

plot(weight~width,data=hydrop)
abline(mod1)

# We found a significant positive relationship between head capsule width and body mass 
# (linear regression, F_1,128=192, P<0.001, a=-3.4+/-0.43,b=5.39+/-0.39).

# 3. diagnostic plots
mod1$residuals # to access residuals
residuals(mod1)
mod1$fitted.values

plot(mod1$fitted.values,mod1$residuals)                # check if model adequate, should not show any trend
hist(mod1$residuals)                                    # check ND of residuals
qqnorm(mod1$residuals); qqline(mod1$residuals)         # check ND of residuals
plot(mod1$fitted.values,sqrt(abs(mod1$residuals)))          # check variance homogeneity, should not show any trend
# in the last plot the point is to plot absolute/squared residuals (whatever positive) to check if variance changes with X

# can recognize (non-linear) trends in residuals and absolute residuals, also ND of residuals questionable (but not critical)

# 4. transform data and produce new model
# power function seems reasonable as mass is directly proportional to volume, and volume is proportion to length^3
plot(log(weight)~log(width),data=hydrop)
mod2 <- lm(log(weight) ~ log(width), data = hydrop)
coef(mod2)
summary(mod2)
anova(mod2)

exp(coef(mod2)[1])

abline(mod2)
layout(matrix(1:4,2,2))
plot(mod2)
# this model is much better and diagnostic plots do not show a violation of assumptions

# 5. final plot with confidence intervals
layout(1)
plot(log(hydrop$width),predict(mod2),type="l")

conf<-predict(mod2,interval="confidence",level=0.95)
pred<-predict(mod2,interval="prediction",level=0.95,newdata=hydrop)
# confidence intervals are for the model, prediction intervals are for data

lines(log(hydrop$width),conf[,2],type="l",col="red")
lines(log(hydrop$width),conf[,3],type="l",col="red")

plot(log(hydrop$width),log(hydrop$weight),type="p",
     xlab="log(head capsule width (mm))", ylab="log(body mass(mg))")
matlines(log(hydrop$width),conf,typ="l",col=c("black","red","red"),lty=c(1,2,2))
text(-1.2,2,"F_1,128=1650, P<0.001",pos=4)
text(-1.2,.5,"r2=0.93",pos=4)
text(-1,-5,"log(BM)=0.014+3.58*log(HCW)",pos=4)

# Bonus: scatterplot with power model but using non-logarithmic axes
econf<-exp(conf)
plot(hydrop$width,hydrop$weight,type="p",
     xlab="Head capsule width (mm)", ylab="Body mass(mg)")
matlines(hydrop$width,econf,typ="l",col=c("black","red","red"),lty=c(1,2,2))

