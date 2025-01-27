# Worksheet 2.2
# SPM in lakes
rm(list=ls(all=TRUE))

lakes <- read.table("data/LakeSPM.txt", header=TRUE)

# 1. explore data and check effects of TP and Dm on spm
names(lakes)
head(lakes)
str(lakes)

lakes<-lakes[,-c(1:2)] # drop not needed variables
plot(spm~TP,data=lakes)
plot(spm~Dm,data=lakes)
plot(log(spm)~log(TP),data=lakes)
plot(log(spm)~log(Dm),data=lakes)

plot(log(Dm)~log(TP),data=lakes)

summary(lm(log(spm)~log(TP),data=lakes))
summary(lm(log(spm)~log(Dm),data=lakes))
summary(m1<-lm(log(spm)~log(TP)+log(Dm),data=lakes))

str(m1)

hist(m1$residuals)
plot(m1$fitted.values,m1$residuals)
plot(m1$fitted.values,m1$residuals^2)
layout(matrix(1:4,2,2))
plot(m1)

# 2. exploring MLR and transformations
# log-transform all?
layout(1)
plot(lakes) # some serious skewness issues
boxplot(scale(lakes))
any(lakes<=0)
names(lakes)
lakes[,-c(5,10)]<-log(lakes[,-c(5,10)]) # all except pH and Vd
plot(lakes)
boxplot(scale(lakes))

# 3. use vif() to select predictors a priori
# dot (.) makes a model with all predictors 
mlr.full = lm(spm ~ ., data = lakes) 
summary(mlr.full)
library(car)
vif(mlr.full) #computes VIF

# look at how area is predicted by the X matrix
# R^2 is one!
summary(lm(area ~ . - spm, data = lakes))

# try a few reductions
# minus removes a predictor from the model
vif(lm(spm ~ . - area, data = lakes))
vif(lm(spm ~ . - area - Dm, data = lakes))
vif(lm(spm ~ . - area - Dm - Dmax, data = lakes))

# 4. build a model with variables reduced by vif
vif.mod<-lm(spm ~ . - area - Dm - Dmax, data = lakes)
summary(vif.mod)
full.mod<-lm(spm ~ ., data = lakes)
summary(full.mod)
# not how some variables have strongly changed slopes (and higher SE) with/without pre-selection by vif
# pre-selection here makes a lot of sense given mathematical relationships between some variables

# 5. stepwise model selection with AIC
step(vif.mod, direction="both")
fin.mod1<-lm(spm ~ pH+TP+DR+Vd, data = lakes)
step(full.mod, direction="both")
fin.mod2<-lm(spm ~ area+Dm+pH+TP+DR, data = lakes)

# 6. leave-one-out cross validation

# loop
nrow(lakes)
pred.spm<-numeric(26)
#i<-1
for(i in 1:26){
  one.lake<-lakes[i,]
  lakes2<-lakes[-i,]
  #mr<-lm(spm ~ pH+TP+DR+Vd, data = lakes2) # this is the "best" model, vif-preselected variables, AIC-based model selection
  #mr<-lm(spm ~ area+Dm+pH+TP+DR, data = lakes2) # only AIC-based model selection
  mr<-lm(spm ~ ., data = lakes2) # just the full model, no worries about vif or AIC
  pred.spm[i]<-predict(mr,newdata=one.lake)
}
plot(lakes$spm,pred.spm)
cor(lakes$spm,pred.spm)
sqrt(mean((lakes$spm-pred.spm)^2)) # RMSE, root mean squared error
# The model built from a vif-preselected X-matrix and then using AIC
# for model selection is indeed the model with the best predictive capacity





