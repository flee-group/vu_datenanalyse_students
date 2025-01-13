lakes = read.table("data/LakeSPM.txt", header=TRUE)

# drop the first two columns, non-numeric
lakes = lakes[, -c(1, 2)]
plot(lakes) # skewness issues!

# log transform everything except pH and Vd
lakes[, -c(5,10)] = log(lakes[, -c(5,10)])
plot(lakes)

# suggestion: make some histograms!
par(mfrow = c(2,5))
for(i in 1:10) 
	hist(lakes[,i], main = "", xlab = colnames(lakes)[i])

# how one might do this in ggplot, if one was so inclined ;-)
library(reshape2)
library(ggplot2)
lakes_melt = melt(lakes)
ggplot(lakes_melt) + geom_histogram(aes(x = value), 
		bins = 15, fill = '#2266aa44', color = '#2266aa') + 
	facet_wrap( . ~ variable, nrow=2, ncol=5, scales = "free") + 
	theme_minimal()


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

mf = lm(spm ~ pH + TP + T + Q + DR + Vd, data = lakes)
summary(mf)

mod = step(mf, direction="both")
summary(mod)


## use LOO to compare the full model (mf) to my stepwise selected model, mod

# here is a custom function that will drop data point i from a model and
# predict back to the original data
# this can also be done in a for loop, here we show you a more R-like way :-)
loo_predict = function(i, fit_model) {
	lakes_drop = lakes[-i,]
	model_loo = lm(formula(fit_model), data = lakes_drop)
	predict(model_loo, lakes[i,])
}

# now we will do it for every data point for the two models
pr_mod = sapply(1:nrow(lakes), loo_predict, fit_model = mod)
pr_full = sapply(1:nrow(lakes), loo_predict, fit_model = mf)

# squared error for both
sqerr_mod = (pr_mod - lakes$spm)^2
sqerr_mf = (pr_full - lakes$spm)^2

# root mean square error, lower is better
sqrt(mean(sqerr_mod))
sqrt(mean(sqerr_mf))
