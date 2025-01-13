env = read.csv("data/env_values.csv")
str(env)

## Set the -1 values to NA, then remove all NAs
i = which(env$value_of_env_protection == -1)
env$value_of_env_protection[i] = NA
env = env[complete.cases(env),]

# 2-panel figure with histograms of both variables
par(mfrow = c(2,1), mar=c(4,4,1,1))
hist(env$env_education, main = "", breaks = 5)
hist(env$value_of_env_protection, main = "", breaks = 7)


# Scatter plots of interval data are not useful!
plot(value_of_env_protection ~ env_education, data = env, pch = 16)

# Better - a tile plot
tab = table(env)
library(reshape2)
env_tall = melt(tab, value.name = "count")
ggplot(env_tall) + 
	geom_tile(aes(x = env_education, y = value_of_env_protection,
				  fill = count)) + 
	scale_fill_viridis_c()


# Test the spearman correlation
# This throws a warning: p-values are not exact
# This is because the test is non-parametric, we
# Must approximate p-values. Here this is done via an algorithm
# (see help file for which)
cor.test(env$env_education, env$value_of_env_protection,
		 alternative = "greater", method = "spearman")


# But we can manually estimate the p-value via permutation!
rhos = numeric(1000)
empirical_rho = cor(env$env_education, env$value_of_env_protection, method = "spearman")
for(i in 1:1000) {
	# shuffle the order of one column
	shuffle_dat = env
	rows = sample(1:nrow(shuffle_dat))
	shuffle_dat$value_of_env_protection = shuffle_dat$value_of_env_protection[rows]
	
	# compute and save the correlation
	rhos[i] = cor(shuffle_dat$env_education, shuffle_dat$value_of_env_protection,
				  method = "spearman")
}

hist(rhos, breaks = 30)
abline(v = empirical_rho, col = "blue")
sum(rhos >= empirical_rho) / length(rhos)

