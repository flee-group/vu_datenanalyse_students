tirol_forest = read.csv("data/tirol_forest_cover.csv")
pop_n = nrow(tirol_forest)

# compute population mean
mu = mean(tirol_forest$for_cover_km2)

# squared deviations from mean, (x - mu)^2
sq_diff = (tirol_forest$for_cover_km2 - mu)^2

# the numerator above, the sum of squared deviations
numerator = sum(sq_diff)

# population standard deviation
sigma = sqrt(numerator/pop_n)



# let's do an experiment where we repeat an experiment 500 times
# each time, we take a sample with n = 25
n = 25
n_samples = 500
samples = matrix(0, nrow = n, ncol = n_samples)

for(i in 1:n_samples) {
	samples[, i] = sample(tirol_forest$for_cover_km2, n)
}

# compute sample statistics
# mean, sd, sterr for each simulated sample
means = apply(samples, 2, mean)
sds = apply(samples, 2, sd)
sterr = sds/sqrt(n)

# means, sds are normally distributed
# the mean of means is the population mean
# the mean of sds is the population sd
par(mfrow = c(2, 1))
hist(means, main = "")
abline(v = mu, col = "blue")
hist(sds, main = "")
abline(v = sigma, col = "blue")
mean(means) - mu
mean(sds) - sigma

# what t-quantile do we need when n = 25 and alpha = 0.05?
alpha = 0.05
quant = qt(1 - (alpha/2), df = n - 1)

# compute confidence limits for each sample
lower = means - quant * sterr 
upper = means + quant * sterr

# what proportion of times is the true mean mu within the confidence limits?
(in_ci_tab = table(mu >= lower & mu <= upper))
in_ci_tab[2] / sum(in_ci_tab)
