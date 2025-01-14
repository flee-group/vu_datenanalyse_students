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


n = 25
samp = sample(tirol_forest$for_cover_km2, n)
samp_mean = mean(samp)
samp_sd = sd(samp)

st_err = samp_sd/sqrt(n)

# compute quantiles from the t-distribution
# because this is a sample!
# why 0.975?? (hint: this is a 2-sided confidence interval)
quant = qt(0.975, df = n - 1)

# compute confidence limits
lower = samp_mean - quant * st_err 
upper = samp_mean + quant * st_err

## get data copied from a google sheet
## first copy the means columns (without header)
class_means = scan()
# then paste
# next copy the sds columns (without header)
class_sds = scan()

class_data = data.frame(class_means, class_sds)
class_data$st_err = class_data$class_sds/sqrt(25)
class_data$lower = class_data$class_means - quant * class_data$st_err
class_data$upper = class_data$class_means + quant * class_data$st_err

mean_in_interval = mu > class_data$lower & mu < class_data$upper
sum(mean_in_interval) / length(mean_in_interval)
