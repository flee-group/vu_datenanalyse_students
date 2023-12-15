fish = read.csv("data/coral_fish.csv")

str(fish)
head(fish)
table(fish$type)

# easy histograms
# make a figure with two plots, organized into 2 rows, 1 column
# also adjust margins
par(mfrow = c(2, 1), mar = c(4, 4, 1.5, 0.5))
hist(fish[fish$type == "tropical", 'richness'], 
	 main = "Tropical", xlab = "richness")
hist(fish[fish$type == "subtropical", 'richness'], 
	 main = "Subtropical", xlab = "richness")

## ggplot histogram
ggplot(fish) + geom_histogram(aes(x = richness, fill = type), 
	position = "dodge", bins = 20)

## boxplot
dev.off() # reset the plot
boxplot(richness ~ type, data = fish, notch = TRUE)


## mean
with(fish, tapply(richness, type, mean))
with(fish, tapply(richness, type, length))

## confidence interval
conf_interval = function(x, alpha = 0.05) {
	# the default is a 95% confidence interval, can be changed when you call the function
	n = length(x)
	xbar = mean(x)
	sem = sd(x)/sqrt(n) # standard error
	# half of alpha comes from each side of the distribution
	t_vals = qt(c(alpha/2, 1-alpha/2), n -1)
	# compute the CI
	ci = xbar + sem * t_vals
	names(ci) = c("lower", "upper")
	return(ci)
}

# use tapply to get a confidence interval for both types
with(fish, tapply(richness, type, conf_interval))


t.test(richness ~ type, data = fish, alternative = "less")

