hydrop = read.table("data/Hydropsyche.txt", header = TRUE)

## data exploration
plot(weight ~ width, data = hydrop, pch = 16)
hist(hydrop$weight)
hist(hydrop$width)

mod1 = lm(weight ~ width, data = hydrop)
summary(mod1)

# diagnostics
par(mfrow = c(2,2))
plot(mod1)

plot(weight ~ width, data = hydrop, pch = 16)
abline(mod1, col = 'blue')

# transformations
mod_log = lm(log(weight) ~ log(width), data = hydrop)
summary(mod_log)
par(mfrow = c(2,2))
plot(mod_log)
plot(log(weight) ~ log(width), data = hydrop, pch = 16)
abline(mod_log, col = 'blue')





# Bonus - scatterplot 
# to get you started
ggplot(hydrop, aes(x = width, y = weight)) + geom_point() + 
	geom_smooth(method = "glm", formula = y ~ log(x), 
				method.args = list(family = gaussian(link = "log")))




plot(weight ~ width, data = hydrop, pch = 16)
xx = seq(0, 2, length.out = 100)
yy = exp(0.0139 + 3.5828 * log(xx))
lines(xx, yy, col = 'blue')
