env = read.csv("data/env_values.csv")
str(env)

i = which(env$value_of_env_protection == -1)
env$value_of_env_protection[i] = NA

plot(value_of_env_protection ~ env_education, data = env, pch = 16)


env_tab = table(env)

library(reshape2)
env_tall = melt(env_tab)
colnames(env_tall) = c("env_education", "value_of_env_protection", "count")

ggplot(env_tall) + 
	geom_tile(aes(x = env_education, y = value_of_env_protection, fill = count)) + 
	scale_fill_viridis_c()

## histogram
par(mfrow = c(2,1))
hist(env$env_education)
hist(env$value_of_env_protection)

cor.test(env$env_education, env$value_of_env_protection,
		 alternative = "greater", method = "spearman")
