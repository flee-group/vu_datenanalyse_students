# worksheet 2iii
rm(list=ls(all=TRUE))

# one-way ANOVA, niche optima of periphyton
data <- read.table("data/nicheoptima.txt", header = TRUE)
names(data)
head(data)
str(data)
data$group<-factor(data$group)
data$group # 3 levels of factor "group"

boxplot(vel~group,data=data)
hist(data$vel[data$group=="Chrysophytes"])
hist(data$vel[data$group=="Cyanobacteria"])
hist(data$vel[data$group=="Diatoms"])
# more checking of ND possible

# variance homogeneity?
bartlett.test(vel~group,data=data)

# ANOVA
mod1<-aov(vel~group,data=data)
summary(mod1)
TukeyHSD(mod1)
# There is a significant effect of periphyton group on niche optima 
# (ANOVA, F=..., P<0.001).

mod2<-lm(vel~group,data=data)
anova(mod2)
summary(mod2)
pairwise.t.test(data$vel,data$group,p.adjust="bonferroni",pool.sd=TRUE)
# post-hoc results better shown by graph
boxplot(vel~group,data=data)
text(1.2,.25,"a")
text(2.2,.35,"b")
text(3.2,.28,"a")
locator(1)

####################
# two-way ANOVA, stoichiometry of microbes
rm(list=ls(all=TRUE))
mic <- read.table("data/MicrobesStoichiometry.txt", header = TRUE)
names(mic)
str(mic)
mic$species<-factor(mic$species)
mic$phase<-factor(mic$phase)
mic$NP<-factor(mic$NP)
str(mic)

# choosing on 1 medium and testing species*phase effects on na_avg
mic2<-mic[mic$NP=="5",]

boxplot(na_avg~species*phase,data=,mic2)
boxplot(na_avg~phase*species,data=,mic2)

# checking ND
hist(mic2$na_avg[mic2$species=="Pec" & mic2$phase=="log"])
table(paste(mic2$species,mic2$phase))
# total n=32, in each group n=8, maybe ND check not reasonable?
# check residuals at end?

# variance homogeneity?
spec_phase<-factor(paste(mic2$species,mic2$phase,sep="_"))
spec_phase
bartlett.test(na_avg~spec_phase,data=mic2)

# ANOVA
boxplot(na_avg~phase*species,data=mic2)
mod1<-aov(na_avg~phase*species,data=mic2)
summary(mod1)
# missed a significant interaction (sample size issue likely)
TukeyHSD(mod1)
# Pec differs between log and stat, Ver does not
# --> post-hoc tests capture interaction better than ANOVA!

# using only log-phase and testing species*NP effects on na_avg
mic3<-mic[mic$phase=="log",]

boxplot(na_avg~species*NP,data=,mic3)
boxplot(na_avg~NP*species,data=,mic3)

# checking ND
hist(mic3$na_avg[mic2$species=="Pec" & mic2$NP=="5"])
table(paste(mic3$species,mic3$NP))
# total n=32, in each group n=8, maybe ND check not reasonable?

# variance homogeneity?
spec_NP<-factor(paste(mic3$species,mic3$NP,sep="_"))
spec_NP
bartlett.test(na_avg~spec_NP,data=mic3)

# ANOVA
boxplot(na_avg~NP*species,data=mic3)
mod2<-aov(na_avg~NP*species,data=mic3)
summary(mod2)
TukeyHSD(mod2)

# 3-way ANOVA ;-)
boxplot(na_avg~phase*species*NP,data=mic)
mod3<-aov(na_avg~phase*species*NP,data=mic)
summary(mod3)
TukeyHSD(mod3)
# note now existing significance for phase:species interaction
# smart color use for boxplot would be helpful
colors<-c("darkblue","lightblue","darkgreen","lightgreen")
boxplot(na_avg~phase*species*NP,data=mic,col=colors)

##############
# ANCOVA, Hucho
rm(list=ls(all=TRUE))
hucho <- read.table("data/HuchoRatschan2012.txt", header = TRUE)
names(hucho)
str(hucho)
hucho$population<-factor(hucho$population)
hucho$population
hucho$population<-factor(hucho$population,levels=c("limited","medium","large"))

plot(length~discharge,data=hucho)
plot(log(length)~log(discharge),data=hucho)
# could run linear regression
mod1<-lm(log(length)~log(discharge),data=hucho)
summary(mod1)

plot(log(length)~population,data=hucho)
# could run ANOVA
mod2<-aov(log(length)~population,data=hucho)
anova(mod2)

# combine to ANCOVA
mod3<-lm(log(length)~log(discharge)+population,data=hucho)
summary(mod3)
anova(mod3)

mod4<-lm(log(length)~log(discharge)*population,data=hucho)
summary(mod4)
anova(mod4)
# no significant interaction, so drop the interaction term

plot(log(length)~log(discharge),col=population,data=hucho)
# use custom color
col<-hucho$population
levels(col)<-c("red","orange","green")
col<-as.character(col)

plot(log(length)~log(discharge),col=col,data=hucho)
# add 3 parallel lines
summary(mod3)
coef(mod3)
abline(a=coef(mod3)[1],b=coef(mod3)[2],col="red")
abline(a=coef(mod3)[1]+coef(mod3)[3],b=coef(mod3)[2],col="orange")
abline(a=coef(mod3)[1]+coef(mod3)[4],b=coef(mod3)[2],col="green")
# the model for the large population (green) should be used
# the models for the small and medium populations (red) would underestimate fish (pass) size


