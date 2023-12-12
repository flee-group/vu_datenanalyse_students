library(vegan)
library(MASS)

DOM<-read.table(file="data/DOM_Berlin_Romero.txt",header=TRUE)
names(DOM)

DOM2<-DOM[,8:27] # only these variables have complete info for all seasons
boxplot(scale(DOM2))
# also make histograms
plot(DOM2)
# use quartz() or x11() to open a separate graphical device 
# variable E4_E6 behaves differently than others (highly skewed)
any(DOM2$E4_E6<=0)
hist(DOM2$E4_E6)
hist(1/sqrt(DOM2$E4_E6)) # better (even if pretty arbitrary)
DOM2$E4_E6<-1/sqrt(DOM2$E4_E6)
plot(DOM2) # ok to go

pca<-prcomp(DOM2,retx=TRUE,center=TRUE,scale.=TRUE)

summary(pca)
pca$sdev^2
screeplot(pca,type="lines")

pca$rotation
pca$x

# correlation biplot
biplot(pca) # quick and dirty
# by hand:
plot(pca$x)

# use different colors for water body types
levels(DOM$Type)
DOM$col.type<-DOM$Type
levels(DOM$col.type)<-c("darkgreen","lightgreen","darkblue","lightblue")
DOM$col.type<-as.character(DOM$col.type)

# could use different symbols for season
DOM$pch.season<-DOM$Season
levels(DOM$pch.season)<-c(21:24)
DOM$pch.season<-as.numeric(as.character(DOM$pch.season))

plot(pca$x,pch=DOM$pch.season,col=DOM$col.type)

structure<-cor(DOM2,pca$x)


