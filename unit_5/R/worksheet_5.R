library(vegan)
library(MASS)

DOM<-read.table(file="data/DOM_Berlin_Romero.txt",header=TRUE)
names(DOM)

DOM2<-DOM[,8:27] # only these variables have complete info for all seasons
boxplot(scale(DOM2))
# also make histograms
plot(DOM2)
# use quartz() or x11() to open a separate graphical device 
quartz()

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


# plot scores
plot(pca$x[,1],pca$x[,2],pch=DOM$pch.season,col=DOM$col.type,cex=2,lwd=2,
     xlab="PC1 (35.5%)", ylab="PC2 (19.7%)")
ordiellipse(pca,groups=DOM$Site)

# run 2-way ANOVA
m<-lm(pca$x[,1]~Season*Type,data=DOM)
anova(m)

# correlation biplot here
x11() # new graphical device
layout(matrix(1:2,1,2)) # make room for 2 plots
plot(pca$x[,1]/pca$sdev[1],pca$x[,2]/pca$sdev[2],pch=DOM$pch.season,col=DOM$col.type,cex=2,lwd=2,
     xlab="PC1 (35.5%)", ylab="PC2 (19.7%)")

plot(pca$x[,1]/pca$sdev[1],pca$x[,2]/pca$sdev[2],pch=DOM$pch.season,col="white",
     xlab="PC1 (35.5%)", ylab="PC2 (19.7%)")
structure<-cor(DOM2,pca$x)*2
Arrows(x0=0,y0=0,x1=structure[,1],y1=structure[,2])
text(structure[,1]*1.2,structure[,2]*1.2,names(DOM2))

# adding mass-spectrometry information
names(DOM)
DOM3<-DOM[,28:34]
structure2<-cor(DOM3,pca$x,use="complete.obs")*2
Arrows(x0=0,y0=0,x1=structure2[,1],y1=structure2[,2],col="red")
text(structure2[,1]*1.2,structure2[,2]*1.2,names(DOM3),col="red")

# adding potential drivers
names(DOM)
DOM4<-DOM[,35:42]
structure3<-cor(DOM4,pca$x,use="complete.obs")*2
Arrows(x0=0,y0=0,x1=structure3[,1],y1=structure3[,2],col="blue",lwd=2)
text(structure3[,1]*1.2,structure3[,2]*1.2,names(DOM4),col="blue")

# consider regressions to explain DOM quality
m1<-lm(pca$x[,1]~., data=DOM4)
summary(m1)

# do this much better with a RDA
xmat<-DOM4
myrda<-rda(DOM2~.,data=xmat)

anova(myrda)
anova(myrda,by="axis",model="direct",perm.max=9999,step=1000)
anova(myrda,by="term",model="direct",perm.max=9999,step=1000)
anova(myrda,by="margin",model="direct",perm.max=9999,step=1000)

(sites<-scores(myrda,choices=c(1,2),display="sites",scaling=1)) 
(lcs<-scores(myrda,choices=c(1,2),display="lc",scaling=1))
(species<-scores(myrda,choices=c(1,2),display="species",scaling=1)*0.3)
(constraints<-scores(myrda,choices=c(1,2),display="bp",scaling=1)*5) 

layout(matrix(1:2,1,2)) # make room for 2 plots
plot(sites,asp=1,pch=DOM$pch.season,col=DOM$col.type,cex=2,lwd=2)
#plot(lcs,asp=1,pch=DOM$pch.season,col=DOM$col.type,cex=2,lwd=2)

plot(sites,asp=1,pch=DOM$pch.season,col="white",cex=2,lwd=2)
Arrows(x0=0,y0=0,x1=constraints[,1],y1=constraints[,2],lwd=1.5,col="blue")
text(constraints[,1]*1.1,constraints[,2]*1.1,label=rownames(constraints),pos=4,cex=0.8,col="blue")

Arrows(x0=0,y0=0,x1=species[,1],y1=species[,2],lwd=2,arr.length=0,col="red")
text(species[,1:2]*1.1,label=rownames(species),pos=4,cex=1,col="red")



