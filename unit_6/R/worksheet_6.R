library(vegan)
library(MASS)

lipids<-read.table(file="data/BacterialMembrane.txt",header=TRUE)
names(lipids)

# t.test for acclimation effect in warm isolates only
lipids_warm = lipids[lipids$isolate == 'warm',]
boxplot(SA_branprop~temperature,data=lipids_warm)
t.test(SA_branprop~temperature,data=lipids_warm)

boxplot(FA1_SAnb~temperature,data=lipids_warm)
t.test(FA1_SAnb~temperature,data=lipids_warm)

# permutation-based testing for acclimation effect in warm isolates
# several "homemade" TSs possible: difference of rank sums between samples, difference of sample means, difference of sample means weighted by variance, just the formula of the t-statistic anyway. 
# rank(x) # to get ranks of a vector if such a transformation is desired
# sample(x) # creates a random permutation of x

R<-rank(lipids_warm$SA_branprop)
#R<-rank(lipids_warm$FA1_SAnb) # rerun followinglines with this one included for the other variable
RS6<-sum(R[lipids_warm$temperature==6])
RS28<-sum(R[lipids_warm$temperature==28])
TS_emp<-RS6-RS28

temperature<-lipids_warm$temperature
# schematically (!!) your code will have to look this:
TS_p<-numeric(999) # a vector to collect TS computed after permutation
for (i in 1:999) {
  temp_p = sample(temperature) # random assignment of temp to objects
  RS6_p = sum(R[temp_p == 6]) # 
  RS28_p =  sum(R[temp_p == 28]) # same for second temp treatment
  TS_p[i] = RS6_p-RS28_p# computing your TS
}
## null distribution of TSs
hist(TS_p)

# assess how likely your "empirical" TS is given this null distribution
# the "empirical" one is the TS computed for the unpermuted data!
abline(v = TS_emp, col='red', lwd=2)
sum(RSD>=TS_emp)/1000

# multivariate analysis using dissimilarity
library(vegan)
lipids2<-lipids[,4:20] # choose only FA columns
apply(lipids2,1,sum) # confirm proportional data
dmat <- vegdist(lipids2, method = "bray") # compute a dissimilarity matrix

clu_1<-hclust(dmat, method = "single") # one of many possible solutions, play with setting for method, check help(hclust)
clu_2<-hclust(dmat, method = "ward.D")

# then compare effect of agglomeration method on dendrogram
combifac<-paste(lipids$isolate,lipids$temperature,sep="_")
plot(clu_1, hang = -1, labels = combifac, ylab = "BC")
plot(clu_2, hang = -1, labels = combifac, ylab = "BC")

# which agglomeration method (and thus dendrogram) is best?
cophenetic(clu_2) # linkage distances in dendrogram
plot(dmat,cophenetic(clu_1))
cor(dmat,cophenetic(clu_1),method="spearman")
# could compare several clustering methods with this

# define subgroups
plot(clu_2$height,nrow(lipids):2,,ylab="number of clusters k") # look for jumps to define a "cutting distance"
text(clu_2$height,nrow(lipids):2,labels=nrow(lipids):2,cex=0.7)

cutree(clu_2,k=4) # gives a new grouping defined from dendrogram
cutree(clu_2,h=0.5)

# some more graphing options
plot(clu_2, hang = -1, labels = combifac, ylab = "BC")
rect.hclust(clu_2,k=4)
rect.hclust(clu_2,h=0.2)

# more plotting and cutting options with dendrogram()
lipids_dend<-as.dendrogram(clu_2)
heatmap(as.matrix(dmat),Rowv=lipids_dend,symm=TRUE)
######


# principal coordinate analyis (PCoA)
pcoa <- cmdscale(dmat, k = 2, eig=TRUE) # always computes all axes, but will only report scores of k=2
pcoa$eig # to assess importance of axes
# note some negative eigenvalues (variances!) for less important axes caused by squeezing the semimetric BC into a Euclidean space
plot(pcoa$eig)

plot(pcoa$points) # score plot
# use isolate and temperature information to color and set point characters

pch.temperature <- as.integer(as.character(lipids$temperature))
pch.temperature[pch.temperature==6]<-21
pch.temperature[pch.temperature==28]<-23

library(RColorBrewer)
cols = brewer.pal(8, "Set2")
col.isolate = as.character(lipids$isolate)
col.isolate[col.isolate == "warm"] = cols[1]
col.isolate[col.isolate == "cold"] = cols[2]

plot(pcoa$points,pch=pch.temperature,bg=col.isolate,cex=2) # score plot

# to show variables
plot(envfit(pcoa,env=lipids2))
ordisurf(x=pcoa,y=lipids2$FA1_SAnb,add=TRUE) # as contourplot

# NMDS ###################
#isoMDS()
mds_lipids = metaMDS(
  comm = lipids2, distance = "bray", k = 2, trymax = 100) # to run a NMDS, $points to get scores, $stress to get information about fit
mds_lipids$stress
## % of dissimilarities unrepresented

stressplot(mds_lipids) # to compare configuration distances with dissimilarities
goodness(mds_lipids) # sample-specific goodness of fit

plot(mds_lipids$points,asp=1,pch=pch.temperature,bg=col.isolate)

# to define space using the variables
wascores(mds_lipids$points,lipids2) # variables as weighted averages of site (=sample) scores
ordisurf(x=mds_lipids,y=lipids2$FA1_SAnb) # as contourplot
envfit(mds_lipids,env=lipids2) # take care: behaviour of variables not necessarily monotonous in ordination space

# some more useful graphical tools
plot(mds_lipids$points,asp=1,pch=pch.temperature,bg=col.isolate)
ordispider(mds_lipids,combifac)
ordihull()
ordiellipse()
ordicluster()

# PERMANOVA, tests for differences in location
adonis(lipids2~lipids$temperature*lipids$isolate,method="bray")
adonis(dmat~isolate*temperature,data=lipids)
# the hypotheses for this are:
# H0 isolate affects fatty acid composition
# H0 temp affects fatty acid composition
# H0 temp effect on fatty acid composition differs between isolates

# betadisper to test for differences in dispersion
bd<betadisper(dmat,group=combifac)
bd$distances # these are the distances to group centroids

permutest(bd)
anova(bd)
anova(lm(bd$distances~isolate*temperature,data=lipids))

# conclude:
# 1) we can´t find differences in dispersion among groups
# 2) Fatty acid composition differs between isolates and is affected by temperature. 
#    There is no interaction. Acclimation and Adaptation reactions are different.



