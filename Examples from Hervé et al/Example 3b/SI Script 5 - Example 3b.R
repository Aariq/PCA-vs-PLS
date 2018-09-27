## Packages loading

library(Hotelling)     # Function needed: clr
library(vegan)         # Functions needed: rda, stressplot
library(RVAideMemoire) # Functions needed: MVA.synt, to.dudi, MVA.plot
library(ade4)          # Functions needed: coinertia, randtest



## Data loading

tab.Chemistry <- read.table("Example 3b - Chemical data.txt",header=TRUE)

tab.Microbio <- read.table("Example 3b - Microbial data.txt",header=TRUE)
rowSums(tab.Microbio)



## Pre-treatment

Chemistry.log <- log(tab.Chemistry)
Chemistry.scaled <- scale(Chemistry.log)

min(tab.Microbio[tab.Microbio != 0])  # min value ignoring the zeroes
Microbio.clr <- clr(tab.Microbio + 0.01)
Microbio.scaled <- scale(Microbio.clr)



## Analysis

#  Step 1: PCA

PCA.Chemistry <- rda(Chemistry.scaled)
MVA.synt(PCA.Chemistry)
PCA.Chemistry.for.CIA <- to.dudi(PCA.Chemistry)

PCA.Microbio <- rda(Microbio.scaled)
MVA.synt(PCA.Microbio)
stressplot(PCA.Microbio,main="Shepard diagram")
PCA.Microbio.for.CIA <- to.dudi(PCA.Microbio)


#  Step 2: CIA

CIA <- coinertia(PCA.Chemistry.for.CIA,PCA.Microbio.for.CIA,scannf=FALSE,nf=4)
randtest(CIA)
MVA.synt(CIA)

MVA.plot(CIA,"corr",space=1,main="Chemistry",yax=NULL)
MVA.plot(CIA,"corr",space=2,main="Microbio",yax=NULL,cex=0.8)
MVA.plot(CIA,"corr",space=1,main="Chemistry",yax=NULL,xax=2)
MVA.plot(CIA,"corr",space=2,main="Microbio",yax=NULL,xax=2,cex=0.8)
