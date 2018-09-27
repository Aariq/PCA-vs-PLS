## Packages loading

library(Hotelling)     # Function needed: clr
library(ade4)          # Functions needed: dist.binary, is.euclid
library(vegan)         # Functions needed: rda, stressplot
library(pls)           # Function needed: cppls
library(RVAideMemoire) # Functions needed: MVA.synt, MVA.plot, MVA.cmv, MVA.test



## Data loading

tab.compositional <- read.table("Example 1 - Osimia_compositional.txt",header=TRUE)
rowSums(tab.compositional[,2:54])

Groups.PPLSDA <- dummy(tab.compositional$Color)

tab.binary <- read.table("Example 1 - Osimia_binary.txt",header=TRUE)



## Pre-treatment

min(tab.compositional[,2:54][tab.compositional[,2:54] != 0])
Chemistry.compositional <- clr(tab.compositional[,2:54] + 0.01)
Chemistry.compositional.scaled <- scale(Chemistry.compositional)

mat.dist.binary <- dist.binary(tab.binary[,2:54],method=2)



## Analysis 1: PCA on compositional data

PCA <- rda(Chemistry.compositional.scaled)
MVA.synt(PCA)
stressplot(PCA,main="Shepard diagram")

MVA.plot(PCA,fac=tab.compositional$Color,drawextaxes=FALSE,pch=c(15,17),main="PCA")



## Analysis 2: PPLS-DA on compositional data

PPLSDA <- cppls(Groups.PPLSDA~Chemistry.compositional.scaled)
MVA.cmv(Chemistry.compositional.scaled,tab.compositional$Color,model="PPLS-DA",
  crit.inn="NMC",ncomp=2)
# This may take several mninutes to run, remove progress=FALSE to see computation progress:
MVA.test(Chemistry.compositional.scaled,tab.compositional$Color,model="PPLS-DA",
  cmv=TRUE,ncomp=2,progress=FALSE)

MVA.plot(PPLSDA,fac=tab.compositional$Color,drawextaxes=FALSE,pch=c(15,17),main="PPLS-DA")
MVA.plot(PPLSDA,"corr",set=1,main="PPLS-DA")



## Analysis 3: PCoA on binary data

is.euclid(mat.dist.binary)
PCoA <- dbrda(mat.dist.binary~1)
MVA.synt(PCoA)
stressplot(PCoA,main="Shepard diagram")

MVA.plot(PCoA,fac=tab.compositional$Color,drawextaxes=FALSE,pch=c(15,17),main="PCoA")
