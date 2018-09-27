## Packages loading

library(ade4)          # Function needed: is.euclid
library(vegan)         # Function needed: dbrda
library(RVAideMemoire) # Functions needed: MVA.synt, MVA.anova, pairwise.factorfit,
                       #                   MVA.plot



## Data loading

tab <- read.table("Example S2.txt",header=TRUE)



## Pre-treatment

Chemistry <- tab[,3:6560]^(1/4)
Chemistry.scaled <- scale(Chemistry)
mat.dist.Chemistry <- dist(Chemistry.scaled)



## Analysis

is.euclid(mat.dist.Chemistry)
dbRDA <- dbrda(mat.dist.Chemistry~Date*Treatment,data=tab)
MVA.synt(dbRDA)
anova(dbRDA)
MVA.anova(dbRDA)
pairwise.factorfit(dbRDA,tab$Date:tab$Treatment)

MVA.plot(dbRDA,fac=tab$Date:tab$Treatment,drawextaxes=FALSE)
