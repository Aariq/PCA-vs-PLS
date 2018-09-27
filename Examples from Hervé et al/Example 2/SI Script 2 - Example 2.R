## Packages loading

library(Hotelling)     # Function needed: clr
library(vegan)         # Function needed: rda
library(RVAideMemoire) # Functions needed: MVA.synt, MVA.anova, pairwise.factorfit,
                       #                   MVA.plot
library(here)


## Data loading

tab <- read.table(here("Examples from Hervé et al", "Example 2", "Example 2.txt"),header=TRUE)
rowSums(tab[,4:73])
View(tab)

## Pre-treatment

min(tab[,4:73][tab[,4:73] != 0])
Chemistry <- clr(tab[,4:73] + 0.0001)
Chemistry.scaled <- scale(Chemistry)



## Analysis

RDA <- rda(Chemistry.scaled~Species*Organ*Time,data=tab)
MVA.synt(RDA)
anova(RDA)
MVA.anova(RDA)
pairwise.factorfit(RDA,tab$Species:tab$Organ)
pairwise.factorfit(RDA,tab$Organ:tab$Time)

MVA.plot(RDA,fac=tab$Species:tab$Organ,drawextaxes=FALSE)
MVA.plot(RDA,fac=tab$Organ:tab$Time,drawextaxes=FALSE)
MVA.plot(RDA,"corr")
