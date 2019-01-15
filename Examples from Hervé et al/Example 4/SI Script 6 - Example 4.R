## Packages loading

library(mixOmics)      # Functions needed: block.plsda, plotDiablo, circosPlot, cimDiablo,
                       #                   network
library(RVAideMemoire) # Functions needed: DIABLO.cv, DIABLO.test, MVA.synt, MVA.plot
library(here)



## Data loading

tab.Perianths <- read.table(here("Examples from Hervé et al","Example 4", "Example 4 - Perianths.txt"), 
                            header=TRUE)

tab.Anthers <- read.table(here("Examples from Hervé et al", "Example 4", "Example 4 - Anthers.txt"),
                               header=TRUE)

Genotype <- tab.Anthers$Genotype



## Pre-treatment

Chemistry.Perianths <- scale(tab.Perianths[ , 2:41])
Chemistry.Anthers <- scale(tab.Anthers[ , 2:44])
Chemistry <- list(Perianths = Chemistry.Perianths, Anthers = Chemistry.Anthers)



## Analysis

DIABLO <- block.plsda(Chemistry, Genotype, ncomp = 2)

DIABLO.cv(DIABLO)

# This may take several mninutes to run, remove progress=FALSE to see computation progress:
DIABLO.test(DIABLO, progress = TRUE) # Remove progress=FALSE to see computation progress
MVA.synt(DIABLO)

MVA.plot(DIABLO,fac=Genotype,space=1,drawextaxes=FALSE,main="Perianths")
MVA.plot(DIABLO,fac=Genotype,space=2,drawextaxes=FALSE,main="Anthers")
par(mfrow=c(1,2))
MVA.plot(DIABLO,"corr",space=1,main="Perianths")
MVA.plot(DIABLO,"corr",space=2,main="Anthers")

plotDiablo(DIABLO,ncomp=1)
plotDiablo(DIABLO,ncomp=2)

class(DIABLO)[1] <- "block.splsda" # R-related step
circosPlot(DIABLO,cutoff=0.8,line=FALSE)

cimDiablo(DIABLO)

network(DIABLO,cutoff=0.8)
