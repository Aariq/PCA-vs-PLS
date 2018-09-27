## Packages loading

library(ade4)          # Functions needed: is.euclid, procuste, randtest
library(vegan)         # Functions needed: dbrda, scores
library(RVAideMemoire) # Functions needed: MVA.synt, MVA.plot



## Data loading

mat.dist.geography <- read.table("Example 3a - Geographic data.txt",header=TRUE)
mat.dist.geography <- as.dist(mat.dist.geography)

mat.dist.chemistry <- read.table("Example 3a - Chemical data.txt",header=TRUE)
mat.dist.chemistry <- as.dist(mat.dist.chemistry)



## Analysis

#  Step 1: PCoA

is.euclid(mat.dist.geography)
PCoA.geography <- dbrda(mat.dist.geography~1,add=TRUE)
MVA.synt(PCoA.geography)
scores.geography <- scores(PCoA.geography,choices=1:2,display="sites")

is.euclid(mat.dist.chemistry)
PCoA.chemistry <- dbrda(mat.dist.chemistry~1)
MVA.synt(PCoA.chemistry)
scores.chemistry <- scores(PCoA.chemistry,choices=1:5,display="sites")


#  Step 2: PCIA

PCIA <- procuste(scores.geography,scores.chemistry)
randtest(PCIA)

MVA.plot(PCIA,"pairs",drawextaxes=FALSE)
