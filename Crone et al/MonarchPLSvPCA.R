library(here)
library(pls)
mydat = read.csv(here("Crone et al", "simpleMonarch.csv"))
head(mydat)

# PCA
p0 = princomp(scale(mydat[,4:10]))
loadings(p0)
p0

## Regression on PCA scores
p1 = lm(ABUND~ scores(p0)[,1] + scores(p0)[,2], data = mydat)
summary(p1)


# PLSR
## Fit initial model
p2 = plsr(ABUND ~ COAST_DEV + GLY + NN + COAST_T + COAST_P + BR_TEMP + BR_PDSI,
          scale = TRUE,
          validation = "LOO", method = "oscorespls",
          data = mydat)
loadings(p2)
summary(p2)

## Cross validation
plot(RMSEP(p2), legendpos = "topright")
selectNcomp(p2, method = "randomization", plot = TRUE)
selectNcomp(p2, method = "onesigma", plot = TRUE)
# only one component necessary, but I'll use 2 for visualization purposes.
p2 = plsr(ABUND ~ COAST_DEV + GLY + NN + COAST_T + COAST_P + BR_TEMP + BR_PDSI,
          scale = TRUE,
          validation = "LOO", method = "oscorespls",
          data = mydat,
          ncomp = 2)

# Regression on PLSR scores?  I'm not sure what this does.
# p3 = lm(ABUND~ scores(m.ap3)[,1] + scores(m.ap)[,2], data = mondat[2:36,]) #this didn't work
p3 = lm(ABUND~ scores(p2)[,1] + scores(p2)[,2], data = mydat[2:36,]) #this works, but I'm not sure what it does.  Why do a regression on the PLS scores??
summary(p3)

# Do Loadings and Scores Match?
plot(loadings(p0)[,1], loadings(p2)[,1]) #almost exactly
plot(loadings(p0)[,2], loadings(p2)[,2]) #not well
plot(scores(p0)[,1], scores(p2)[,1]) #almost exactly
plot(scores(p0)[,2], scores(p2)[,2]) #not well

# plot of PLSR - point size is proportional to value of monarch abundance
plot(-scores(p2)[,1], scores(p2)[,2], pch = 21, bg = "gray80", cex = mydat$ABUND/5, xlab = "", ylab = "", xlim = c(-4,4), ylim = c(-8,8)) # scale separation of points to be same on both axes
mtext("PLS comp. 1", side = 1, line = 2)
mtext("PLS comp. 2", side = 2, line = 2)

# plot of PCR - point size is proportional to value of monarch abundance
plot(scores(p0)[,1], scores(p0)[,2], pch = 21, bg = "gray80", cex = mydat$ABUND/5, xlab = "", ylab = "", xlim = c(-4,4), ylim = c(-8,8)) # scale separation of points to be same on both axes
mtext("PCA comp. 1", side = 1, line = 2)
mtext("PCA comp. 2", side = 2, line = 2)