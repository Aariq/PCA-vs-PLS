library(pls)
library(tidyverse)
library(here)

dat2 = read_csv(here("data", "mourning-cloaks", "MOCLclim.csv"))
head(dat2)
View(dat2) #there's a trailing row of NAs

dat2 <- dat2 %>% filter(!is.na(year))
# I added a response variable of mourning cloak sightings after June 30 because they fly as adults in spring then lay eggs and the 2nd gen flys in fall.  Could also play around with timing of these responses - I thought fall adults would  work well because summer drought was a weak predictor of total sightings
m0 = plsr(MOCL_fall ~ WiTemp+SprTemp+SuTemp+FaTemp_prev+WiPDSI+SprPDSI+SuPDSI+FaPDSI, validation = "CV", data = dat2)
summary(m0)
loadings(m0)
# first axis is cool, wet years, especially in the summer/spring when they are caterpillars/adults (overwinter as adults)
selectNcomp(m0, alpha = 0.05, plot = "T") #NS by prediction error
m0a = lm(dat2$MOCL_fall[1:20] ~ m0$scores[,1] + m0$scores[,2])
summary(m0a)


loadings(m0)
names(dat2)
m1 =princomp(dat2[1:20,c(3:6,19:22)])
m1$scores
summary(m1)
loadings(m1)
# first axis of climate variation is fall/winter temperature in year before emergence
# 2nd axis weights summer drought during development, but not as strongly as PLSR model

m2 = lm(dat2$MOCL_fall[1:20] ~ m1$scores[,1] + m1$scores[,2])
summary(m2)
# p-values for PCR axes noticeably lower than PLSR values
# PCR adj R2 = 0.116 (as opposed to 0.374 for PLSR)

plot(scores(m0)[,1], scores(m1)[,1])
plot(scores(m0)[,1], scores(m1)[,2])
plot(scores(m0)[,2], scores(m1)[,1])
plot(scores(m0)[,2], scores(m1)[,2])

plot(scores(m0)[,1], dat2$MOCL_fall[1:20])
plot(scores(m1)[,2], dat2$MOCL_fall[1:20])
