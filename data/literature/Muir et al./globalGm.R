#
# LMA-gm relationship in tomatoes compared to global pattern
# Chris Muir
# Last updated March 24, 2016
# Contact: chrisdmuir@gmail.com
#

	library(distr)
	
	# Import data (set working directory to where you downloaded data)

	globalGM <- read.csv("TableS3.csv")
	muir2016 <- read.csv("data.csv", row.names = 1)
	
	## Just get tomato species average log(gm) and log(LMA)
	muir2016 <- data.frame(lma = tapply(muir2016$lma.leaflet, muir2016$Spe, mean, na.rm = T),
												 gm = tapply(muir2016$gm, muir2016$Spe, mean, na.rm = T))
	
	# Plot
	pdf("Fig5.pdf", 5, 5, useDingbats = F)
	par(mar = c(5.5, 5.5, 0.5, 0.5))
	with(globalGM, plot(lma, gm, log = "xy", pch = 21, bg = "grey", 
											xlim = c(15, 400), ylim = c(0.01, 1), axes = F,
											frame.plot = T, cex.lab = 1.5, type = "n",
											xlab = expression(paste("LMA (g ", m ^ -2, ") [log scale]")),
											ylab = expression(paste(italic(g)[m], " (mol C", O[2], " ", 
																							m^-2, " ", s^-1, ") [log scale]"))))
	axis(1, at = c(25, 50, 100, 200, 400), lwd = 0, lwd.ticks = 1)
	axis(2, at = c(0.01, 0.1, 1), lwd = 0, lwd.ticks = 1, las = 1)
	
	# Regression and CI for global dataset
	m1 <- lm(log(gm) ~ log(lma), data = globalGM)
	
	# Add prediction intervals
	new <- with(globalGM, data.frame("lma" = seq(min(lma, na.rm = T), 
																							 max(lma, na.rm = T), 
																							 length.out = 1e3)))
	CIs <- predict(m1, interval = "prediction", newdata = new)
	polygon(c(new$lma, rev(new$lma)), exp(c(CIs[, 2], rev(CIs[, 3]))), border = NA,
					col = "grey")		
	points(new$lma, exp(CIs[, 2]), type = "l", lwd = 2)
	points(new$lma, exp(CIs[, 3]), type = "l", lwd = 2)
	
	# Add points
	with(globalGM, points(lma, gm, pch = 21, bg = "white"))
	with(muir2016, points(lma, gm, pch = 21, bg = "black", cex = 1.5))
	
	# Add legend
	rect(grconvertX(0.85, "npc", "user"), grconvertY(0.85, "npc", "user"),
			 grconvertX(0.95, "npc", "user"), grconvertY(0.95, "npc", "user"), 
			 col = "grey", border = NA)
	lines(c(grconvertX(0.85, "npc", "user"), grconvertX(0.95, "npc", "user")), 
				rep(grconvertY(0.85, "npc", "user"), 2), lwd = 2)
	lines(c(grconvertX(0.85, "npc", "user"), grconvertX(0.95, "npc", "user")), 
				rep(grconvertY(0.95, "npc", "user"), 2), lwd = 2)
	points(grconvertX(0.9, "npc", "user"), grconvertY(0.9, "npc", "user"),
				 pch = 21, bg = "white")
	text(grconvertX(0.85, "npc", "user"), grconvertY(0.9, "npc", "user"),
			 labels = "Global dataset\n95% prediction intervals", pos = 2, adj = 0.5)
	points(grconvertX(0.9, "npc", "user"), grconvertY(0.8, "npc", "user"),
				 pch = 21, bg = "black", cex = 1.5)
	text(grconvertX(0.85, "npc", "user"), grconvertY(0.8, "npc", "user"),
			 labels = "this study", pos = 2, adj = 0.5)
	dev.off()

# Are residuals significantly different from 0?

	Yj <- log(muir2016$gm)
	Yjhat <- predict(m1, newdata = data.frame(lma = muir2016$lma))
	t.test(Yj - Yjhat) # no, p = 0.13

# Is variance more/less than expected?

	## Distribution of expected sum of squared residuals
	## Prediction interval equations from pg. 65 of Neter et al. 1996

	n <- nrow(globalGM)	# sample size in global dataset
	Xnew <- log(muir2016$lma)	# Values of LMA we want to make predictions of gm from
	Xi <- log(globalGM$lma)
	Xbar <- mean(log(globalGM$lma))
	MSE <- sum((log(globalGM$gm) - predict(m1)) ^ 2) / (n - 2) # see eq. 1.28 on pg. 34

	sj <- sqrt(MSE) * sqrt(1 + 1 / n + (Xnew - Xbar) ^ 2 / sum((Xi - Xbar) ^ 2))

	fU <- convpow((sj[1] * Td(df = 97)) ^ 2 + (sj[2] * Td(df = 97)) ^ 2 +
								(sj[3] * Td(df = 97)) ^ 2 + (sj[4] * Td(df = 97)) ^ 2 +
								(sj[5] * Td(df = 97)) ^ 2 + (sj[6] * Td(df = 97)) ^ 2 +
								(sj[7] * Td(df = 97)) ^ 2 + (sj[8] * Td(df = 97)) ^ 2 +
								(sj[9] * Td(df = 97)) ^ 2 + (sj[10] * Td(df = 97)) ^ 2 +
								(sj[11] * Td(df = 97)) ^ 2 + (sj[12] * Td(df = 97)) ^ 2 +
								(sj[13] * Td(df = 97)) ^ 2 + (sj[14] * Td(df = 97)) ^ 2 +
								(sj[15] * Td(df = 97)) ^ 2 + (sj[16] * Td(df = 97)) ^ 2 +
								(sj[17] * Td(df = 97)) ^ 2 + (sj[18] * Td(df = 97)) ^ 2 +
								(sj[19] * Td(df = 97)) ^ 2, 1)	

	## Compared observed with expected

	Unew <- sum((Yj - Yjhat) ^ 2)
	p(fU)(Unew) # within 95% of distribution

	### Plot distribution 95% interval

	u <- seq(0, 15, length.out = 1e3)
	plot(u, d(fU)(u), type = "l")
	abline(v = q(fU)(0.025))
	abline(v = q(fU)(0.975))
	abline(v = Unew, col = "red", lwd = 2)

