## This replication file
## makes the following tables and figures:
# Extended Data Figure 3

# Packages
library(MatchIt)
library(tidyverse)
library(MASS)

##
# Extended Data Figure 3
##

jpeg(filename = "output/Fukumoto_ED_Fig3.jpg", width=7.28, height=2.36, units="in",
     pointsize = 7, res = 500)
par(mfrow=c(1,3), mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))

# April 6
p_path <- "preprocess/preprocess.0406.R"
m_path <- "matching_results/alt_atc_0406_1000_1000.RDS"
source("negative_binomial/nb.0406.R")
plot(YMD[part.adj], summary.NB.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(min.LB, max.UB),
     xlab="Day", ylab="Treatment effect",
     type="l",
     xaxt="n",
     main="a April 6")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.NB.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 6 
abline(0,0,lty=2)

# Fukumoto_ED_Fig3_a_source
Fukumoto_ED_Fig3_a <- cbind(as.character(YMD[part.adj]), summary.NB.out[, 1], LB, UB)
colnames(Fukumoto_ED_Fig3_a) <- c("date", "ATC", "lower bound of 95% confidence interval",
                                  "upper bound of 95% confidence interval")
write.csv(Fukumoto_ED_Fig3_a, "output/Fukumoto_ED_Fig3_a_source.csv", row.names = FALSE)

# April 10
p_path <- "preprocess/preprocess.0410.R"
m_path <- "matching_results/alt_atc_0410_1000_1000.RDS"
source("negative_binomial/nb.0410.R")
plot(YMD[part.adj], summary.NB.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(-5,35),
     xlab="Day", ylab="Treatment effect",
     type="l",
     xaxt="n",
     main="b April 10")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.NB.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 10
abline(0,0,lty=2)

irregular.0410 <- cbind(410, summary.NB.out, LB, UB)
rownames(irregular.0410) <- as.character(YMD[part.adj])

# Fukumoto_ED_Fig3_b_source
Fukumoto_ED_Fig3_b <- cbind(as.character(YMD[part.adj]), summary.NB.out[, 1], LB, UB)
colnames(Fukumoto_ED_Fig3_b) <- c("date", "ATC", "lower bound of 95% confidence interval",
                                  "upper bound of 95% confidence interval")
write.csv(Fukumoto_ED_Fig3_b, "output/Fukumoto_ED_Fig3_b_source.csv", row.names = FALSE)

# # April 16
# p_path <- "preprocess/preprocess.0416.R"
# m_path <- "matching_results/alt_atc_0416_1000_1000.RDS"
# source("negative_binomial/nb.0416.R")
# plot(YMD[part.adj][-29], summary.NB.out[1:28, 1],
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      ylim=c(-35,25),
#      xlab="Day", ylab="Treatment effect",
#      type="l",
#      xaxt="n",
#      main="c April 16")
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# polygon(c(YMD[part.adj][-29], rev(YMD[part.adj][-29])), c(UB[1:28], rev(LB[1:28])), col = "gray90", border = "gray90")
# lines(YMD[part.adj], summary.NB.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part.adj[8]], col = "turquoise") # March 16
# abline(0,0,lty=2)
# 
# irregular.0416 <- cbind(416, summary.NB.out, LB, UB)
# rownames(irregular.0416) <- as.character(YMD[part.adj])
# 
# # Fukumoto_ED_Fig3_c_source
# Fukumoto_ED_Fig3_c <- cbind(as.character(YMD[part.adj]), summary.NB.out[, 1], LB, UB)
# colnames(Fukumoto_ED_Fig3_c) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                   "upper bound of 95% confidence interval")
# write.csv(Fukumoto_ED_Fig3_c, "output/Fukumoto_ED_Fig3_c_source.csv", row.names = FALSE)

dev.off()

# Fukumoto_ED_Fig3_d_source
#Fukumoto_ED_Fig3_d <- rbind(irregular.0410, irregular.0416)
Fukumoto_ED_Fig3_d <- data.frame(irregular.0410)
Fukumoto_ED_Fig3_d <- Fukumoto_ED_Fig3_d[Fukumoto_ED_Fig3_d[, 3] > 10 | is.na(Fukumoto_ED_Fig3_d[, 3]),]
Fukumoto_ED_Fig3_d <- round(Fukumoto_ED_Fig3_d, 1)
colnames(Fukumoto_ED_Fig3_d) <- c("Treatment", "Point estimate", "Standard error",
                                  "lower bound of 95% confidence interval",
                                  "upper bound of 95% confidence interval")
write.csv(Fukumoto_ED_Fig3_d, "output/Fukumoto_ED_Fig3_d_source.csv")
