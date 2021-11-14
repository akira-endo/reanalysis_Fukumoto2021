## This replication file
## makes the following tables and figures:
# Figure 4

# Packages
library(MatchIt)
library(lmtest)
library(sandwich)
library(tidyverse)

##
# Figure 4
##

savepdf <- function(file, width=7.28, height=5.31){ 
    fname <- paste("",file,".pdf",sep="")
    pdf(fname, width=width, height=height,
        pointsize=7)
    par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

savepdf("output/Fukumoto_Fig4")
par(mfrow=c(3,4))

source("neighbor/pq.neighbor.functions.R")

# March 4
p_path <- "neighbor/preprocess/n.preprocess.0304.R"
m_path <- "neighbor/results/neighbor_atc_0304_1000_1000.RDS"
lm_form <- as.formula("gm[, part[p]] ~ shutdown.0304")
source("genetic_matching/gen.0304.R")
plot(YMD[part.adj], average.C, col = 2, lty = 2,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     main = "a Outcome (March 4)",
     type="l",
     xaxt="n",
     ylim=c(0,
            max(average.all.T[part.adj], average.C, average.T, max.UB) - min(0,min.LB)))
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
lines(YMD[part.adj], average.C, col = 2, lty = 1, lwd = 1)
lines(YMD[part.adj], average.T, col = 1, lty = 1, lwd = 1)
lines(YMD[part.adj], average.all.T[part.adj], col = 1, lty = 2, lwd = 1)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 4

plot(YMD[part.adj], summary.Match.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C, average.T, max.UB)),
     xlab="Day", ylab="ATC",
     main = "b ATC (March 4)",
     type="l",
     xaxt="n")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 4
abline(0,0,lty=2)

# Fukumoto_Fig4_a_source
Fukumoto_Fig4_a <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
colnames(Fukumoto_Fig4_a) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
                               "outcome average in all treated municipalities")
write.csv(Fukumoto_Fig4_a, "output/Fukumoto_Fig4_a_source.csv", row.names = FALSE)

# Fukumoto_Fig4_b_source
Fukumoto_Fig4_b <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
colnames(Fukumoto_Fig4_b) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_Fig4_b, "output/Fukumoto_Fig4_b_source.csv", row.names = FALSE)

# March 16
p_path <- "neighbor/preprocess/n.preprocess.0316.R"
m_path <- "neighbor/results/neighbor_atc_0316_1000_1000.RDS"
lm_form <- as.formula("gm[, part[p]] ~ shutdown.0316")
source("genetic_matching/gen.0316.R")
plot(YMD[part.adj], average.C, col = 2, lty = 2,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     main = "c Outcome (March 16)",
     type="l",
     xaxt="n",
     ylim=c(0,
            max(average.all.T[part.adj], average.C, average.T, max.UB) - min(0,min.LB)))
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
lines(YMD[part.adj], average.C, col = 2, lty = 1, lwd = 1)
lines(YMD[part.adj], average.T, col = 1, lty = 1, lwd = 1)
lines(YMD[part.adj], average.all.T[part.adj], col = 1, lty = 2, lwd = 1)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 16

plot(YMD[part.adj], summary.Match.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     #ylim=c(-0.5,1),
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C, average.T, max.UB)),
     xlab="Day", ylab="ATC",
     main = "d ATC (March 16)",
     type="l",
     xaxt="n")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 16
abline(0,0,lty=2)

# Fukumoto_Fig4_c_source
Fukumoto_Fig4_c <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
colnames(Fukumoto_Fig4_c) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
                               "outcome average in all treated municipalities")
write.csv(Fukumoto_Fig4_c, "output/Fukumoto_Fig4_c_source.csv", row.names = FALSE)

# Fukumoto_Fig4_d_source
Fukumoto_Fig4_d <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
colnames(Fukumoto_Fig4_d) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_Fig4_d, "output/Fukumoto_Fig4_d_source.csv", row.names = FALSE)

# April 6
p_path <- "neighbor/preprocess/n.preprocess.0406.R"
m_path <- "neighbor/results/neighbor_atc_0406_1000_1000.RDS"
lm_form <- as.formula("gm[, part[p]] ~ shutdown.0406")
source("genetic_matching/gen.0406.R")
plot(YMD[part.adj], average.C, col = 2, lty = 2,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     main = "e Outcome (April 6)",
     type="l",
     xaxt="n",
     #ylim=c(0,12))
     ylim=c(0,
            max(average.all.T[part.adj], average.C, average.T, max.UB) - min(0,min.LB)))
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
lines(YMD[part.adj], average.C, col = 2, lty = 1, lwd = 1)
lines(YMD[part.adj], average.T, col = 1, lty = 1, lwd = 1)
lines(YMD[part.adj], average.all.T[part.adj], col = 1, lty = 2, lwd = 1)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 6

plot(YMD[part.adj], summary.Match.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     #ylim=c(-6,6),
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C, average.T, max.UB)),
     xlab="Day", ylab="ATC",
     main = "f ATC (April 6)",
     type="l",
     xaxt="n")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 6
abline(0,0,lty=2)

# Fukumoto_Fig4_e_source
Fukumoto_Fig4_e <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
colnames(Fukumoto_Fig4_e) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
                               "outcome average in all treated municipalities")
write.csv(Fukumoto_Fig4_e, "output/Fukumoto_Fig4_e_source.csv", row.names = FALSE)

# Fukumoto_Fig4_f_source
Fukumoto_Fig4_f <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
colnames(Fukumoto_Fig4_f) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_Fig4_f, "output/Fukumoto_Fig4_f_source.csv", row.names = FALSE)

# # April 16
# p_path <- "neighbor/preprocess/n.preprocess.0416.R"
# m_path <- "neighbor/results/neighbor_atc_0416_1000_1000.RDS"
# lm_form <- as.formula("gm[, part[p]] ~ shutdown.0416")
# source("genetic_matching/gen.0416.R")
# plot(YMD[part.adj], average.C, col = 2, lty = 2,
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      xlab="Day", ylab="Outcome",
#      main = "g Outcome (April 16)",
#      type="l",
#      xaxt="n",
#      ylim=c(0,
#             max(average.all.T[part.adj], average.C, average.T, max.UB) - min(0,min.LB)))
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# lines(YMD[part.adj], average.C, col = 2, lty = 1, lwd = 1)
# lines(YMD[part.adj], average.T, col = 1, lty = 1, lwd = 1)
# lines(YMD[part.adj], average.all.T[part.adj], col = 1, lty = 2, lwd = 1)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 16
# 
# plot(YMD[part.adj], summary.Match.out[, 1],
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      ylim=c(min(0,min.LB),
#             max(average.all.T[part.adj], average.C, average.T, max.UB)),
#      xlab="Day", ylab="ATC",
#      main = "h ATC (April 16)",
#      type="l",
#      xaxt="n")
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
# lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 16
# abline(0,0,lty=2)
# 
# # Fukumoto_Fig4_g_source
# Fukumoto_Fig4_g <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
# colnames(Fukumoto_Fig4_g) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
#                                "outcome average in all treated municipalities")
# write.csv(Fukumoto_Fig4_g, "output/Fukumoto_Fig4_g_source.csv", row.names = FALSE)
# 
# # Fukumoto_Fig4_h_source
# Fukumoto_Fig4_h <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
# colnames(Fukumoto_Fig4_h) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                "upper bound of 95% confidence interval")
# write.csv(Fukumoto_Fig4_h, "output/Fukumoto_Fig4_h_source.csv", row.names = FALSE)
# 
# # April 22
# p_path <- "neighbor/preprocess/n.preprocess.0422.R"
# m_path <- "neighbor/results/neighbor_atc_0422_1000_1000.RDS"
# lm_form <- as.formula("gm[, part[p]] ~ shutdown.0422")
# source("genetic_matching/gen.0422.R")
# plot(YMD[part.adj], average.C, col = 2, lty = 2,
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      xlab="Day", ylab="Outcome",
#      main = "i Outcome (April 22)",
#      type="l",
#      xaxt="n",
#      ylim=c(0,
#             max(average.all.T[part.adj], average.C, average.T, max.UB) - min(0,min.LB)))
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# lines(YMD[part.adj], average.C, col = 2, lty = 1, lwd = 1)
# lines(YMD[part.adj], average.T, col = 1, lty = 1, lwd = 1)
# lines(YMD[part.adj], average.all.T[part.adj], col = 1, lty = 2, lwd = 1)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 22
# 
# plot(YMD[part.adj], summary.Match.out[, 1],
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      ylim=c(min(0,min.LB),
#             max(average.all.T[part.adj], average.C, average.T, max.UB)),
#      xlab="Day", ylab="ATC",
#      main = "j ATC (April 22)",
#      type="l",
#      xaxt="n")
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
# lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 22
# abline(0,0,lty=2)
# 
# # Fukumoto_Fig4_i_source
# Fukumoto_Fig4_i <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
# colnames(Fukumoto_Fig4_i) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
#                                "outcome average in all treated municipalities")
# write.csv(Fukumoto_Fig4_i, "output/Fukumoto_Fig4_i_source.csv", row.names = FALSE)
# 
# # Fukumoto_Fig4_j_source
# Fukumoto_Fig4_j <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
# colnames(Fukumoto_Fig4_j) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                "upper bound of 95% confidence interval")
# write.csv(Fukumoto_Fig4_j, "output/Fukumoto_Fig4_j_source.csv", row.names = FALSE)
# 
# # May 11
# p_path <- "neighbor/preprocess/n.preprocess.0511.R"
# m_path <- "neighbor/results/neighbor_atc_0511_1000_1000.RDS"
# lm_form <- as.formula("gm[, part[p]] ~ shutdown.0511")
# source("genetic_matching/gen.0511.R")
# plot(YMD[part.adj], average.C, col = 2, lty = 2,
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      xlab="Day", ylab="Outcome",
#      main = "k Outcome (May 11)",
#      type="l",
#      xaxt="n",
#      ylim=c(0,
#             max(average.all.T[part.adj], average.C, average.T, max.UB) - min(0,min.LB)))
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# lines(YMD[part.adj], average.C, col = 2, lty = 1, lwd = 1)
# lines(YMD[part.adj], average.T, col = 1, lty = 1, lwd = 1)
# lines(YMD[part.adj], average.all.T[part.adj], col = 1, lty = 2, lwd = 1)
# abline(v=YMD[part.adj[8]], col = "turquoise") # May 11
# 
# plot(YMD[part.adj], summary.Match.out[, 1],
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      ylim=c(min(0,min.LB),
#             max(average.all.T[part.adj], average.C, average.T, max.UB)),
#      xlab="Day", ylab="ATC",
#      main = "l ATC (May 11)",
#      type="l",
#      xaxt="n")
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
# lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part.adj[8]], col = "turquoise") # May 11
# abline(0,0,lty=2)
# 
# # Fukumoto_Fig4_k_source
# Fukumoto_Fig4_k <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
# colnames(Fukumoto_Fig4_k) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
#                                "outcome average in all treated municipalities")
# write.csv(Fukumoto_Fig4_k, "output/Fukumoto_Fig4_k_source.csv", row.names = FALSE)
# 
# # Fukumoto_Fig4_l_source
# Fukumoto_Fig4_l <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
# colnames(Fukumoto_Fig4_l) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                "upper bound of 95% confidence interval")
# write.csv(Fukumoto_Fig4_l, "output/Fukumoto_Fig4_l_source.csv", row.names = FALSE)

dev.off()
