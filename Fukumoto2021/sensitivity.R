## This replication file
## makes the following tables and figures:
# Extended Data Figure 5

# Packages
library(MatchIt)
library(lmtest)
library(sandwich)
library(tidyverse)

##
# Extended Data Figure 5
##

jpeg(filename = "output/Fukumoto_ED_Fig5.jpg", width=7.28, height=7.08, units="in",
     pointsize = 7, res = 500)
par(mfrow=c(4,4), mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))

# March 4
p_path <- "preprocess/preprocess.0304.R"
m_path <- "sensitivity/small_atc_0304_1000_1000.RDS"
lm_form <- as.formula("gm[, part[p]] ~ shutdown.0304")
source("genetic_matching/gen.0304.R")
plot(YMD[part.adj], average.C, col = 2, lty = 1,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     main="a Outcome (March 4)",
     type="n",
     xaxt="n",
     ylim=c(0,0.03))
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
lines(YMD[part.adj], average.C, col = 2, lty = 1, lwd = 1)
lines(YMD[part.adj], average.T, col = 1, lty = 1, lwd = 1)
lines(YMD[part.adj], average.all.T[part.adj], col = 1, lty = 2, lwd = 1)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 4

plot(YMD[part.adj], summary.Match.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(0,0.03),
     xlab="Day", ylab="ATC",
     type="n",
     xaxt="n",
     main = "b ATC (March 4)")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 4
abline(0,0,lty=2)

# Fukumoto_ED_Fig5_a_source
Fukumoto_ED_Fig5_a <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
colnames(Fukumoto_ED_Fig5_a) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
                                  "outcome average in all treated municipalities")
write.csv(Fukumoto_ED_Fig5_a, "output/Fukumoto_ED_Fig5_a_source.csv", row.names = FALSE)

# Fukumoto_ED_Fig5_b_source
Fukumoto_ED_Fig5_b <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
colnames(Fukumoto_ED_Fig5_b) <- c("date", "ATC", "lower bound of 95% confidence interval",
                                  "upper bound of 95% confidence interval")
write.csv(Fukumoto_ED_Fig5_b, "output/Fukumoto_ED_Fig5_b_source.csv", row.names = FALSE)

# March 16
p_path <- "preprocess/preprocess.0316.R"
m_path <- "sensitivity/small_atc_0316_1000_1000.RDS"
lm_form <- as.formula("gm[, part[p]] ~ shutdown.0316")
source("genetic_matching/gen.0316.R")
plot(YMD[part.adj], average.C, col = 2, lty = 1,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     main="c Outcome (March 16)",
     type="n",
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
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C, average.T, max.UB)),
     xlab="Day", ylab="ATC",
     type="n",
     xaxt="n",
     main = "d ATC (March 16)")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 16
abline(0,0,lty=2)

# Fukumoto_ED_Fig5_c_source
Fukumoto_ED_Fig5_c <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
colnames(Fukumoto_ED_Fig5_c) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
                                  "outcome average in all treated municipalities")
write.csv(Fukumoto_ED_Fig5_c, "output/Fukumoto_ED_Fig5_c_source.csv", row.names = FALSE)

# Fukumoto_ED_Fig5_d_source
Fukumoto_ED_Fig5_d <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
colnames(Fukumoto_ED_Fig5_d) <- c("date", "ATC", "lower bound of 95% confidence interval",
                                  "upper bound of 95% confidence interval")
write.csv(Fukumoto_ED_Fig5_d, "output/Fukumoto_ED_Fig5_d_source.csv", row.names = FALSE)

# April 6
p_path <- "preprocess/preprocess.0406.R"
m_path <- "sensitivity/small_atc_0406_1000_1000.RDS"
lm_form <- as.formula("gm[, part[p]] ~ shutdown.0406")
source("genetic_matching/gen.0406.R")
plot(YMD[part.adj], average.C, col = 2, lty = 1,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     type="n",
     xaxt="n",
     main="e Outcome (April 6)",
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
     type="n",
     xaxt="n",
     main = "f ATC (April 6)")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 6
abline(0,0,lty=2)

# Fukumoto_ED_Fig5_e_source
Fukumoto_ED_Fig5_e <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
colnames(Fukumoto_ED_Fig5_e) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
                                  "outcome average in all treated municipalities")
write.csv(Fukumoto_ED_Fig5_e, "output/Fukumoto_ED_Fig5_e_source.csv", row.names = FALSE)

# Fukumoto_ED_Fig5_f_source
Fukumoto_ED_Fig5_f <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
colnames(Fukumoto_ED_Fig5_f) <- c("date", "ATC", "lower bound of 95% confidence interval",
                                  "upper bound of 95% confidence interval")
write.csv(Fukumoto_ED_Fig5_f, "output/Fukumoto_ED_Fig5_f_source.csv", row.names = FALSE)

# April 10
p_path <- "preprocess/preprocess.0410.R"
m_path <- "sensitivity/small_atc_0410_1000_1000.RDS"
lm_form <- as.formula("gm[, part[p]] ~ shutdown.0410")
source("genetic_matching/gen.0410.R")
plot(YMD[part.adj], average.C, col = 2, lty = 1,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     main="g Outcome (April 10)",
     type="n",
     xaxt="n",
     ylim=c(0,
            max(average.all.T[part.adj], average.C, average.T, max.UB) - min(0,min.LB)))
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
lines(YMD[part.adj], average.C, col = 2, lty = 1, lwd = 1)
lines(YMD[part.adj], average.T, col = 1, lty = 1, lwd = 1)
lines(YMD[part.adj], average.all.T[part.adj], col = 1, lty = 2, lwd = 1)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 10

plot(YMD[part.adj], summary.Match.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C, average.T, max.UB)),
     xlab="Day", ylab="ATC",
     type="n",
     xaxt="n",
     main = "h ATC (April 10")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 10
abline(0,0,lty=2)

# Fukumoto_ED_Fig5_g_source
Fukumoto_ED_Fig5_g <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
colnames(Fukumoto_ED_Fig5_g) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
                                  "outcome average in all treated municipalities")
write.csv(Fukumoto_ED_Fig5_g, "output/Fukumoto_ED_Fig5_g_source.csv", row.names = FALSE)

# Fukumoto_ED_Fig5_h_source
Fukumoto_ED_Fig5_h <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
colnames(Fukumoto_ED_Fig5_h) <- c("date", "ATC", "lower bound of 95% confidence interval",
                                  "upper bound of 95% confidence interval")
write.csv(Fukumoto_ED_Fig5_h, "output/Fukumoto_ED_Fig5_h_source.csv", row.names = FALSE)

# # April 16
# p_path <- "preprocess/preprocess.0416.R"
# m_path <- "sensitivity/small_atc_0416_1000_1000.RDS"
# lm_form <- as.formula("gm[, part[p]] ~ shutdown.0416")
# source("genetic_matching/gen.0416.R")
# plot(YMD[part.adj], average.C, col = 2, lty = 1,
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      xlab="Day", ylab="Outcome",
#      main="i Outcome (April 16)",
#      type="n",
#      xaxt="n",
#      ylim = c(0,0.6))
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# lines(YMD[part.adj], average.C, col = 2, lty = 1, lwd = 1)
# lines(YMD[part.adj], average.T, col = 1, lty = 1, lwd = 1)
# lines(YMD[part.adj], average.all.T[part.adj], col = 1, lty = 2, lwd = 1)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 16
# 
# plot(YMD[part.adj], summary.Match.out[, 1],
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      ylim = c(-0.4,0.2),
#      xlab="Day", ylab="ATC",
#      type="n",
#      xaxt="n",
#      main = "j ATC (April 16)")
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
# lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 16
# abline(0,0,lty=2)
# 
# # Fukumoto_ED_Fig5_i_source
# Fukumoto_ED_Fig5_i <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
# colnames(Fukumoto_ED_Fig5_i) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
#                                   "outcome average in all treated municipalities")
# write.csv(Fukumoto_ED_Fig5_i, "output/Fukumoto_ED_Fig5_i_source.csv", row.names = FALSE)
# 
# # Fukumoto_ED_Fig5_j_source
# Fukumoto_ED_Fig5_j <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
# colnames(Fukumoto_ED_Fig5_j) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                   "upper bound of 95% confidence interval")
# write.csv(Fukumoto_ED_Fig5_j, "output/Fukumoto_ED_Fig5_j_source.csv", row.names = FALSE)
# 
# # April 22
# p_path <- "preprocess/preprocess.0422.R"
# m_path <- "sensitivity/small_atc_0422_1000_1000.RDS"
# lm_form <- as.formula("gm[, part[p]] ~ shutdown.0422")
# source("genetic_matching/gen.0422.R")
# plot(YMD[part.adj], average.C, col = 2, lty = 1,
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      xlab="Day", ylab="Outcome",
#      main="k Outcome (April 22)",
#      type="n",
#      xaxt="n",
#      ylim = c(0,1))
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# lines(YMD[part.adj], average.C, col = 2, lty = 1, lwd = 1)
# lines(YMD[part.adj], average.T, col = 1, lty = 1, lwd = 1)
# lines(YMD[part.adj], average.all.T[part.adj], col = 1, lty = 2, lwd = 1)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 22
# 
# plot(YMD[part.adj], summary.Match.out[, 1],
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      ylim = c(-0.2,0.8),
#      xlab="Day", ylab="ATC",
#      type="n",
#      xaxt="n",
#      main = "l ATC (April 22)")
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
# lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 22
# abline(0,0,lty=2)
# 
# # Fukumoto_ED_Fig5_k_source
# Fukumoto_ED_Fig5_k <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
# colnames(Fukumoto_ED_Fig5_k) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
#                                   "outcome average in all treated municipalities")
# write.csv(Fukumoto_ED_Fig5_k, "output/Fukumoto_ED_Fig5_k_source.csv", row.names = FALSE)
# 
# # Fukumoto_ED_Fig5_l_source
# Fukumoto_ED_Fig5_l <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
# colnames(Fukumoto_ED_Fig5_l) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                   "upper bound of 95% confidence interval")
# write.csv(Fukumoto_ED_Fig5_l, "output/Fukumoto_ED_Fig5_l_source.csv", row.names = FALSE)
# 
# # May 11
# p_path <- "preprocess/preprocess.0511.R"
# m_path <- "sensitivity/small_atc_0511_1000_1000.RDS"
# lm_form <- as.formula("gm[, part[p]] ~ shutdown.0511")
# source("genetic_matching/gen.0511.R")
# plot(YMD[part.adj], average.C, col = 2, lty = 1,
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      xlab="Day", ylab="Outcome",
#      main="m Outcome (May 11)",
#      type="n",
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
#      type="n",
#      xaxt="n",
#      main = "n ATC (May 11)")
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
# lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part.adj[8]], col = "turquoise") # May 11
# abline(0,0,lty=2)
# 
# # Fukumoto_ED_Fig5_m_source
# Fukumoto_ED_Fig5_m <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
# colnames(Fukumoto_ED_Fig5_m) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
#                                   "outcome average in all treated municipalities")
# write.csv(Fukumoto_ED_Fig5_m, "output/Fukumoto_ED_Fig5_m_source.csv", row.names = FALSE)
# 
# # Fukumoto_ED_Fig5_n_source
# Fukumoto_ED_Fig5_n <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
# colnames(Fukumoto_ED_Fig5_n) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                   "upper bound of 95% confidence interval")
# write.csv(Fukumoto_ED_Fig5_n, "output/Fukumoto_ED_Fig5_n_source.csv", row.names = FALSE)

dev.off()

