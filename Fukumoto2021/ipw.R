## This replication file
## makes the following tables and figures:
# Extended Data Figure 4

# Packages
library(WeightIt)
library(survey)
library(tidyverse)
library(CBPS)

##
# Extended Data Figure 4
##

jpeg(filename = "output/Fukumoto_ED_Fig4.jpg", width=7.28, height=7.08, units="in",
     pointsize = 7, res = 500)
par(mfrow=c(4,4), mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))

# March 4
source("ipw/ipw.0304.R")
plot(YMD[part.adj], average.C[part.adj], col = 2, lty = 2,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     main = "a Outcome (March 4)",
     type="l",
     xaxt="n",
     ylim=c(0,
            max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB) - min(0,min.LB)))
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
lines(YMD[part.adj], average.C[part.adj], col = 2, lty = 1)
lines(YMD[part.adj], average.T[part.adj], lty = 1)
lines(YMD[part.adj], average.all.T[part.adj], lty = 2)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 4

plot(YMD[part.adj], summary.IPW.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB)),
     xlab="Day", ylab="ATC",
     main = "b ATC (March 4)",
     type="l", xaxt="n")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(summary.IPW.out[, 3], rev(summary.IPW.out[, 2])), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.IPW.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 4
abline(0,0,lty=2)

# Fukumoto_ED_Fig4_a_source
Fukumoto_ED_Fig4_a <- cbind(as.character(YMD[part.adj]), average.C[part.adj], average.T[part.adj], average.all.T[part.adj])
colnames(Fukumoto_ED_Fig4_a) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
                               "outcome average in all treated municipalities")
write.csv(Fukumoto_ED_Fig4_a, "output/Fukumoto_ED_Fig4_a_source.csv", row.names = FALSE)

# Fukumoto_ED_Fig4_b_source
Fukumoto_ED_Fig4_b <- cbind(as.character(YMD[part.adj]), summary.IPW.out[, 1], summary.IPW.out[, 2], summary.IPW.out[, 3])
colnames(Fukumoto_ED_Fig4_b) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_ED_Fig4_b, "output/Fukumoto_ED_Fig4_b_source.csv", row.names = FALSE)

# March 16
source("ipw/ipw.0316.R")
plot(YMD[part.adj], average.C[part.adj], col = 2, lty = 2,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     main = "c Outcome (March 16)",
     type="l",
     xaxt="n",
     ylim=c(0,
            max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB) - min(0,min.LB)))
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
lines(YMD[part.adj], average.C[part.adj], col = 2, lty = 1)
lines(YMD[part.adj], average.T[part.adj], lty = 1)
lines(YMD[part.adj], average.all.T[part.adj], lty = 2)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 16

plot(YMD[part.adj], summary.IPW.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB)),
     xlab="Day", ylab="ATC",
     main = "d ATC (March 16)",
     type="l", xaxt="n")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(summary.IPW.out[, 3], rev(summary.IPW.out[, 2])), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.IPW.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 16
abline(0,0,lty=2)

# Fukumoto_ED_Fig4_c_source
Fukumoto_ED_Fig4_c <- cbind(as.character(YMD[part.adj]), average.C[part.adj], average.T[part.adj], average.all.T[part.adj])
colnames(Fukumoto_ED_Fig4_c) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
                               "outcome average in all treated municipalities")
write.csv(Fukumoto_ED_Fig4_c, "output/Fukumoto_ED_Fig4_c_source.csv", row.names = FALSE)

# Fukumoto_ED_Fig4_d_source
Fukumoto_ED_Fig4_d <- cbind(as.character(YMD[part.adj]), summary.IPW.out[, 1], summary.IPW.out[, 2], summary.IPW.out[, 3])
colnames(Fukumoto_ED_Fig4_d) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_ED_Fig4_d, "output/Fukumoto_ED_Fig4_d_source.csv", row.names = FALSE)

# April 6
source("ipw/ipw.0406.R")
plot(YMD[part.adj], average.C[part.adj], col = 2, lty = 2,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     main = "e Outcome (April 6)",
     type="l",
     xaxt="n",
     ylim=c(0,
            max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB) - min(0,min.LB)))
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
lines(YMD[part.adj], average.C[part.adj], col = 2, lty = 1)
lines(YMD[part.adj], average.T[part.adj], lty = 1)
lines(YMD[part.adj], average.all.T[part.adj], lty = 2)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 6

plot(YMD[part.adj], summary.IPW.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB)),
     xlab="Day", ylab="ATC",
     main = "f ATC (April 6)",
     type="l",
     xaxt="n")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(summary.IPW.out[, 3], rev(summary.IPW.out[, 2])), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.IPW.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 6
abline(0,0,lty=2)

# Fukumoto_ED_Fig4_e_source
Fukumoto_ED_Fig4_e <- cbind(as.character(YMD[part.adj]), average.C[part.adj], average.T[part.adj], average.all.T[part.adj])
colnames(Fukumoto_ED_Fig4_e) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
                               "outcome average in all treated municipalities")
write.csv(Fukumoto_ED_Fig4_e, "output/Fukumoto_ED_Fig4_e_source.csv", row.names = FALSE)

# Fukumoto_ED_Fig4_f_source
Fukumoto_ED_Fig4_f <- cbind(as.character(YMD[part.adj]), summary.IPW.out[, 1], summary.IPW.out[, 2], summary.IPW.out[, 3])
colnames(Fukumoto_ED_Fig4_f) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_ED_Fig4_f, "output/Fukumoto_ED_Fig4_f_source.csv", row.names = FALSE)

# April 10
source("ipw/ipw.0410.R")
plot(YMD[part.adj], average.C[part.adj], col = 2, lty = 2,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     main = "g Outcome (April 10)",
     type="l",
     xaxt="n",
     ylim=c(0,
            max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB) - min(0,min.LB)))
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
lines(YMD[part.adj], average.C[part.adj], col = 2, lty = 1)
lines(YMD[part.adj], average.T[part.adj], lty = 1)
lines(YMD[part.adj], average.all.T[part.adj], lty = 2)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 10

plot(YMD[part.adj], summary.IPW.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB)),
     xlab="Day", ylab="ATC",
     main = "h ATC (April 10)",
     type="l",
     xaxt="n")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(summary.IPW.out[, 3], rev(summary.IPW.out[, 2])), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.IPW.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 10
abline(0,0,lty=2)

# Fukumoto_ED_Fig4_g_source
Fukumoto_ED_Fig4_g <- cbind(as.character(YMD[part.adj]), average.C[part.adj], average.T[part.adj], average.all.T[part.adj])
colnames(Fukumoto_ED_Fig4_g) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
                               "outcome average in all treated municipalities")
write.csv(Fukumoto_ED_Fig4_g, "output/Fukumoto_ED_Fig4_g_source.csv", row.names = FALSE)

# Fukumoto_ED_Fig4_h_source
Fukumoto_ED_Fig4_h <- cbind(as.character(YMD[part.adj]), summary.IPW.out[, 1], summary.IPW.out[, 2], summary.IPW.out[, 3])
colnames(Fukumoto_ED_Fig4_h) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_ED_Fig4_h, "output/Fukumoto_ED_Fig4_h_source.csv", row.names = FALSE)

# # April 16
# source("ipw/ipw.0416.R")
# plot(YMD[part.adj], average.C[part.adj], col = 2, lty = 2,
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      xlab="Day", ylab="Outcome",
#      main = "i Outcome (April 16)",
#      type="l",
#      xaxt="n",
#      ylim=c(0,
#             max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB) - min(0,min.LB)))
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# lines(YMD[part.adj], average.C[part.adj], col = 2, lty = 1)
# lines(YMD[part.adj], average.T[part.adj], lty = 1)
# lines(YMD[part.adj], average.all.T[part.adj], lty = 2)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 16
# 
# plot(YMD[part.adj], summary.IPW.out[, 1],
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      ylim=c(min(0,min.LB),
#             max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB)),
#      xlab="Day", ylab="ATC",
#      main = "j ATC (April 16)",
#      type="l",
#      xaxt="n")
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(summary.IPW.out[, 3], rev(summary.IPW.out[, 2])), col = "gray90", border = "gray90")
# lines(YMD[part.adj], summary.IPW.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 16
# abline(0,0,lty=2)
# 
# # Fukumoto_ED_Fig4_i_source
# Fukumoto_ED_Fig4_i <- cbind(as.character(YMD[part.adj]), average.C[part.adj], average.T[part.adj], average.all.T[part.adj])
# colnames(Fukumoto_ED_Fig4_i) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
#                                "outcome average in all treated municipalities")
# write.csv(Fukumoto_ED_Fig4_i, "output/Fukumoto_ED_Fig4_i_source.csv", row.names = FALSE)
# 
# # Fukumoto_ED_Fig4_j_source
# Fukumoto_ED_Fig4_j <- cbind(as.character(YMD[part.adj]), summary.IPW.out[, 1], summary.IPW.out[, 2], summary.IPW.out[, 3])
# colnames(Fukumoto_ED_Fig4_j) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                "upper bound of 95% confidence interval")
# write.csv(Fukumoto_ED_Fig4_j, "output/Fukumoto_ED_Fig4_j_source.csv", row.names = FALSE)
# 
# # April 22
# source("ipw/ipw.0422.R")
# plot(YMD[part.adj], average.C[part.adj], col = 2, lty = 2,
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      xlab="Day", ylab="Outcome",
#      main = "k Outcome (April 22)",
#      type="l",
#      xaxt="n",
#      ylim=c(0,
#             max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB) - min(0,min.LB)))
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# lines(YMD[part.adj], average.C[part.adj], col = 2, lty = 1)
# lines(YMD[part.adj], average.T[part.adj], lty = 1)
# lines(YMD[part.adj], average.all.T[part.adj], lty = 2)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 22
# 
# plot(YMD[part.adj], summary.IPW.out[, 1],
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      ylim=c(min(0,min.LB),
#             max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB)),
#      xlab="Day", ylab="ATC",
#      main = "l ATC (April 22)",
#      type="l",
#      xaxt="n")
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(summary.IPW.out[, 3], rev(summary.IPW.out[, 2])), col = "gray90", border = "gray90")
# lines(YMD[part.adj], summary.IPW.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 22
# abline(0,0,lty=2)
# 
# # Fukumoto_ED_Fig4_k_source
# Fukumoto_ED_Fig4_k <- cbind(as.character(YMD[part.adj]), average.C[part.adj], average.T[part.adj], average.all.T[part.adj])
# colnames(Fukumoto_ED_Fig4_k) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
#                                "outcome average in all treated municipalities")
# write.csv(Fukumoto_ED_Fig4_k, "output/Fukumoto_ED_Fig4_k_source.csv", row.names = FALSE)
# 
# # Fukumoto_ED_Fig4_l_source
# Fukumoto_ED_Fig4_l <- cbind(as.character(YMD[part.adj]), summary.IPW.out[, 1], summary.IPW.out[, 2], summary.IPW.out[, 3])
# colnames(Fukumoto_ED_Fig4_l) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                "upper bound of 95% confidence interval")
# write.csv(Fukumoto_ED_Fig4_l, "output/Fukumoto_ED_Fig4_l_source.csv", row.names = FALSE)
# 
# # May 11
# source("ipw/ipw.0511.R")
# plot(YMD[part.adj], average.C[part.adj], col = 2, lty = 2,
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      xlab="Day", ylab="Outcome",
#      main = "m Outcome (May 11)",
#      type="l",
#      xaxt="n",
#      ylim=c(0,
#             max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB) - min(0,min.LB)))
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# lines(YMD[part.adj], average.C[part.adj], col = 2, lty = 1)
# lines(YMD[part.adj], average.T[part.adj], lty = 1)
# lines(YMD[part.adj], average.all.T[part.adj], lty = 2)
# abline(v=YMD[part.adj[8]], col = "turquoise") # May 11
# 
# plot(YMD[part.adj], summary.IPW.out[, 1],
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      ylim=c(min(0,min.LB),
#             max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB)),
#      xlab="Day", ylab="ATC",
#      main = "n ATC (May 11)",
#      type="l",
#      xaxt="n")
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(summary.IPW.out[, 3], rev(summary.IPW.out[, 2])), col = "gray90", border = "gray90")
# lines(YMD[part.adj], summary.IPW.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part.adj[8]], col = "turquoise") # May 11
# abline(0,0,lty=2)
# 
# # Fukumoto_ED_Fig4_m_source
# Fukumoto_ED_Fig4_m <- cbind(as.character(YMD[part.adj]), average.C[part.adj], average.T[part.adj], average.all.T[part.adj])
# colnames(Fukumoto_ED_Fig4_m) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
#                                "outcome average in all treated municipalities")
# write.csv(Fukumoto_ED_Fig4_m, "output/Fukumoto_ED_Fig4_m_source.csv", row.names = FALSE)
# 
# # Fukumoto_ED_Fig4_n_source
# Fukumoto_ED_Fig4_n <- cbind(as.character(YMD[part.adj]), summary.IPW.out[, 1], summary.IPW.out[, 2], summary.IPW.out[, 3])
# colnames(Fukumoto_ED_Fig4_n) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                "upper bound of 95% confidence interval")
# write.csv(Fukumoto_ED_Fig4_n, "output/Fukumoto_ED_Fig4_n_source.csv", row.names = FALSE)

dev.off()

