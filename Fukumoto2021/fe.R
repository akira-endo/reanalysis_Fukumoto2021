## This replication file
## makes the following tables and figures:
# Figure 3

# Packages
library(tidyverse)
library(lfe)

##
# Figure 3
##

savepdf <- function(file, width=7.28, height=3.54){ 
    fname <- paste("",file,".pdf",sep="")
    pdf(fname, width=width, height=height,
        pointsize=7)
    par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

savepdf("output/Fukumoto_Fig3")
par(mfrow=c(2,4))

# March 4
source("fixed_effects/fe.0304.R")
plot(YMD[part], summary.FE.out[, 1],
     xlim=c(YMD[part[1]], YMD[part[length(part)]]),
     #ylim=c(min.LB, max.UB),
     ylim=c(0,0.03),
     xlab="Day", ylab="Treatment effect",
     type="l",
     xaxt="n",
     main="a March 4")
axis(1, at = c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]),
     labels = format(c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]), "%b %d"))
polygon(c(YMD[part], rev(YMD[part])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part], summary.FE.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part[8]], col = "turquoise") # March 4
abline(0,0,lty=2)

# Fukumoto_Fig3_a_source
Fukumoto_Fig3_a <- cbind(as.character(YMD[part]), summary.FE.out[, 1], LB, UB)
colnames(Fukumoto_Fig3_a) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_Fig3_a, "output/Fukumoto_Fig3_a_source.csv", row.names = FALSE)

# March 16
source("fixed_effects/fe.0316.R")
plot(YMD[part], summary.FE.out[, 1],
     xlim=c(YMD[part[1]], YMD[part[length(part)]]),
     ylim=c(min.LB, max.UB),
     xlab="Day", ylab="Treatment effect",
     type="l",
     xaxt="n",
     main="b March 16")
axis(1, at = c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]),
     labels = format(c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]), "%b %d"))
polygon(c(YMD[part], rev(YMD[part])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part], summary.FE.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part[8]], col = "turquoise") # March 16
abline(0,0,lty=2)

# Fukumoto_Fig3_b_source
Fukumoto_Fig3_b <- cbind(as.character(YMD[part]), summary.FE.out[, 1], LB, UB)
colnames(Fukumoto_Fig3_b) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_Fig3_b, "output/Fukumoto_Fig3_b_source.csv", row.names = FALSE)

# April 6
source("fixed_effects/fe.0406.R")
plot(YMD[part], summary.FE.out[, 1],
     xlim=c(YMD[part[1]], YMD[part[length(part)]]),
     ylim=c(min.LB, max.UB),
     xlab="Day", ylab="Treatment effect",
     type="l",
     xaxt="n",
     main="c April 6")
axis(1, at = c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]),
     labels = format(c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]), "%b %d"))
polygon(c(YMD[part], rev(YMD[part])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part], summary.FE.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part[8]], col = "turquoise") # April 6
abline(0,0,lty=2)

# Fukumoto_Fig3_c_source
Fukumoto_Fig3_c <- cbind(as.character(YMD[part]), summary.FE.out[, 1], LB, UB)
colnames(Fukumoto_Fig3_c) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_Fig3_c, "output/Fukumoto_Fig3_c_source.csv", row.names = FALSE)

# April 10
source("fixed_effects/fe.0410.R")
plot(YMD[part], summary.FE.out[, 1],
     xlim=c(YMD[part[1]], YMD[part[length(part)]]),
     ylim=c(min.LB, max.UB),
     xlab="Day", ylab="Treatment effect",
     type="l",
     xaxt="n",
     main="d April 10")
axis(1, at = c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]),
     labels = format(c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]), "%b %d"))
polygon(c(YMD[part], rev(YMD[part])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part], summary.FE.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part[8]], col = "turquoise") # April 10
abline(0,0,lty=2)

# Fukumoto_Fig3_d_source
Fukumoto_Fig3_d <- cbind(as.character(YMD[part]), summary.FE.out[, 1], LB, UB)
colnames(Fukumoto_Fig3_d) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_Fig3_d, "output/Fukumoto_Fig3_d_source.csv", row.names = FALSE)

# # April 16
# source("fixed_effects/fe.0416.R")
# plot(YMD[part], summary.FE.out[, 1],
#      xlim=c(YMD[part[1]], YMD[part[length(part)]]),
#      ylim=c(min.LB, max.UB),
#      xlab="Day", ylab="Treatment effect",
#      type="l",
#      xaxt="n",
#      main="e April 16")
# axis(1, at = c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]),
#      labels = format(c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]), "%b %d"))
# polygon(c(YMD[part], rev(YMD[part])), c(UB, rev(LB)), col = "gray90", border = "gray90")
# lines(YMD[part], summary.FE.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part[8]], col = "turquoise") # April 16
# abline(0,0,lty=2)
# 
# # Fukumoto_Fig3_e_source
# Fukumoto_Fig3_e <- cbind(as.character(YMD[part]), summary.FE.out[, 1], LB, UB)
# colnames(Fukumoto_Fig3_e) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                "upper bound of 95% confidence interval")
# write.csv(Fukumoto_Fig3_e, "output/Fukumoto_Fig3_e_source.csv", row.names = FALSE)
# 
# # April 22
# source("fixed_effects/fe.0422.R")
# plot(YMD[part], summary.FE.out[, 1],
#      xlim=c(YMD[part[1]], YMD[part[length(part)]]),
#      ylim=c(min.LB, max.UB),
#      xlab="Day", ylab="Treatment effect",
#      type="l",
#      xaxt="n",
#      main="f April 22")
# axis(1, at = c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]),
#      labels = format(c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]), "%b %d"))
# polygon(c(YMD[part], rev(YMD[part])), c(UB, rev(LB)), col = "gray90", border = "gray90")
# lines(YMD[part], summary.FE.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part[8]], col = "turquoise") # April 22
# abline(0,0,lty=2)
# 
# # Fukumoto_Fig3_f_source
# Fukumoto_Fig3_f <- cbind(as.character(YMD[part]), summary.FE.out[, 1], LB, UB)
# colnames(Fukumoto_Fig3_f) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                "upper bound of 95% confidence interval")
# write.csv(Fukumoto_Fig3_f, "output/Fukumoto_Fig3_f_source.csv", row.names = FALSE)
# 
# # May 11
# source("fixed_effects/fe.0511.R")
# plot(YMD[part], summary.FE.out[, 1],
#      xlim=c(YMD[part[1]], YMD[part[length(part)]]),
#      ylim=c(min.LB, max.UB),
#      xlab="Day", ylab="Treatment effect",
#      type="l",
#      xaxt="n",
#      main="g May 11")
# axis(1, at = c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]),
#      labels = format(c(YMD[part[1]], YMD[part[8]], YMD[part[15]], YMD[part[22]], YMD[part[29]]), "%b %d"))
# polygon(c(YMD[part], rev(YMD[part])), c(UB, rev(LB)), col = "gray90", border = "gray90")
# lines(YMD[part], summary.FE.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part[8]], col = "turquoise") # May 11
# abline(0,0,lty=2)
# 
# # Fukumoto_Fig3_g_source
# Fukumoto_Fig3_g <- cbind(as.character(YMD[part]), summary.FE.out[, 1], LB, UB)
# colnames(Fukumoto_Fig3_g) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                "upper bound of 95% confidence interval")
# write.csv(Fukumoto_Fig3_g, "output/Fukumoto_Fig3_g_source.csv", row.names = FALSE)

dev.off()

