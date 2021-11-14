## This replication file
## makes the following tables and figures:
# Figure 2

# Packages
library(MatchIt)
library(lmtest)
library(sandwich)
library(tidyverse)

##
# Figure 2
##

savepdf <- function(file, width=7.28, height=3.54){ 
    fname <- paste("",file,".pdf",sep="")
    pdf(fname, width=width, height=height,
        pointsize=7)
    par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

savepdf("output/Fukumoto_Fig2")
par(mfrow=c(2,4))

covs <- c("D2201 + A1301 + A1303 + A1801 + A6103 + A6104 + A6105 + A6106 + F1101 + I6100 + I5101 + I5102 + I5211 + I5212 + E2501 + E3501 + unemployment + per.capita.income + student.per.school.1 + student.per.school.2 + A6108 + D320101 + D3202 + A5103 + A5104 + livable + density + revenue.per.capita + F2201 + F2211 + population + number.of.health.centers + as.factor(pref) + lon + lat + prec_mean + shine_mean + tmean_mean + electoral.time + cases_1 + cases_2 + cases_3 + cases_4 + cases_5 + cases_6 + cases_7 + log.number + prior.infection.per.capita")

# March 4
p_path <- "preprocess/preprocess.0304.R"
m_path <- "matching_results/alt_atc_0304_1000_1000.RDS"
lm_form <- as.formula(paste("gm[, part[p]] ~ shutdown.0304 + ", covs, " + age.0304", " + win_count.0304", sep=""))
source("genetic_matching/gen.0304.R")
plot(YMD[part.adj], summary.Match.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C, average.T, max.UB)),
     xlab="Day", ylab="Treatment effect",
     xaxt="n",
     type="l",
     main = "a March 4")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 4
abline(0,0,lty=2)

# Fukumoto_Fig2_a_source
Fukumoto_Fig2_a <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
colnames(Fukumoto_Fig2_a) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_Fig2_a, "output/Fukumoto_Fig2_a_source.csv", row.names = FALSE)

# March 16
p_path <- "preprocess/preprocess.0316.R"
m_path <- "matching_results/alt_atc_0316_1000_1000.RDS"
lm_form <- as.formula(paste("gm[, part[p]] ~ shutdown.0316 + ", covs, " + age.0316", " + win_count.0316", " + shutdown.0304", sep=""))
source("genetic_matching/gen.0316.R")
plot(YMD[part.adj], summary.Match.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C, average.T, max.UB)),
     xlab="Day", ylab="Treatment effect",
     type="l",
     xaxt="n",
     main = "b March 16")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 16
abline(0,0,lty=2)

# Fukumoto_Fig2_b_source
Fukumoto_Fig2_b <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
colnames(Fukumoto_Fig2_b) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_Fig2_b, "output/Fukumoto_Fig2_b_source.csv", row.names = FALSE)

# April 6
p_path <- "preprocess/preprocess.0406.R"
m_path <- "matching_results/alt_atc_0406_1000_1000.RDS"
lm_form <- as.formula(paste("gm[, part[p]] ~ shutdown.0406 + ", covs, " + age.0406", " + win_count.0406", " + shutdown.0304", " + shutdown.0316", sep=""))
source("genetic_matching/gen.0406.R")
plot(YMD[part.adj], summary.Match.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C, average.T, max.UB)),
     xlab="Day", ylab="Treatment effect",
     type="l",
     xaxt="n",
     main = "c April 6")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 6
abline(0,0,lty=2)

# Fukumoto_Fig2_c_source
Fukumoto_Fig2_c <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
colnames(Fukumoto_Fig2_c) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_Fig2_c, "output/Fukumoto_Fig2_c_source.csv", row.names = FALSE)

# April 10
p_path <- "preprocess/preprocess.0410.R"
m_path <- "matching_results/alt_atc_0410_1000_1000.RDS"
lm_form <- as.formula(paste("gm[, part[p]] ~ shutdown.0410 + ", covs, " + age.0410", " + win_count.0410", " + shutdown.0304", " + shutdown.0316", " + shutdown.0406", sep=""))
source("genetic_matching/gen.0410.R")
plot(YMD[part.adj], summary.Match.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C, average.T, max.UB)),
     xlab="Day", ylab="Treatment effect",
     type="l",
     xaxt="n",
     main = "d April 10")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 10
abline(0,0,lty=2)

# Fukumoto_Fig2_d_source
Fukumoto_Fig2_d <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
colnames(Fukumoto_Fig2_d) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_Fig2_d, "output/Fukumoto_Fig2_d_source.csv", row.names = FALSE)

# # April 16
# p_path <- "preprocess/preprocess.0416.R"
# m_path <- "matching_results/alt_atc_0416_1000_1000.RDS"
# lm_form <- as.formula(paste("gm[, part[p]] ~ shutdown.0416 + ", covs, " + age.0416", " + win_count.0416", " + shutdown.0304", " + shutdown.0316", " + shutdown.0406", " + shutdown.0410", sep=""))
# source("genetic_matching/gen.0416.R")
# plot(YMD[part.adj], summary.Match.out[, 1],
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      ylim=c(min(0,min.LB),
#             max(average.all.T[part.adj], average.C, average.T, max.UB)),
#      xlab="Day", ylab="Treatment effect",
#      type="l",
#      xaxt="n",
#      main = "e April 16")
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
# lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 16
# abline(0,0,lty=2)
# 
# # Fukumoto_Fig2_e_source
# Fukumoto_Fig2_e <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
# colnames(Fukumoto_Fig2_e) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                "upper bound of 95% confidence interval")
# write.csv(Fukumoto_Fig2_e, "output/Fukumoto_Fig2_e_source.csv", row.names = FALSE)
# 
# # April 22
# p_path <- "preprocess/preprocess.0422.R"
# m_path <- "matching_results/alt_atc_0422_1000_1000.RDS"
# lm_form <- as.formula(paste("gm[, part[p]] ~ shutdown.0422 + ", covs, " + age.0422", " + win_count.0422", " + shutdown.0304", " + shutdown.0316", " + shutdown.0406", " + shutdown.0410", " + shutdown.0416", sep=""))
# source("genetic_matching/gen.0422.R")
# plot(YMD[part.adj], summary.Match.out[, 1],
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      ylim=c(min(0,min.LB),
#             max(average.all.T[part.adj], average.C, average.T, max.UB)),
#      xlab="Day", ylab="Treatment effect",
#      type="l",
#      xaxt="n",
#      main = "f April 22")
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
# lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part.adj[8]], col = "turquoise") # April 22
# abline(0,0,lty=2)
# 
# # Fukumoto_Fig2_f_source
# Fukumoto_Fig2_f <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
# colnames(Fukumoto_Fig2_f) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                "upper bound of 95% confidence interval")
# write.csv(Fukumoto_Fig2_f, "output/Fukumoto_Fig2_f_source.csv", row.names = FALSE)
# 
# # May 11
# p_path <- "preprocess/preprocess.0511.R"
# m_path <- "matching_results/alt_atc_0511_1000_1000.RDS"
# lm_form <- as.formula(paste("gm[, part[p]] ~ shutdown.0511 + ", covs, " + age.0511", " + win_count.0511", " + shutdown.0304", " + shutdown.0316", " + shutdown.0406", " + shutdown.0410", " + shutdown.0416", " + shutdown.0511", sep=""))
# source("genetic_matching/gen.0511.R")
# plot(YMD[part.adj], summary.Match.out[, 1],
#      xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
#      ylim=c(min(0,min.LB),
#             max(average.all.T[part.adj], average.C, average.T, max.UB)),
#      xlab="Day", ylab="Treatment effect",
#      type="l",
#      xaxt="n",
#      main = "g May 11")
# axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
#      labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
# polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
# lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
# abline(v=YMD[part.adj[8]], col = "turquoise") # May 11
# abline(0,0,lty=2)
# 
# # Fukumoto_Fig2_g_source
# Fukumoto_Fig2_g <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
# colnames(Fukumoto_Fig2_g) <- c("date", "ATC", "lower bound of 95% confidence interval",
#                                "upper bound of 95% confidence interval")
# write.csv(Fukumoto_Fig2_g, "output/Fukumoto_Fig2_g_source.csv", row.names = FALSE)

dev.off()
