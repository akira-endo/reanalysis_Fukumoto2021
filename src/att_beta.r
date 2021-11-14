## This replication file
## makes the following tables and figures:
# Figure 5

# Packages
library(MatchIt)
library(lmtest)
library(sandwich)
library(tidyverse)

##
# Figure 5
##

#savepdf <- function(file, width=7.28, height=3.54){ # 3.46, 1.77
#    fname <- paste("",file,".pdf",sep="")
#    pdf(fname, width=width, height=height,
#        pointsize=7)
#    par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
#}

#savepdf("output/Fukumoto_Fig5")

p_path <- "../Fukumoto2021/preprocess/preprocess.0406.R"
m_path <- "../Fukumoto2021/matching_results/alt_att_0406_1000_1000.RDS"
source("gen.0406.att_beta.R")

plot(YMD[part.adj], average.C2, col = 2, lty = 2,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     type="l",
     main = paste("Outcome (April 6): ", reduction*100, "% mitigating effect"),
     ylim=c(0,1))
lines(YMD[part.adj], average.C2, col = 2, lty = 1)
lines(YMD[part.adj], average.T2, lty = 1)
#lines(YMD[part.adj], average.all.T2[part.adj], lty = 2)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 6

plot(YMD[part.adj], summary.Match.out2[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(-0.5,0.5),
     main = "ATT",
     xlab="Day", ylab="ATT",
     type="l")
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB2, rev(LB2)), col = shadecolor, border = NA)
lines(YMD[part.adj], summary.Match.out2[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 6
abline(0,0,lty=2)

# Fukumoto_Fig5_a_source
Fukumoto_Fig5_a <- cbind(as.character(YMD[part.adj]), average.C2, average.T2, average.all.T2[part.adj])
colnames(Fukumoto_Fig5_a) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
                               "outcome average in all treated municipalities")
write.csv(Fukumoto_Fig5_a, "../output/Fukumoto_Fig5_a_beta_source.csv", row.names = FALSE)

# Fukumoto_Fig5_b_source
Fukumoto_Fig5_b <- cbind(as.character(YMD[part.adj]), summary.Match.out2[, 1], LB2, UB2)
colnames(Fukumoto_Fig5_b) <- c("date", "ATC", "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval")
write.csv(Fukumoto_Fig5_b, "../output/Fukumoto_Fig5_b_beta_source.csv", row.names = FALSE)


