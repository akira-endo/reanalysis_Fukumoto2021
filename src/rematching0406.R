## This replication file
## makes the following tables and figures:
# Figure 1

# Packages
library(MatchIt)
library(lmtest)
library(sandwich)
library(tidyverse)

##
# Figure 1
##

# April 6
p_path <- "../Fukumoto2021/preprocess/preprocess.0406.R"
m_path <- "../output/rematch_alt_atc_0406_1000_1000.RDS"
lm_form <- as.formula("gm[, part[p]] ~ shutdown.0406")
source("gen.0406.unmatch.R",chdir=T)
plot(YMD[part.adj], average.C, col = 2, lty = 1,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     type="n",
     xaxt="n",
     main=paste("Outcome (April 6)"),
     ylim=c(0,2))
            #max(average.all.T[part.adj], average.C, average.T, max.UB) ))
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
lines(YMD[part.adj], average.C, col = 2, lty = 1, lwd = 1)
lines(YMD[part.adj], average.T, col = 1, lty = 1, lwd = 1)
lines(YMD[part.adj], average.all.T[part.adj], col = 1, lty = 2, lwd = 1)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 6

plot(YMD[part.adj], summary.Match.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     #ylim=c(-6,6),
     ylim=c(-0.2,0.5),#c(min(0,min.LB),
          #  max( average.C, average.T, max.UB)),
     xlab="Day", ylab="ATC",
     type="n",
     xaxt="n",
     main = "ATC (April 6)")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(UB, rev(LB)), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.Match.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # April 6
abline(0,0,lty=2)

# Fukumoto_Fig1_e_source
Fukumoto_Fig1_e <- cbind(as.character(YMD[part.adj]), average.C, average.T, average.all.T[part.adj])
colnames(Fukumoto_Fig1_e) <- c("date", "outcome average in matched control municipalities", "outcome average in matched treated municipalities",
                         "outcome average in all treated municipalities")
write.csv(Fukumoto_Fig1_e, "../output/Fukumoto_Fig1_e_rematch_source.csv", row.names = FALSE)

# Fukumoto_Fig1_f_source
Fukumoto_Fig1_f <- cbind(as.character(YMD[part.adj]), summary.Match.out[, 1], LB, UB)
colnames(Fukumoto_Fig1_f) <- c("date", "ATC", "lower bound of 95% confidence interval",
                         "upper bound of 95% confidence interval")
write.csv(Fukumoto_Fig1_f, "../output/Fukumoto_Fig1_f_rematch_source.csv", row.names = FALSE)

## Count unmatched samples
unmatchedsamples=sapply(1:25,function(col){sum(pref.dummy[gm$id[1:(nrow(gm)/2)*2]%>%as.numeric ,col]!=pref.dummy[gm$id[1:(nrow(gm)/2)*2-1]%>%as.numeric ,col])}) # count the number of unmatches for each pref dummy. The sum will be 2 * # unmatched pairs as the unmatches are counted twice for each of the pair
unmatchedid0406=(sapply(1:25,function(col){(pref.dummy[gm$id[1:(nrow(gm)/2)*2]%>%as.numeric ,col]!=pref.dummy[gm$id[1:(nrow(gm)/2)*2-1]%>%as.numeric ,col])%>%rep(.,each=2) })%>%rowSums)!=0

cat("April 6:",(sum(unmatchedsamples)/2)%>%floor, "matched pairs out of", nrow(gm)/2,"had their prefecture dummy variables unmatched for.\n")

m.out$nn