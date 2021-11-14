## This replication file
## makes the following tables and figures:
# Figure S5
# Tables S8-S10

library(tidyverse)

data <- read.csv("covid.csv")
survey.dates <- c("0304", "0316", "0406", "0410")#, "0416", "0422", "0511", "0601")
#survey.date.col <- c(91, 103, 124, 128, 134, 140, 159, 180) - 51
survey.date.col <- c(91, 103, 124, 128, 134) - 51
t.test.pref <- 
  t.test.miss <- 
  fisher.test.pref <- 
  fisher.test.miss <- 
  outcome.out <- list()
table.category <- matrix(NA, 4, 3) #matrix(NA, 7, 3)
test.pref.out <- test.miss.out <- matrix(NA, 4, 2) #matrix(NA, 7, 2)

# Jan. 25 - Aug. 10
period <- 52:250 # indicator
YMD <- as.Date("2020-1-25") + 1:length(period) - 1 # year-month-day
survey.date.col.tab <- c(91, 103, 124, 128) - 51

#for(d in 1:7){
for(d in 1:4){
  if(d < 1){
    rm(data.analysis)
    rm(data.miss)
  }
  source("missing.subroutine.r")
}

# Table S8
table.S8 <- table.category[, c(3,2,1)]
colnames(table.S8) <- c("Analyzed", "Missing", "Outsiude the target")
rownames(table.S8) <- as.character(YMD[survey.date.col.tab])
table.S8
write.csv(table.S8, "output/Fukumoto_TableS8.csv")

#for(d in 1:7){
for(d in 1:4){
  test.pref <- c(t.test.pref[[d]], fisher.test.pref[[d]])
  test.miss <- c(t.test.miss[[d]], fisher.test.miss[[d]])
  test.pref.out[d, 1] <- length(test.pref)
  test.pref.out[d, 2] <- sum(test.pref < 0.05)
  test.miss.out[d, 1] <- length(test.miss)
  test.miss.out[d, 2] <- sum(test.miss < 0.05)
}

# Table S9
test.miss.out <- cbind(test.miss.out[, 1:2],
                       round(test.miss.out[, 2]*100/test.miss.out[,1], 1))
colnames(test.miss.out) <- c("All", "Different", "(%)")
rownames(test.miss.out) <- as.character(YMD[survey.date.col.tab])
test.miss.out
write.csv(test.miss.out, "output/Fukumoto_TableS9.csv")

# Table S10
test.pref.out <- cbind(test.pref.out[, 1:2],
                       round(test.pref.out[, 2]*100/test.pref.out[,1], 1))
colnames(test.pref.out) <- c("All", "Different", "(%)")
rownames(test.pref.out) <- as.character(YMD[survey.date.col.tab])
test.pref.out
write.csv(test.pref.out, "output/Fukumoto_TableS10.csv")

outcome.out.all <- NULL
#for(d in 1:7){
for(d in 1:4){
  outcome.out.all <- rbind(outcome.out.all, outcome.out[[d]])
}
#outcome.out.all <- outcome.out.all[-90, ]

# "The differences are never significantly different from zero " (SI, p. 18).
sum(outcome.out.all[, 6] < 0.05) 

survey.date.pos <- survey.date.col - 39
part.adj <- 94:(93 + nrow(outcome.out.all)) - 54
part.adj.rev <- 94:(93 + nrow(outcome.out.all) + 1) - 54
X <- YMD[part.adj]
XX <- YMD[part.adj.rev]

y.max <- max(outcome.out.all[, 3:4])
y.min <- min(outcome.out.all[, 3:4])

# Figure S5
savepdf <- function(file, width=7.28, height=3.54){ 
  fname <- paste("",file,".pdf",sep="")
  pdf(fname, width=width, height=height,
      pointsize=7)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

savepdf("output/Fukumoto_FigS5")
par(mfcol=c(1,2))
# left panel
plot(X, outcome.out.all[, 1], type = "l", ylim = c(0, y.max - y.min),
     xaxt="n",
     xlab="Day", ylab="Outcome", main="a Average outcome")
axis(1, at = c(XX[1], XX[29], XX[59], XX[90]),
     labels = format(c(XX[1], XX[29], XX[59], XX[90]), "%b %d"))
lines(X, outcome.out.all[, 2], col = 2)
abline(0, 0, lty = 2)
for(d in 1:8){
  abline(v = XX[survey.date.pos[d]], col="Turquoise")
}
# right panel
plot(X, outcome.out.all[, 5], type = "n", ylim = c(y.min, y.max),
     xlab="Day", ylab="Difference in outcomes", main="b Difference in outcomes")
polygon(c(X, rev(X)),
        c(outcome.out.all[, 3], rev(outcome.out.all[, 4])),
        col = "gray90", border = "gray90")
lines(X, outcome.out.all[, 5], type = "l", ylim = c(y.min, y.max), lwd = 2)
abline(0, 0, lty = 2)
for(d in 1:8){
  abline(v = XX[survey.date.pos[d]], col="Turquoise")
}
dev.off()

rownames(outcome.out.all) <- as.character(X)
colnames(outcome.out.all) <- c("outcome average in analyzed municipalities", 
                               "outcome average in missing municipalities",
                               "lower bound of 95% confidence interval",
                               "upper bound of 95% confidence interval", 
                               "difference in the two outcome averages",
                               "p-value")
head(outcome.out.all)
write.csv(outcome.out.all, "output/Fukumoto_FigS5_source.csv")
