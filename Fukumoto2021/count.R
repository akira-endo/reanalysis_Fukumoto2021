## This replication file
## makes the following tables and figures:
# Tables S6 and S7
# Figure S4

library(tidyverse)

# Load neighbor functions
source("neighbor/pq.neighbor.functions.R")

data <- read.csv("covid.csv")

survey.dates <- c("0304", "0316", "0406", "0410")#,
                  #"0416", "0422", "0511") #, "0601")
table.out <- matrix(NA, length(survey.dates), 12)
matched <- rep(NA, length(survey.dates))

# Jan. 25 - Aug. 10
period <- 52:250 # indicator
YMD <- as.Date("2020-1-25") + 1:length(period) - 1 # year-month-day
survey.date.col <- c(91, 103, 124, 128, 134, 140, 159) - 51
survey.date.col <- survey.date.col[1:length(survey.dates)]
rownames(table.out) <- as.character(YMD[survey.date.col])

for(d in 1:length(survey.dates)){
  day <- survey.dates[d]
  source(paste0("neighbor/preprocess_count/n.preprocess_count.", day, ".R"))
  table.out[d,2:1] <- table(treat)
  table.out[d,3] <- nrow(data.analysis)
  neighbor.treatment.3 <- ifelse(data.analysis$neighbor.treatment==1,1,
                               ifelse(data.analysis$neighbor.treatment==0,0,9))
  table.out[d,4] <- sum(neighbor.treatment.3==1)
  table.out[d,5] <- sum(neighbor.treatment.3==0)
  table.out[d,6] <- sum(neighbor.treatment.3==9)
  table.out[d,7] <- sum(treat[neighbor.treatment.3==1]==1)
  table.out[d,8] <- sum(treat[neighbor.treatment.3==1]==0)
  table.out[d,9] <- sum(treat[neighbor.treatment.3==0]==1)
  table.out[d,10] <- sum(treat[neighbor.treatment.3==0]==0)
  table.out[d,11] <- sum(treat[neighbor.treatment.3==9]==1)
  table.out[d,12] <- sum(treat[neighbor.treatment.3==9]==0)
  if(d!=4){
    m.out <- readRDS(paste0("neighbor/results/neighbor_atc_", day, "_1000_1000.RDS"))
    matched[d] <- length(unique(m.out$match.matrix))
  }
}

# Table S6
table.S6 <- table.out[, c(4:6, 3)]
colnames(table.S6) <- c("Treated", "Control", "Mixed", "All")
table.S6
write.csv(table.S6, "output/Fukumoto_TableS6.csv")

# Table S7
table.S7 <- table.out[, 7:8]
colnames(table.S7) <- c("Treated", "Control")
table.S7
write.csv(table.S7, "output/Fukumoto_TableS7.csv")

d <- 3
day <- survey.dates[d]
source(paste0("neighbor/preprocess_count/n.preprocess_count.", day, ".R"))

savepdf <- function(file, width=3.46, height=3.46){ 
  fname <- paste("",file,".pdf",sep="")
  pdf(fname, width=width, height=height,
      pointsize=7)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

# Figure S4
savepdf("output/Fukumoto_FigS4")
hist.out <- hist(trunc(data.analysis$neighbor.treatment*100), breaks=0:100, probability=T,
                 ylim=c(0, 0.5), main=c(""),
                 xlab=c("Percentage of Treated Neighbors"),
                 ylab=c("Proportion"))
dev.off()

Figure.S4.source <- cbind(hist.out$mids - 0.5, hist.out$mids + 0.5, hist.out$density)
colnames(Figure.S4.source) <- c("left end", "right end", "Proportion")
head(Figure.S4.source)
write.csv(Figure.S4.source, "output/Fukumoto_FigS4_source.csv", row.names = F)