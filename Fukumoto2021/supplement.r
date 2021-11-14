## This replication file
## makes the following tables and figures:
# Tables 2 and S2-S5
# Figures S1-S3
## calculates numbers mentioned in Supplementary Information (SI), Main analysis section

library(MatchIt) # get_matches()
library(cobalt) # bal.tab()
library(lmtest) # coeftest()
library(sandwich) # vcovCL()
library(tidyverse)

data <- read.csv("covid.csv")
data.all <- data[data$pref %in% c(3, 6, 7, 9, 11, 12, 13, 15,
                                  16, 18, 21, 23, 25, 27, 31,
                                  32, 33, 34, 35, 36, 37, 38,
                                  41, 42, 44, 45, 46),]
survey.dates <- c("0304", "0316", "0406", "0410")#, "0416", "0422", "0511", "0601")
period <- 84:180 # indicates Feb.26 to June 1 
# Replace -1's with 0's
cases <- NULL
for(p in 1:length(period)){
  data.all[, period[p]] <- as.character(data.all[, period[p]])
  data.all[, period[p]] <- gsub("-1", "0", data.all[, period[p]])
  data.all[, period[p]] <- as.integer(data.all[, period[p]]) 
  cases <- c(cases, data.all[, period[p]])
}

##################################################################
# The Numbers of Treated and Control Municipalities by Survey Date

table.treat <- matrix(NA, length(survey.dates), 4)
table.health.mun <- matrix(NA, length(survey.dates), 6)
table.health.cen <- matrix(NA, length(survey.dates), 2)
matched <- rep(NA, 7)

# Jan. 25 - Aug. 10
period <- 52:250 # indicator
YMD <- as.Date("2020-1-25") + 1:length(period) - 1 # year-month-day
survey.date.col <- c(91, 103, 124, 128, 134, 140, 159, 180) - 51
survey.date.col <- survey.date.col[1:length(survey.dates)]
rownames(table.treat) <- 
  rownames(table.health.mun) <- 
  rownames(table.health.cen) <- as.character(YMD[survey.date.col])

for(d in 1:length(survey.dates)){
  day <- survey.dates[d]
  
  source(paste0("preprocess/preprocess.", day, ".R"))
  table.treat[d, 1:2] <- table(treat)
  
  table.out <- table(data.analysis$health.center.id, treat)
  mix <- as.integer(table.out[, 1] * table.out[, 2] > 0) # whether the health center has treated and control municipalities
  table.health.mun[d, 1:2] <- c(sum(mix * table.out[, 1]), # the number of treated municipalities in a mixed health center
                                sum(mix * table.out[, 2])) # the number of control municipalities in a mixed health center
  table.health.cen[d, 1:2] <- c(nrow(table.out),
                                sum(mix)) # whether the health center has treated and control municipalities
  if(d < 8){
    m.out <- readRDS(paste0("matching_results/alt_atc_", day, "_1000_1000.RDS"))
    matched[d] <- length(unique(m.out$match.matrix))
  }
}

# Table 2  
table.treat[,3] <- apply(table.treat[,1:2],1,sum)
table.treat[,4] <- round(100*table.treat[,2]/table.treat[,3], 1)
table.treat[,1:2] <- table.treat[,2:1]
colnames(table.treat) <- c("Treated", "Control", "All", "Treated (%)")
table.treat
write.csv(table.treat, "output/Fukumoto_Table2.csv")

# Table S2
#(table.matched <- cbind(matched[1:7], table.treat[1:7,2]))
table.matched <- cbind(matched[1:4], table.treat[1:4,2])
colnames(table.matched) <- c("Treated", "Control")
table.matched
write.csv(table.matched, "output/Fukumoto_TableS2.csv")

# Table S4
#table.health.cen <- table.health.cen[-8,]
colnames(table.health.cen) <- c("All", "Mixed")
table.health.cen
write.csv(table.health.cen, "output/Fukumoto_TableS4.csv")

# Table S5
table.health.mun[, 1:2] <- table.health.mun[, 2:1]
table.health.mun[, 4:5] <- table.treat[, 1:2] - table.health.mun[, 1:2]
table.health.mun[, 3] <- apply(table.health.mun[, 1:2], 1, sum)
table.health.mun[, 6] <- apply(table.health.mun[, 4:5], 1, sum)
#table.health.mun <- table.health.mun[-8,]
colnames(table.health.mun) <- c("Treated (Mixed)", "Control (Mixed)", "All (Mixed)", 
                                "Treated (Non-mixed)", "Control (Non-mixed)", "All (Non-mixed)")
table.health.mun
write.csv(table.health.mun, "output/Fukumoto_TableS5.csv")

##############################
# Negative Binomial Regression

# "our analysis covers all of the 847 municipalities in the 27 target prefectures
# from February 26 to June 1 (97 days)." (SI, p. 12)
length(cases)
847 * 97
# "the number of cases is not available for 62 municipalities in Tokyo Prefecture
# from February 26 to March 31 (35 days)." (SI, p. 12)
sum(is.na(cases)) # Tokyo
62 * 35
# "Thus, the total number of units is 847 * 97 - 62 * 35 = 79,989." (SI, p. 12)
table.cases <- table(cases)
sum(table.cases)
length(cases) - sum(is.na(cases))

prop.table.cases <- round(prop.table(table.cases)*100, 1)
# "the number of cases is equal to zero for 95.4% of our 79,989 municipality-day observations
# and less than 10 for 99.9% of our observations." (SI, p. 12)
prop.table.cases[1]
sum(prop.table.cases[1:10])

savepdf <- function(file, width=3.46, height=3.46){ 
  fname <- paste("",file,".pdf",sep="")
  pdf(fname, width=width, height=height,
      pointsize=7)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

# Figure S2
savepdf("output/Fukumoto_FigS2")
hist.out <- hist(cases, freq = F, 
                 breaks = (min(cases, na.rm = T) - 1):max(cases, na.rm = T) + 0.5, 
                 xlab="Number of cases", ylab="Proportion", main="", 
                 ylim=c(0, 0.025))
dev.off()

Figure.S2.source <- cbind(hist.out$mids, hist.out$density)
colnames(Figure.S2.source) <- c("Number of cases", "Proportion")
head(Figure.S2.source)
write.csv(Figure.S2.source, "output/Fukumoto_FigS2_source.csv", row.names = F)

######################
# Public Health Center

# "Each of the 263 public health centers is in charge of implementing public health policy, 
# including counting 26 COVID-19 cases, and covers 1 to 13 municipalities." (Main Text)
(table.out.2 <- table(table(data.all$health.center.id)))
sum(table.out.2)
# "66.9% of the 263 public health centers cover multiple municipalities, 
# which occupy 89.7% of the 847 municipalities." (SI, pp. 12-13)
table.out.1 <- table(data.all$health.center.id)
sum(table.out.1)
round((1 - table.out.2[1]/sum(table.out.2))*100, 1)
round((1 - table.out.2[1]/sum(table.out.1))*100, 1)

# Figure S3
savepdf("output/Fukumoto_FigS3")
hist.out <- hist(table.out.1, freq = F, breaks = 0:13 +0.5, 
                 xlab="Number of municipalities per public health center", 
                 ylab="Proportion", main="")
dev.off()

Figure.S3.source <- cbind(hist.out$mids, hist.out$density)
colnames(Figure.S3.source) <- c("Number of municipalities", "Proportion")
Figure.S3.source
write.csv(Figure.S3.source, "output/Fukumoto_FigS3_source.csv", row.names = F)

#######
# setup

additional <- c("cases_1", "cases_2", "cases_3", "cases_4", "cases_5", "cases_6", "cases_7", 
                "prior.infection.per.capita", 
                "lon", "lat", "prec_mean", "shine_mean",  "tmean_mean",  "electoral.time", "log.number", 
                "shutdown.0304", "shutdown.0316", "shutdown.0406", "shutdown.0410", "shutdown.0416", "shutdown.0422")
bal.tab.out <- list()
mains <- c("a March 4", "b March 16", "c April 6", "d April 10",
           "e April 16", "f April 22", "g May 11", "h April 6 (ATT)")

######
# ASMD

#for (d in 1:8){
for (d in c(1:4, 8)){
  if(d < 8){
    dd <- d
    day <- survey.dates[dd]
    m_path <- paste0("matching_results/alt_atc_", day, "_1000_1000.RDS")
  }
  if(d == 8){# ATT
    dd <- 3
    day <- survey.dates[dd]
    m_path <- paste0("matching_results/alt_att_", day, "_1000_1000.RDS") #
  }  
  p_path <- paste0("preprocess/preprocess.", day, ".R")
  lm_form <- as.formula(paste0("gm[, part[p]] ~ shutdown.", day))  
  source(paste0("genetic_matching/gen.", day, ".R"))
  bal.tab.out[[d]] <- bal.tab(cbind(control, pref.dummy, 
                                    data.analysis[, additional[1:(dd + 14)]],
                                    data.analysis[, c(paste0("age.", day),
                                                      paste0("win_count.", day))]),
                              binary = "std", continuous = "std", treat = m.out$treat, 
                              weights = m.out$weights, abs = TRUE, stats = c("mean.diffs"), un = TRUE)$Balance
  bal.tab.out[[d]] <- bal.tab.out[[d]] %>%
    mutate(above2 = ifelse(Diff.Un > 2, "Yes", "No"),
           Diff.Un.draw = ifelse(Diff.Un > 2, 2, Diff.Un),
           Diff.Adj.draw = ifelse(Diff.Adj > 2, 2, Diff.Adj),
           Diff.diff = Diff.Un - Diff.Adj,
           Diff.ratio = Diff.Adj / Diff.Un)
}

savepdf <- function(file, width=7.28, height=3.54){ 
  fname <- paste("",file,".pdf",sep="")
  pdf(fname, width=width, height=height,
      pointsize=7)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

savepdf("output/Fukumoto_FigS1")
par(mfrow=c(2,4))

#for (d in 1:8){
for (d in c(1:4, 8)){
  plot(bal.tab.out[[d]]$Diff.Un.draw, bal.tab.out[[d]]$Diff.Adj.draw, 
       xlim=c(0,2), ylim=c(0,2), 
       xlab = "ASMD before matching", ylab="ASMD after matching", 
       main = mains[d], 
       pch=c(1, 3)[as.factor(bal.tab.out[[d]]$above2)])
  abline(0, 1, lty = 2)
}
dev.off()

# Table S3
covariate_list <- read.csv("covariate_list.csv")
#for (d in 1:8){
for (d in c(1:4, 8)){
  Figure.S1.d <- bal.tab.out[[d]][,c("Diff.Un", "Diff.Adj")]
  dd <- d
  if(d == 8){# ATT
    dd <- 3
  }
  if(dd <= 3){
    pref.dummy.pos <- c(33:38, 40:58) # excluding Tokyo
  }
  if(dd >= 4){
    pref.dummy.pos <- c(33:58) # including Tokyo
  }
  rownames(Figure.S1.d) <- covariate_list[c(1:32, #control
                                            pref.dummy.pos, 
                                            (58 + 1):(58 + 14 + dd), #additional[1:(dd + 14)]]
                                            80, 81), # age, win_count
                                            2]
  Figure.S1.d <- Figure.S1.d[rev(order(Figure.S1.d[, "Diff.Un"])),]
  colnames(Figure.S1.d) <- c("ASMD before matching", "ASMD after matching")
  write.csv(Figure.S1.d, paste0("output/Fukumoto_FigS1_", letters[d], "_source.csv"))
  if(d == 3){
    Figure.S1.d <- cbind(Figure.S1.d, Figure.S1.d[, 2]/Figure.S1.d[, 1])
    colnames(Figure.S1.d)[3] <- c("Ratio")
    Figure.S1.d <- round(Figure.S1.d, 2)
    write.csv(Figure.S1.d, "output/Fukumoto_TableS3.csv")
  }
}

###########################
# Supplementary Information
# Main Analysis

col.shutdown <- 44:51

# March 4
d <- 1
(day <- survey.dates[d])
source(paste0("preprocess/preprocess.", day, ".R"))
shutdown <- data.analysis[,col.shutdown]
shutdown[is.na(shutdown)] <- 9
(table.out <- table(shutdown[,1], shutdown[,2]))
# "Since 9 of the 10 (90.0\%) control municipalities also had open schools as of the next survey date (March 16)," (SI, p. 8) 
table.out[1,1]/sum(table.out[1,])
# "718 (94.3\%) % 718/761 treated municipalities reported that their schools continued to be closed until March 16." (SI, p. 8) 
table.out[2,2]/sum(table.out[2,])

# # April 6
# d <- 3
# (day <- survey.dates[d])
# source(paste0("preprocess/preprocess.", day, ".R"))
# shutdown <- data.analysis[,col.shutdown]
# shutdown[is.na(shutdown)] <- 9
# # "247 of the 256 (96.5\%) treated municipalities as of April 6 closed schools on all of the following three survey dates as well (April 10, 16, and 22)," (SI, p. 10)  
# all.close <- shutdown[,4]==1 & shutdown[,5]==1 & shutdown[,6]==1 
# (table.out <- table(shutdown[,d], all.close))
# table.out[2,2]/sum(table.out[2,])
# # "though only 79 of the 483 (16.4\%) control municipalities opened their schools on all three dates." (SI, p. 10)  
# all.open <- shutdown[,4]==0 & shutdown[,5]==0 & shutdown[,6]==0 
# (table.out <- table(shutdown[,d], all.open))
# table.out[1,2]/sum(table.out[1,])
# 
# # April 22
# d <- 6
# (day <- survey.dates[d])
# source(paste0("preprocess/preprocess.", day, ".R"))
# shutdown <- data.analysis[,col.shutdown]
# shutdown[is.na(shutdown)] <- 9
# (table.out <- table(shutdown[,d], shutdown[,7]))
# # "We find that 70 of the 80 (87.5%) control municipalities and 634 (89.3%) of 710 treated municipalities as of April 22 opened and closed their schools as of the next survey date (May 11), respectively." (SI, p. 11)
# table.out[1,1]/sum(table.out[1,])
# table.out[2,2]/sum(table.out[2,])
# 
# # May 11
# d <- 7
# (day <- survey.dates[d])
# source(paste0("preprocess/preprocess.", day, ".R"))
# shutdown <- data.analysis[,col.shutdown]
# shutdown[is.na(shutdown)] <- 9
# (table.out <- table(shutdown[,d], shutdown[,8]))
# # "just 2 of the 641 (0.3%) treated municipalities as of May 11 reported closed schools as of the next survey date (June 1)." (SI, p. 11)
# table.out[2,2]/sum(table.out[2,])
# # "All of the control municipalities as of May 11 said that their schools were also open on June 1." (SI, p. 12)
# table.out[1,1]/sum(table.out[1,])

##################
# Irregular values

# March 16

d <- 2
(day <- survey.dates[d])
source(paste0("preprocess/preprocess.", day, ".R"))
outcome <- outcome/10
v1 <- c(66:68)
v2 <- c(117:119)
m.out <- readRDS(paste0("matching_results/alt_atc_", day, "_1000_1000.RDS"))
# Create matched dataset
gm <- get_matches(m.out, data = data.analysis)
# estimate the ATC for every day from one week before (March 30) to three weeks after (April 27)
part <- 99:127 # indicates March 9 to April 6
gm[,part] <- (10^5)*gm[, part]/gm$pop.2020
summary.Match.out <- rep(NA, length(part))
for(p in 1:length(part)){
  fit <- lm(gm[, part[p]] ~ shutdown.0316,
            data = gm, weights = weights)
  summary.Match.out[p] <- coef(summary(fit))[2,1]
}
names(summary.Match.out) <- colnames(gm[,part])
summary.Match.out[24]

# "on April 1 ... In Wadomari Town, Kagoshima Prefecture, in the control group had a case among its 6,537 residents, which is equivalent to 15.3 cases per 100,000 residents." (SI, p. 9)
vv <- 3
colnames(outcome)[v1[vv]]
use <- which(treat == 0 & data.analysis[,v2[vv]]>0)
outlier <- cbind(data.analysis[use, c("municipality_code", "pop.2020")], 
                 data.analysis[use, v2[vv]],
                 outcome[use, v1[vv]])
outlier[2,]
# "This single municipality contributes to 103.3\% $(=(-15.3/29)/(-0.511))$ of the ATC (-0.511)." (SI, p. 9)
(-outlier[2,4]/table(treat)[1])/summary.Match.out[24]
# "the mean of population among all of the 785 municipalities in the 26 target prefectures is 76,830." (SI, p. 9)
mean(data.all$pop.2020[data.all$pref != 13])

# # April 16
# 
# d <- 5
# (day <- survey.dates[d])
# source(paste0("preprocess/preprocess.", day, ".R"))
# outcome <- outcome/10
# v1 <- c(89:92,98:101)
# v2 <- c(140:143, 149:152)
# m.out <- readRDS(paste0("matching_results/alt_atc_", day, "_1000_1000.RDS"))
# # Create matched dataset
# gm <- get_matches(m.out, data = data.analysis)
# # estimate the ATC for every day from one week before (March 30) to three weeks after (April 27)
# part <- 130:158 # indicates April 9 to May 7
# gm[,part] <- (10^5)*gm[, part]/gm$pop.2020
# summary.Match.out <- rep(NA, length(part))
# for(p in 1:length(part)){
#   fit <- lm(gm[, part[p]] ~ shutdown.0416,
#             data = gm, weights = weights)
#   summary.Match.out[p] <- coef(summary(fit))[2,1]
# }
# names(summary.Match.out) <- colnames(gm[,part])
# summary.Match.out[c(15, 25)]
# 
# vv <- 2
# colnames(outcome)[v1[vv]]
# use <- which(treat == 0 & data.analysis[,v2[vv]]>0)
# outlier <- cbind(data.analysis[use, c("municipality_code", "pop.2020")], 
#                  data.analysis[use, v2[vv]],
#                  outcome[use, v1[vv]])
# outlier[6,]
# (-outlier[6,4]/table(treat)[1])/summary.Match.out[15]
# 
# vv <- 7
# colnames(outcome)[v1[vv]]
# use <- which(treat == 0 & data.analysis[,v2[vv]]>0)
# outlier <- cbind(data.analysis[use, c("municipality_code", "pop.2020")], 
#                  data.analysis[use, v2[vv]],
#                  outcome[use, v1[vv]])
# outlier[1,]
# (-outlier[1,4]/table(treat)[1])/summary.Match.out[25]
# 
# # the mean of population among all of the 847 municipalities in the 27 target prefectures is 87,540.
# mean(data.all$pop.2020)
