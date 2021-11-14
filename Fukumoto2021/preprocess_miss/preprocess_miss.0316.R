##
# Matching Preprocess (March 16)
##

data.miss <- data

# Small Size Dataset
# data.miss <- data.miss[data.miss$pref %in% c(3, 6, 7, 9, 11, 12, 13, 15,
#                                                          16, 18, 21, 23, 25, 27, 31,
#                                                          32, 33, 34, 35, 36, 37, 38,
#                                                          41, 42, 44, 45, 46),]

# Remove municipalities with missing data
# data.miss <- data.miss[is.na(data.miss$shutdown.0316) == FALSE,]
# data.miss <- data.miss[is.na(data.miss$shutdown.0304) == FALSE,]

# Remove missing data for tmean_mean (sunshine, precipitation also missing for the same municipality)
#data.miss <- data.miss[is.na(data.miss$tmean_mean) == FALSE,]

# Remove municipalities with prior infections
# data.miss <- data.miss[data.miss$prior.infection.0316 == 0, ]

# Remove 62 municipalities in Tokyo and elsewhere missing COVID data
#data.miss <- data.miss[is.na(data.miss$X2020.3.16) == FALSE,]

# Calculate infections in the last week and prior infection per capita
data.miss$cumulative.week.0316 <- rowSums(data.miss[,96:102])/data.miss$pop.2020
data.miss$prior.infection.0316 <- rowSums(data.miss[,52:102], na.rm=T)
data.miss$prior.infection.per.capita <- data.miss$prior.infection.0316/data.miss$pop.2020
#data.miss <- data.miss[is.na(data.miss$cumulative.week.0316) == FALSE,]

# Exclude Fukushima municipalities that are eligible for the special act for the nuke accident evacuators (including Iwaki City)
#data.miss <- data.miss[data.miss$municipality_code %in% c(7204,7211,7212,7308,7541,7542,7543,7544,7545,7546,7547,7548,7564) == FALSE,]

# Full set of covariates
control <- as.matrix(cbind(data.miss[, c(10:41)]))

# Set electoral time
data.miss$date.0316 <- as.character(data.miss$date.0316)
data.miss$electoral.time <- as.numeric(as.Date("2020-03-16") - as.Date(data.miss$date.0316, "%m/%d/%y"))

### Added Items ###

# Treatment indicator
treat <- data.miss$shutdown.0316

# Jan. 25 - Aug. 10
period <- 52:250 # indicator
YMD <- as.Date("2020-1-25") + 1:length(period) - 1 # year-month-day

# Replace -1's with 0's
for(p in 1:length(period)){
  data.miss[, period[p]] <- as.character(data.miss[, period[p]])
  data.miss[, period[p]] <- gsub("-1", "0", data.miss[, period[p]])
  data.miss[, period[p]] <- as.integer(data.miss[, period[p]]) 
}

# Create Outcome
outcome <- (10^6)*data.miss[, period]/data.miss$pop.2020
# the number of the infected per population (in million)

# Prefecture Dummy
unique.pref <- unique(data.miss$pref)
pref.dummy <- matrix(NA, nrow(data.miss), (length(unique.pref)-1))
for(p in 1:(length(unique.pref)-1)){
  pref.dummy[, p] <- ifelse(data.miss$pref == unique.pref[p], 1, 0)  
}

data.miss$log.number <- log(1 + data.miss$number.of.neighbors)

# Previous Cases
data.miss$cases_1 <- data.miss$X2020.3.15 / data.miss$pop.2020
data.miss$cases_2 <- data.miss$X2020.3.14 / data.miss$pop.2020
data.miss$cases_3 <- data.miss$X2020.3.13 / data.miss$pop.2020
data.miss$cases_4 <- data.miss$X2020.3.12 / data.miss$pop.2020
data.miss$cases_5 <- data.miss$X2020.3.11 / data.miss$pop.2020
data.miss$cases_6 <- data.miss$X2020.3.10 / data.miss$pop.2020
data.miss$cases_7 <- data.miss$X2020.3.9 / data.miss$pop.2020
