##
# Matching Preprocess (April 6)
##

data.analysis <- data

# Create new variables and filter
data.analysis <- data.analysis %>%
  mutate(cumulative.week.0406 = rowSums(data.analysis[,117:123])/pop.2020,
         prior.infection.0406 = rowSums(data.analysis[,c(52:123, 284)], na.rm=T),
         prior.infection.per.capita = prior.infection.0406/pop.2020,
         date.0406 = as.character(date.0406),
         electoral.time = as.numeric(as.Date("2020-04-06") - as.Date(data.analysis$date.0406, "%m/%d/%y"))) %>%
  filter(pref %in% c(3, 6, 7, 9, 11, 12, 13, 15, # limit prefectures
                       16, 18, 21, 23, 25, 27, 31,
                       32, 33, 34, 35, 36, 37, 38,
                       41, 42, 44, 45, 46) &
           is.na(shutdown.0406) == FALSE & # remove missing data on shutdown status
           is.na(shutdown.0316) == FALSE & 
           is.na(shutdown.0304) == FALSE &
           is.na(tmean_mean) == FALSE & # sunshine, precipitation also missing for same municipality
           is.na(X2020.4.6) == FALSE & # removes missing data on COVID (i.e., Tokyo)
           is.na(cumulative.week.0406) == FALSE & # removes missing data on cumulative week
           municipality_code %in% c(7204,7211,7212,7308,7541,7542,7543,7544,7545,7546,7547,7548,7564) == FALSE) # excludes Fukushima municipalities that are part of the special act

# Full set of covariates
control <- as.matrix(cbind(data.analysis[, c(10:41)]))

# Treatment indicator
treat <- data.analysis$shutdown.0406

# Jan. 25 - Aug. 10
period <- 52:250 # indicator
YMD <- as.Date("2020-1-25") + 1:length(period) - 1 # year-month-day

# Replace -1's with 0's
for(p in 1:length(period)){
  data.analysis[, period[p]] <- as.character(data.analysis[, period[p]])
  data.analysis[, period[p]] <- gsub("-1", "0", data.analysis[, period[p]])
  data.analysis[, period[p]] <- as.integer(data.analysis[, period[p]]) 
}

# Create Outcome
outcome <- (10^5)*data.analysis[, period]/data.analysis$pop.2020
# the number of the infected per population (per 100,000)

# Prefecture Dummy
unique.pref <- unique(data.analysis$pref)
pref.dummy <- matrix(NA, nrow(data.analysis), (length(unique.pref)-1))
for(p in 1:(length(unique.pref)-1)){
  pref.dummy[, p] <- ifelse(data.analysis$pref == unique.pref[p], 1, 0)  
}

# Add neighbor variable and previous cases
data.analysis <- data.analysis %>%
  mutate(log.number = log(1 + data.analysis$number.of.neighbors),
         cases_1 = X2020.4.5 / pop.2020,
         cases_2 = X2020.4.4 / pop.2020,
         cases_3 = X2020.4.3 / pop.2020,
         cases_4 = X2020.4.2 / pop.2020,
         cases_5 = X2020.4.1 / pop.2020,
         cases_6 = X2020.3.31 / pop.2020,
         cases_7 = X2020.3.30 / pop.2020)
