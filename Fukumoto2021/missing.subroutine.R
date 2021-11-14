day <- survey.dates[d]
source(paste0("preprocess/preprocess.", day, ".R")) # data.analysis
source(paste0("preprocess_miss/preprocess_miss.", day, ".R")) # data.miss
target <- c(3, 6, 7, 9, 11, 12, 15, 16, 18, 21, 23, 25, 27, 31, 32, 33, 34, 35, 36, 37, 38, 41, 42, 44, 45, 46)
if(d > 3){
  target <- sort(c(target, 13))
}
category <- as.integer(data.miss$municipality_code %in% data.analysis$municipality_code)
category[!(data.miss$pref %in% target)] <- -1
table.category[d, ] <- table(category)
shutdowns <- (43 + 1):(43 + d - 1)
if(d == 1){
  shutdowns <- NULL
}
covariates <- c(
  #time invariant
  10:41, # 32 controls
  42, 43, # lat, lon
  256:258, # weather
  289, # log.number = log of number.of.neighbors
  # time invariant
  267:268, # age, win_count
  287:288, # prior.infection.per.capita, electoral.time
  290:296, # cases_1 through cases_7
  shutdowns) # binary   
data.cov <- data.miss[,covariates]

data.outcome <- outcome[, survey.date.col[d]:(survey.date.col[d + 1] - 1)] / 10
if(d == 7){
  data.outcome <- outcome[, survey.date.col[d]:survey.date.col[d + 1]] / 10
}

################################
# compared with municipalities
# outside the target prefectures

category.pref <- ifelse(category == 1, 1, ifelse(category == -1, 0, NA)) 

# continuous
continuous.covariates <- c(1:6, 9:42)
t.test.pref[[d]] <- array()
for(v in continuous.covariates){
  t.test.pref[[d]][v] <- t.test(na.omit(data.cov[category.pref==1, v]),
                                na.omit(data.cov[category.pref==0, v]))$p.value
}
for(v in 7:8){
  use <- !is.infinite(data.cov[, v])
  t.test.pref[[d]][v] <- t.test(na.omit(data.cov[category.pref==1 & use, v]),
                                na.omit(data.cov[category.pref==0 & use, v]))$p.value
}

# binary
if(d > 1){
  binary.covariates <- c(50:(48 + d))
  fisher.test.pref[[d]] <- array()
  for(v in 1:length(binary.covariates)){
    fisher.test.pref[[d]][v] <- fisher.test(category.pref, data.cov[, binary.covariates[v]])$p.value
  }
}

######################################
# compared with missing municipalities
# inside the target prefectures

category.miss <- ifelse(category == 1, 1, ifelse(category == 0, 0, NA))

# continuous
continuous.covariates <- c(1:6, 9:49)
t.test.miss[[d]] <- array()
for(v in continuous.covariates){
  t.test.miss[[d]][v] <- t.test(na.omit(data.cov[category.miss == 1, v]),
                                na.omit(data.cov[category.miss == 0, v]))$p.value
}
for(v in 7:8){
  use <- !is.infinite(data.cov[, v])
  t.test.miss[[d]][v] <- t.test(na.omit(data.cov[category.miss == 1 & use, v]),
                                na.omit(data.cov[category.miss == 0 & use, v]))$p.value
}

# binary
fisher.test.miss[[d]] <- array()
for(v in 1:(length(target) - 1)){
  fisher.test.miss[[d]][v] <- fisher.test(category.miss, pref.dummy[, target[v]])$p.value
}
if(d > 1){
  binary.covariates <- c(50:(48 + d))
  for(vv in 1:length(binary.covariates)){
    fisher.test.miss[[d]][(vv + length(target) - 1)] <- 
      fisher.test(category.miss, 
                 data.cov[, binary.covariates[vv]])$p.value
  }
}

# outcome
outcome.out[[d]] <- matrix(NA, ncol(data.outcome), 6)
for(v in 1:ncol(data.outcome)){
  outcome.out[[d]][v, 1] <- mean(data.outcome[category.miss == 1, v], na.rm = T)
  outcome.out[[d]][v, 2] <- mean(data.outcome[category.miss == 0, v], na.rm = T)
  lm.out <- lm(data.outcome[,v] ~ category.miss)
  outcome.out[[d]][v, 3:4] <- confint(lm.out)[2,]
  outcome.out[[d]][v, 5:6] <- summary(lm.out)$coef[2, c(1, 4)]
}

