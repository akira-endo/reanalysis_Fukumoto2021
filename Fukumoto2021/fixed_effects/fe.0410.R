#######################
# Fixed Effects 04.10 #
#######################

data <- read.csv("covid.csv")

##
# 1. Preprocess
##

source("preprocess/preprocess.0410.R")

##
# 2. Fixed Effects
##

# estimate the ATE for every day from one week before (April 3) to three weeks after (May 1)
part <- 70:98 # indicates April 3 to May 1

# # Fill regression table
summary.FE.out <- matrix(NA, length(part), 2)
for(p in 1:length(part)){
  fit <- felm(outcome[, part[p]] ~ treat | health.center.id | 0 | health.center.id,
              data = data.analysis)
  summary.FE.out[p,1] <- coef(summary(fit))[1,1]
  summary.FE.out[p,2] <- coef(summary(fit))[1,2]
}
# saveRDS(summary.FE.out, "fixed_effects/fe_0410.RDS")

# Load regression table
#summary.FE.out <- readRDS("fixed_effects/fe_0410.RDS")

# the upper bound of the 95% confidence interval
UB <- summary.FE.out[, 1] + (summary.FE.out[, 2] * qnorm(0.975))
# the lower bound of the 95% confidence interval
LB <- summary.FE.out[, 1] + (summary.FE.out[, 2] * qnorm(0.025))
min.LB <- min(LB, na.rm=T)
max.UB <- max(UB, na.rm=T)
