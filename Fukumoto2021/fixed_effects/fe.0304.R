#######################
# Fixed Effects 03.04 #
#######################

data <- read.csv("covid.csv")

##
# 1. Preprocess
##

source("preprocess/preprocess.0304.R")

##
# 2. Fixed Effects
##

# Estimate the ATE for every day from one week before (Feb. 26) to three weeks after (March 25)
part <- 33:61 # indicates Feb.26 to March 25

# # Fill regression table
summary.FE.out <- matrix(NA, length(part), 2)
for(p in 1:length(part)){
  fit <- felm(outcome[, part[p]] ~ treat | health.center.id | 0 | health.center.id,
            data = data.analysis)
  summary.FE.out[p,1] <- coef(summary(fit))[1,1]
  summary.FE.out[p,2] <- coef(summary(fit))[1,2]
}
# saveRDS(summary.FE.out, "fixed_effects/fe_0304.RDS")

# Load regression table
#summary.FE.out <- readRDS("fixed_effects/fe_0304.RDS")

# the upper bound of the 95% confidence interval
UB <- summary.FE.out[, 1] + (summary.FE.out[, 2] * qnorm(0.975))
# the lower bound of the 95% confidence interval
LB <- summary.FE.out[, 1] + (summary.FE.out[, 2] * qnorm(0.025))
min.LB <- min(LB, na.rm=T)
max.UB <- max(UB, na.rm=T)
