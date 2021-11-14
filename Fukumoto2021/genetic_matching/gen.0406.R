##########################
# Genetic Matching 04.06 #
##########################

data <- read.csv("covid.csv")

##
# 1. Preprocess
##

source(p_path)

##
# 2. Estimate ATC
##

# Read in Genetic Matching File
m.out <- readRDS(m_path)

# Assess balance
#summary(m.out, un = FALSE)

# Create matched dataset
gm <- get_matches(m.out, data = data.analysis)

# estimate the ATC for every day from one week before (March 30) to three weeks after (April 27)
part <- 120:148 # indicates March 30 to April 27
part.adj <- part - 54
gm[,part] <- (10^5)*gm[, part]/gm$pop.2020

summary.Match.out <- matrix(NA, length(part), 2)
for(p in 1:length(part)){
  fit <- lm(lm_form,
            data = gm, weights = weights)
  summary.Match.out[p,1] <- coef(summary(fit))[2,1]
  summary.Match.out[p,2] <- coeftest(fit, vcov. = vcovCL, cluster = ~subclass + id)[2,2]
}

# the upper bound of the 95% confidence interval
UB <- summary.Match.out[, 1] + (summary.Match.out[, 2] * qnorm(0.975))
# the lower bound of the 95% confidence interval
LB <- summary.Match.out[, 1] + (summary.Match.out[, 2] * qnorm(0.025))
min.LB <- min(LB, na.rm=T)
max.UB <- max(UB, na.rm=T)

# average of outcome for matched and treated municipalities
average.T <- apply(gm[gm$shutdown.0406 == 1, part], 2, mean, na.rm = T)
# average of outcome for matched and control municipalities
average.C <- apply(gm[gm$shutdown.0406 == 0, part], 2, mean, na.rm = T)
# average of outcome for treated municipalities (either matched or not)
average.all.T <- apply(outcome[treat == 1, ], 2, mean, na.rm=T)
# average of outcome for control municipalities (either matched or not)
average.all.C <- apply(outcome[treat == 0, ], 2, mean, na.rm=T)

