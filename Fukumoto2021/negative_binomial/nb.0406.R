###########################
# Negative Binomial 04.06 #
###########################

data <- read.csv("covid.csv")

##
# 1. Preprocess
##

source(p_path)

##
# 2. Use Genetic Matching Dataset
##

# Read in Genetic Matching File
m.out <- readRDS(m_path)

# Create matched dataset
gm <- get_matches(m.out, data = data.analysis)

##
# 3. Estimate the NB
##

# estimate the ATT for every day from one week before (March 30) to three weeks after (April 27)
part <- 120:148 # indicates March 30 to April 27
part.adj <- part - 54

summary.NB.out <- matrix(NA, length(part), 2)
for(p in 1:length(part)){
  fit <- glm.nb(gm[, part[p]] ~ shutdown.0406 + offset(log(pop.2020)),
                data = gm, weights = weights)
  summary.NB.out[p,1] <- coef(summary(fit))[2,1]
  summary.NB.out[p,2] <- coef(summary(fit))[2,2]
}

# the upper bound of the 95% confidence interval
UB <- summary.NB.out[, 1] + (summary.NB.out[, 2] * qnorm(0.975))
# the lower bound of the 95% confidence interval
LB <- summary.NB.out[, 1] + (summary.NB.out[, 2] * qnorm(0.025))
min.LB <- min(LB, na.rm=T)
max.UB <- max(UB, na.rm=T)
