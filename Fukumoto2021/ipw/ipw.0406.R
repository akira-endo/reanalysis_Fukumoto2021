#############
# IPW 04.06 #
#############

data <- read.csv("covid.csv")

##
# 1. Preprocess
##

source("preprocess/preprocess.0406.R")

##
# 2. Calculate Weights
##

# # if R version < 4
# # https://yukiyanai.github.io/econometrics2/propensity-score.html
# if (as.integer(version$major) < 4) {
#   deparse1 <- function(x) {
#     paste(deparse(x), collapse = ' ')
#   }
# }
# 
# pref.dummy <- as.data.frame(pref.dummy)
# outcome <- as.data.frame(outcome)
# 
# # compute weights
# # library(WeightIt)
# W.out <- weightit(treat ~ control + pref.dummy +
#                     #data.analysis$lon + data.analysis$lat +
#                     data.analysis$prec_mean + data.analysis$shine_mean +
#                     data.analysis$tmean_mean +
#                     data.analysis$electoral.time +
#                     data.analysis$age.0406 +
#                     data.analysis$win_count.0406 +
#                     #data.analysis$cumulative.week.0406 +
#                     data.analysis$cases_1 + data.analysis$cases_2 + data.analysis$cases_3 + data.analysis$cases_4 +
#                     data.analysis$cases_5 + data.analysis$cases_6 + data.analysis$cases_7 +
#                     data.analysis$log.number +
#                     data.analysis$shutdown.0304 + data.analysis$shutdown.0316 +
#                     data.analysis$prior.infection.per.capita,
#                   # data = data.analysis, # this argument might be mandatory
#                   estimand = "ATC", method = "cbps")
# 
# # Save Weights
# saveRDS(W.out, "ipw/results/w.out_0406.RDS")

##
# 3. IPW Analysis
##

# Load Weights
W.out <- readRDS("ipw/results/w.out_0406.RDS")

# weighted average of outcome for treated municipalities
average.T <- apply(outcome[treat == 1, ], 2, weighted.mean, w = W.out$weights[treat == 1], na.rm=T)
# non-weighted average of outcome for control municipalities
average.C <- apply(outcome[treat == 0, ], 2, mean, na.rm=T)
# non-weighted average of outcome for treated municipalities
average.all.T <- apply(outcome[treat == 1, ], 2, mean, na.rm=T)

# estimate the ATC for every day from one week before (March 30) to three weeks after (April 27)
part <- 117:145 # indicates March 30 to April 27
part.adj <- part - 51 
data.analysis[,part] <- (10^5)*data.analysis[, part]/data.analysis$pop.2020
varlist <- names(data.analysis)[part]

d.w <- svydesign(~1, weights = W.out$weights, data = data.analysis)

# Variables list
models <- lapply(varlist, function(x){
  svyglm(substitute(i ~ shutdown.0406, list(i = as.name(x))), design = d.w)
})

summary.IPW.out <- matrix(NA, length(part), 3)
for(p in 1:length(part)){
  # Coefficient, 2.5, 97.5
  summary.IPW.out[p,1] <- coef(summary(models[[p]]))[2,1]
  summary.IPW.out[p,2] <- confint(models[[p]])[2,1]
  summary.IPW.out[p,3] <- confint(models[[p]])[2,2]
}
min.LB = min(summary.IPW.out[,2])
max.UB = max(summary.IPW.out[,3])


