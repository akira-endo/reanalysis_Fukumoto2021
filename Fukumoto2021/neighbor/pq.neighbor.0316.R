##
# Conditioning on Neighbors 03.16
##

data <- read.csv("covid.csv")

##
# 1. Preprocess
##

source("neighbor/preprocess/n.preprocess.0316.R")

##
# 2. Genetic Matching
##

# Matching
m.out <- matchit(treat ~ control + pref.dummy +
                   data.analysis$lon + data.analysis$lat +
                   data.analysis$prec_mean + data.analysis$shine_mean +
                   data.analysis$tmean_mean +
                   data.analysis$electoral.time +
                   data.analysis$age.0316 +
                   data.analysis$win_count.0316 +
                   #data.analysis$cumulative.week.0316 +
                   data.analysis$cases_1 + data.analysis$cases_2 + data.analysis$cases_3 + data.analysis$cases_4 +
                   data.analysis$cases_5 + data.analysis$cases_6 + data.analysis$cases_7 +
                   #data.analysis$neighbor.infection +
                   data.analysis$log.number +
                   data.analysis$shutdown.0304 +
                   data.analysis$prior.infection.per.capita,
                 method = "genetic",
                 distance = "mahalanobis",
                 pop.size = pop.size,
                 nboots = nboots,
                 verbose = TRUE,
                 replace = TRUE,
                 estimand = "ATC")

# Save Genetic Matching File
saveRDS(m.out, "neighbor/results/neighbor_atc_0316_1000_1000.RDS")

