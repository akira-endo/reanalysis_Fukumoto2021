##########################
# Genetic Matching 04.10 #
##########################

data <- read.csv("../Fukumoto2021/covid.csv")

##
# 1. Preprocess
##

source("../Fukumoto2021/preprocess/preprocess.0410.R")

##
# 2. Genetic Matching
##

# Matching
m.out <- matchit(treat ~ control + pref.dummy +
                   data.analysis$lon + data.analysis$lat +
                   data.analysis$prec_mean + data.analysis$shine_mean +
                   data.analysis$tmean_mean +
                   data.analysis$electoral.time +
                   data.analysis$age.0410 +
                   data.analysis$win_count.0410 +
                   #data.analysis$cumulative.week.0410 +
                   data.analysis$cases_1 + data.analysis$cases_2 + data.analysis$cases_3 + data.analysis$cases_4 +
                   data.analysis$cases_5 + data.analysis$cases_6 + data.analysis$cases_7 +
                   data.analysis$log.number +
                   data.analysis$shutdown.0304 + data.analysis$shutdown.0316 + data.analysis$shutdown.0406 +
                   data.analysis$prior.infection.per.capita,
                 method = "genetic",
                 distance = "lasso",
                 pop.size = pop.size,
                 nboots = nboots,
                 caliper=0.2,
                 replace = TRUE,
                 verbose = FALSE,
                 estimand = "ATC")

# Save Genetic Matching File
saveRDS(m.out, paste0("../output/rematch_alt_atc_0410_", pop.size, "_", nboots, ".RDS"))
