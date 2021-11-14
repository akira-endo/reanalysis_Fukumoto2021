##########################
# Genetic Matching       #
##########################

##
# 1. Preprocess
##

day <- survey.dates[d]
source(paste0("preprocess/preprocess.", day, ".R"))

# small set of covariates
control.small <- control[, c(1, 3, 4, 10:12, 15, 16, 18:22, 27, 31, 32)]

##
# 2. Genetic Matching
##

# Matching
m.out <- matchit(treat ~ control.small + pref.dummy +
                   #data.analysis$lon + data.analysis$lat +
                   #data.analysis$prec_mean + data.analysis$shine_mean +
                   #data.analysis$tmean_mean +
                   #data.analysis$electoral.time +
                   #data.analysis$age.0304 +
                   #data.analysis$win_count.0304 +
                   #data.analysis$cumulative.week.0304 +
                   data.analysis$cases_1 + data.analysis$cases_2 + data.analysis$cases_3 + data.analysis$cases_4 +
                   data.analysis$cases_5 + data.analysis$cases_6 + data.analysis$cases_7 +
                   #data.analysis$log.number +
                   data.analysis$prior.infection.per.capita,
                 method = "genetic",
                 distance = "mahalanobis",
                 pop.size = pop.size, 
                 nboots = nboots, 
                 replace = TRUE,
                 verbose = TRUE,
                 estimand = "ATC")

# Save Genetic Matching File
saveRDS(m.out, paste0("sensitivity/small_atc_", day, "_", pop.size, "_", nboots, ".RDS"))

