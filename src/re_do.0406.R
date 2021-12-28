##########################
# Genetic Matching 04.06 #
##########################

data <- read.csv("../Fukumoto2021/covid.csv")

##
# 1. Preprocess
##

source("../Fukumoto2021/preprocess/preprocess.0406.R")

##
# 2. Genetic Matching
##

# Matching
m.out <- matchit(treat ~ control + pref.dummy +
                   data.analysis$lon + data.analysis$lat +
                   data.analysis$prec_mean + data.analysis$shine_mean +
                   data.analysis$tmean_mean +
                   data.analysis$electoral.time +
                   data.analysis$age.0406 +
                   data.analysis$win_count.0406 +
                   #data.analysis$cumulative.week.0406 +
                   data.analysis$cases_1 + data.analysis$cases_2 + data.analysis$cases_3 + data.analysis$cases_4 +
                   data.analysis$cases_5 + data.analysis$cases_6 + data.analysis$cases_7 +
                   data.analysis$log.number +
                   data.analysis$shutdown.0304 + data.analysis$shutdown.0316 +
                   data.analysis$prior.infection.per.capita,
                 method = "genetic",
                 distance = "lasso",
                 pop.size = pop.size,
                 nboots = nboots,
                 caliper=0.1,
                 replace = TRUE,
                 verbose = FALSE,
                 estimand = "ATC")

# Save Genetic Matching File
saveRDS(m.out, paste0("../output/rematch_alt_atc_0406_", pop.size, "_", nboots, ".RDS"))

##
# 3. Genetic Matching (ATT)
##

# Matching
#m.out2 <- matchit(treat ~ control + pref.dummy +
#                   data.analysis$lon + data.analysis$lat +
 #                  data.analysis$prec_mean + data.analysis$shine_mean +
  #                  data.analysis$tmean_mean +
   #                data.analysis$electoral.time +
    #               data.analysis$age.0406 +
     #               data.analysis$win_count.0406 +
      #             #data.analysis$cumulative.week.0406 +
       #             data.analysis$cases_1 + data.analysis$cases_2 + data.analysis$cases_3 + data.analysis$cases_4 +
        #            data.analysis$cases_5 + data.analysis$cases_6 + data.analysis$cases_7 +
         #          data.analysis$log.number +
          #         data.analysis$shutdown.0304 + data.analysis$shutdown.0316 +
           #        data.analysis$prior.infection.per.capita,
              #   method = "genetic",
            ##     distance = "mahalanobis",
             #    pop.size = pop.size,
              #   nboots = nboots, 
               #  replace = TRUE,
                # verbose = TRUE,
                 #estimand = "ATT")

# Save Genetic Matching File
#saveRDS(m.out2, paste0("matching_results/alt_att_0406_", pop.size, "_", nboots, ".RDS"))


