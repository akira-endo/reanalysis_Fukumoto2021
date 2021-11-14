## This replication file
## creates the results of genetic matching, 
## which can take almost a week

# Checkpoint
library(checkpoint)
checkpoint("2021-05-25", R.version = "4.0.3")

# Other Packages
library(tidyverse)
library(rgenoud)
library(MatchIt)
library(Matching)

pop.size <- 1000 
nboots <- 1000 

set.seed(4032020) # set seed as day-month-year of the survey date
source("do_files/do.0304.R")

set.seed(16032020)
source("do_files/do.0316.R")

set.seed(6042020)
source("do_files/do.0406.R")

set.seed(10042020)
source("do_files/do.0410.R")

# set.seed(16042020)
# source("do_files/do.0416.R")
# 
# set.seed(22042020)
# source("do_files/do.0422.R")
# 
# set.seed(11052020)
# source("do_files/do.0511.R")
