## This replication file
## creates the results of genetic matching with a smaller set of covairates, 
## which can take almost a week

# Checkpoint
library(checkpoint)
checkpoint("2021-05-25", R.version = "4.0.3")

# Other Packages
library(rgenoud)
library(MatchIt)
library(Matching)
library(tidyverse)

data <- read.csv("covid.csv")
survey.dates <- c("0304", "0316", "0406", "0410", "0416", "0422", "0511") 
pop.size <- 1000 
nboots <- 1000 

d <- 1
set.seed(4032020) 
source("do_small.day.R")

d <- 2
set.seed(16032020)
source("do_small.day.R")

d <- 3
set.seed(6042020)
source("do_small.day.R")

d <- 4
set.seed(10042020)
source("do_small.day.R")

# d <- 5
# set.seed(16042020)
# source("do_small.day.R")
# 
# d <- 6
# set.seed(22042020)
# source("do_small.day.R")
# 
# d <- 7
# set.seed(11052020)
# source("do_small.day.R")
