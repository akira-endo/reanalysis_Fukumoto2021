# Checkpoint
library(checkpoint)
checkpoint("2021-05-25", R.version = "4.0.3")
library(rgenoud)
library(MatchIt)
library(Matching)
library(tidyverse)

pop.size <- 1000 
nboots <- 1000 

# Load neighbor functions
source("neighbor/pq.neighbor.functions.R")

set.seed(4032020) 
source("neighbor/pq.neighbor.0304.R")

set.seed(16032020)
source("neighbor/pq.neighbor.0316.R")

set.seed(6042020)
source("neighbor/pq.neighbor.0406.R")

# set.seed(16042020)
# source("neighbor/pq.neighbor.0416.R")
# 
# set.seed(22042020)
# source("neighbor/pq.neighbor.0422.R")
# 
# set.seed(11052020)
# source("neighbor/pq.neighbor.0511.R")

