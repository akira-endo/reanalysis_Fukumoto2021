###
# Neighbor Functions and Data Summary
###

# row-standarized neighbor matrix
neighbor.matrix <- as.matrix(read.csv("neighbor/neighbor.matrix.csv"))
num <- apply(neighbor.matrix, 1, sum)
neighbor.matrix.std <- neighbor.matrix/num
neighbor.matrix.std[num==0,] <-0 

###########
# functions

# a dummy whether a municipality's neighbors have missing value or not
miss.neighbor <- function(var.date, neighbor.matrix.std){
  miss.date <- is.na(var.date)
  miss.out <- neighbor.matrix.std%*%miss.date
  miss.out <- as.integer(miss.out > 0)
  miss.out
}

# the average of a treatment of a municipality's neighbors
shutdown.neighbor <- function(shutdown.date, neighbor.matrix.std){
  use <- !is.na(shutdown.date)
  shutdown.out <- neighbor.matrix.std[,use]%*%shutdown.date[use]
  num.use <- apply(neighbor.matrix.std[,use], 1, sum)
  shutdown.out[num.use==0] <- NA 
  miss.neighbor.out <- miss.neighbor(shutdown.date, neighbor.matrix.std)
  shutdown.out[miss.neighbor.out==1] <- NA
  shutdown.out
}

# the average of an outcome of a municipality's neighbors, weighted by their population
outcome.neighbor <- function(outcome.date, neighbor.matrix.std, pop){
  use <- !is.na(outcome.date)
  outcome.out <- neighbor.matrix.std[,use]%*%outcome.date[use]
  pop.out <- neighbor.matrix.std[,use]%*%pop[use]
  outcome.out <- outcome.out/pop.out
  num.use <- apply(neighbor.matrix.std[,use], 1, sum) 
  outcome.out[num.use==0] <- NA
  miss.neighbor.out <- miss.neighbor(outcome.date, neighbor.matrix.std)
  outcome.out[miss.neighbor.out==1] <- NA
  outcome.out
}

##########

