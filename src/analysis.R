################################################################################
# Load libraries
################################################################################
source("textmining.R")

# test units: testthat, RUnit

# Testing different sparse values
sparse.v <- seq(0.1, 0.9, 0.1)
sapply(sparse.v, main)

# I see there might be a potential maximum between 0.4 and 0.5,
# let's explore that interval
sparse.v <- seq(0.4, 0.5, 0.01)
sapply(sparse.v, main)

# I obtain a score of 100% with a sparse value of 0.48
main(0.48)
