#Headers
library(testthat)
source("../community1.R")

#Load in data
test_matrix <- read.csv("test_matrix.csv")

#Perform tests
expect_that(b.div.wrap(test_matrix), equals(structure(list(total = 1.16666666666667, species = structure(c(0.257142857142857, 
                                                                                                           0.257142857142857, 0.257142857142857, 0.228571428571429), .Names = c("Species.A", 
                                                                                                                                                                                "Species.B", "Species.C", "Species.D")), sites = c(0.204761904761905, 
                                                                                                                                                                                                                                   0.147619047619048, 0.147619047619048, 0.147619047619048, 0.204761904761905, 
                                                                                                                                                                                                                                   0.147619047619048)), .Names = c("total", "species", "sites"))))
expect_that(b.div.forked(test_matrix), equals(structure(list(total = 1.16666666666667, species = 1.16666666666667, 
                                                             sites = 1.16666666666667), .Names = c("total", "species", 
                                                                                                   "sites"))))
#Bodie - write a test that makes sure the three "sides" of the equation tally up
