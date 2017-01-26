library(testthat)
expect_that(b.div.wrap(test_matrix), equals(structure(list(total = 2.6, species = 
                                                             structure(c(0.257142857142857, 0.257142857142857, 0.257142857142857, 
                                                                         0.228571428571429), .Names = c("Species A", "Species B", 
                                                                                                        "Species C", "Species D")), sites = c(0.204761904761905, 
                                                                                                                                              0.147619047619048, 0.147619047619048, 0.147619047619048, 0.204761904761905, 
                                                                                                                                              0.147619047619048)), .Names = c("total", "species", "sites"))))
