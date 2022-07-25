## Context: Maximum Cumulative Ratio approach


#### Maximum Cumulative Ratio approach - main indicators ####

##### hazard_quotient #####

test_that("hazard_quotient returns the right data structure", {
  # Vector -> Vector
  expect_true(is.vector(hazard_quotient(1:5, 1:5)))
  
  # Matrix -> Matrix
  expect_true(is.matrix(hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE), 1:5)))
  expect_true(is.matrix(hazard_quotient(matrix(1:5,  nrow = 1), 1:5)))
})

test_that("hazard_quotient returns a named object if the given one is named", {
  # Vector
  expect_named(hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), 1:5),
               letters[1:5])
  
  # Matrix
  expect_equal(rownames(
    hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE, dimnames = list(c("s1", "s2"))),
                    1:5)),
    c("s1", "s2"))
  expect_equal(colnames(
    hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE, dimnames = list(NULL, letters[1:5])),
                    1:5)),
    letters[1:5])
})

test_that("hazard_quotient returns an unnamed object if the given one is unnamed", {
  # Vector
  expect_named(hazard_quotient(1:5, 1:5), NULL)
  
  # Matrix
  expect_null(colnames(hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE), 1:5)))
  expect_null(rownames(hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE), 1:5)))
})

test_that("hazard_quotient computes the hazard quotients", {
  # Vector
  expect_equal(hazard_quotient(1:5, 1:5),          c(1,1,1,1,1))
  expect_equal(hazard_quotient(1:5, c(1,1,1,1,1)), c(1,2,3,4,5))
  
  # Matrix
  expect_equal(hazard_quotient(matrix(c(1,2,3,4,5,10,10,9,10,10), nrow = 2, byrow = TRUE),
                               c(1,2,3,4,5)),
               matrix(c(1,1,1,1,1, 10,5,3,2.5,2), nrow = 2, byrow = TRUE))
  expect_equal(hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE),
                               c(1,1,1,1,1)),
               matrix(c(1,2,3,4,5, 6,7,8,9,10), nrow = 2, byrow = TRUE))
  
  # Values containing only 0s
  expect_equal(hazard_quotient(values = rep(0,5), references = 1:5),
               c(0,0,0,0,0))
  expect_equal(hazard_quotient(values = matrix(rep(0,10), nrow = 2),
                               references = 1:5),
               matrix(c(0,0,0,0,0, 0,0,0,0,0), nrow = 2, byrow = TRUE))
})


##### hazard_index #####

test_that("hazard_index returns the right number of values", {
  # 'values' as a vector
  expect_length(hazard_index(1:5, 1:5), 1)
  
  # 'values' as a matrix
  expect_length(hazard_index(matrix(1:10, nrow = 2, byrow = TRUE), 1:5), 2)
  expect_length(hazard_index(matrix(1:5,  nrow = 1), 1:5), 1)
  
  # 'hq' as a vector
  expect_length(hazard_index(hq = 1:5), 1)
  
  # 'hq' as a matrix
  expect_length(hazard_index(hq = matrix(1:10, nrow = 2, byrow = TRUE)), 2)
  expect_length(hazard_index(hq = matrix(1:5,  nrow = 1)), 1)
})

test_that("hazard_index returns an unnamed object if a vector is given", {
  # 'values' as a vector
  expect_named(hazard_index(c(a=1, b=2, c=3, d=4, e=5), 1:5), NULL)
  
  # 'hq' as a vector
  expect_named(hazard_index(hq = c(a=1, b=1, c=1, d=1, e=1)), NULL)
})

test_that("hazard_index returns an unnamed object if an unnamed matrix is given", {
  # 'values' as an unnamed matrix
  expect_named(hazard_index(matrix(1:10, nrow = 2, byrow = TRUE), 1:5), NULL)
  
  # 'hq' as an unnamed matrix
  expect_named(hazard_index(hq = matrix(1:10, nrow = 2, byrow = TRUE)), NULL)
})

test_that("hazard_index returns a named object if a named matrix is given", {
  # 'values' as a named matrix
  expect_named(hazard_index(matrix(1:10, nrow = 2, byrow = TRUE, dimnames = list(c("s1", "s2"))), 1:5),
               c("s1", "s2"))
  
  # 'hq' as a named matrix
  expect_named(hazard_index(hq = matrix(1:10, nrow = 2, byrow = TRUE, dimnames = list(c("s1", "s2"))), 1:5),
               c("s1", "s2"))
})

test_that("hazard_index computes the hazard indexes", {
  # 'values' as a vector
  expect_equal(hazard_index(1:5, 1:5),          5)
  expect_equal(hazard_index(1:5, c(1,1,1,1,1)), 15)
  
  # 'values' as a matrix
  expect_equal(hazard_index(matrix(c(1,2,3,4,5, 10,10,9,10,10), nrow = 2, byrow = TRUE), 1:5),
               c(5, 22.5))
  expect_equal(hazard_index(matrix(1:10, nrow = 2, byrow = TRUE), c(1,1,1,1,1)),
               c(15, 40))
  
  # 'hq' as a vector
  expect_equal(hazard_index(hq = c(1,1,1,1,1)), 5)
  expect_equal(hazard_index(hq = 1:5),          15)
  
  # 'hq' as a matrix
  expect_equal(hazard_index(hq = matrix(c(1,2,3,4,5, 10,10,9,10,10),  nrow = 2, byrow = TRUE)),
               c(15, 49))
  expect_equal(hazard_index(hq = matrix(1:10, nrow = 2, byrow = TRUE)),
               c(15, 40))
  
  # Values containing only 0s
  expect_equal(hazard_index(values = rep(0,5), references = 1:5),
               0)
  expect_equal(hazard_index(values = matrix(rep(0,10), nrow = 2),
                            references = 1:5),
               c(0, 0))
})

test_that("hazard_index returns an identical result whatever the chosen usage", {
  values_vector = 1:5
  values_matrix = matrix(1:10, nrow = 2, byrow = TRUE)
  references = 1:5
  
  expect_identical(hazard_index(values_vector, references),
                   hazard_index(hq = hazard_quotient(values_vector, references)))
  expect_identical(hazard_index(values_matrix, references),
                   hazard_index(hq = hazard_quotient(values_matrix, references)))
})


##### maximum_hazard_quotient #####

test_that("maximum_hazard_quotient returns the right number of values", {
  # 'values' as a vector
  expect_length(maximum_hazard_quotient(1:5, 1:5), 1)
  
  # 'values' as a matrix
  expect_length(maximum_hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE), 1:5), 2)
  expect_length(maximum_hazard_quotient(matrix(1:5,  nrow = 1), 1:5), 1)
  
  # 'hq' as a vector
  expect_length(maximum_hazard_quotient(hq = 1:5), 1)
  
  # 'hq' as a matrix
  expect_length(maximum_hazard_quotient(hq = matrix(1:10, nrow = 2, byrow = TRUE)), 2)
  expect_length(maximum_hazard_quotient(hq = matrix(1:5,  nrow = 1)), 1)
  
})

test_that("maximum_hazard_quotient returns an unnamed object if a vector is given", {
  # 'values' as a vector
  expect_named(maximum_hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), 1:5), NULL)
  
  # 'hq' as a vector
  expect_named(maximum_hazard_quotient(hq = c(a=1, b=1, c=1, d=1, e=1)), NULL)
})

test_that("maximum_hazard_quotient returns an unnamed object if an unnamed matrix is given", {
  # 'values' as an unnamed matrix
  expect_named(maximum_hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE), 1:5), NULL)
  
  # 'hq' as an unnamed matrix
  expect_named(maximum_hazard_quotient(hq = matrix(1:10, nrow = 2, byrow = TRUE)), NULL)
})

test_that("maximum_hazard_quotient returns a named object if a named matrix is given", {
  # 'values' as a named matrix
  expect_named(maximum_hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE,
                                              dimnames = list(c("s1", "s2"))),
                                       1:5),
               c("s1", "s2"))
  
  # 'hq' as a named matrix
  expect_named(maximum_hazard_quotient(hq = matrix(1:10, nrow = 2, byrow = TRUE,
                                                   dimnames = list(c("s1", "s2"))),
                                       1:5),
               c("s1", "s2"))
})

test_that("maximum_hazard_quotient find the maximum hazard quotient", {
  # 'values' as a vector
  expect_equal(maximum_hazard_quotient(1:5, 1:5),          1)
  expect_equal(maximum_hazard_quotient(1:5, c(1,1,1,1,1)), 5)
  
  # 'values' as a matrix
  expect_equal(maximum_hazard_quotient(matrix(c(1,2,3,4,5, 10,10,9,10,10), nrow = 2, byrow = TRUE),
                                       1:5),
               c(1, 10))
  expect_equal(maximum_hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE),
                                       c(1,1,1,1,1)),
               c(5, 10))
  
  # 'hq' as a vector
  expect_equal(maximum_hazard_quotient(hq = c(1,1,1,1,1)), 1)
  expect_equal(maximum_hazard_quotient(hq = 1:5),          5)
  
  # 'hq' as a matrix
  expect_equal(maximum_hazard_quotient(hq = matrix(c(1,2,3,4,5, 10,10,9,10,10), nrow = 2, byrow = TRUE)),
               c(5, 10))
  expect_equal(maximum_hazard_quotient(hq = matrix(1:10, nrow = 2, byrow = TRUE)),
               c(5, 10))
  
  # Values containing only 0s
  expect_equal(maximum_hazard_quotient(values = rep(0,5), references = 1:5),
               0)
  expect_equal(maximum_hazard_quotient(values = matrix(rep(0,10), nrow = 2),
                                       references = 1:5),
               c(0, 0))
})

test_that("maximum_hazard_quotient returns an identical result whatever the chosen usage", {
  values_vector = 1:5
  values_matrix = matrix(1:10, nrow = 2, byrow = TRUE)
  references = 1:5
  
  expect_identical(maximum_hazard_quotient(values_vector, references),
                   maximum_hazard_quotient(hq = hazard_quotient(values_vector, references)))
  expect_identical(maximum_hazard_quotient(values_matrix, references),
                   maximum_hazard_quotient(hq = hazard_quotient(values_matrix, references)))
})


##### maximum_cumulative_ratio #####

test_that("maximum_cumulative_ratio returns the right number of values", {
  # 'values' as a vector
  expect_length(maximum_cumulative_ratio(1:5, 1:5), 1)
  
  # 'values' as a matrix
  expect_length(maximum_cumulative_ratio(matrix(1:10, nrow = 2, byrow = TRUE), 1:5), 2)
  expect_length(maximum_cumulative_ratio(matrix(1:5,  nrow = 1), 1:5), 1)
  
  # 'hi' and 'mhq'
  expect_length(maximum_cumulative_ratio(hi = 1,   mhq = 1),   1)
  expect_length(maximum_cumulative_ratio(hi = 1:5, mhq = 1:5), 5)
})

test_that("maximum_cumulative_ratio returns an unnamed object if a vector of values is given", {
  # 'values' as a vector
  expect_named(maximum_cumulative_ratio(c(a=1, b=2, c=3, d=4, e=5), 1:5), NULL)
})

test_that("maximum_cumulative_ratio returns an unnamed object if an unnamed matrix is given", {
  # 'values' as an unnamed matrix
  expect_named(maximum_cumulative_ratio(matrix(1:10, nrow = 2, byrow = TRUE), 1:5), NULL)
})

test_that("maximum_cumulative_ratio returns a named object if a named matrix is given", {
  # 'values' as a named matrix
  expect_named(maximum_cumulative_ratio(matrix(1:10, nrow = 2, byrow = TRUE,
                                               dimnames = list(c("s1", "s2"))),
                                        1:5),
               c("s1", "s2"))
})

test_that("maximum_cumulative_ratio returns an unnamed object if unnamed vectors of hi and mhq are given", {
  # 'hi' and 'mhq' as unnamed vectors
  expect_named(maximum_cumulative_ratio(hi = 1:5, mhq = 1:5), NULL)
})

test_that("maximum_cumulative_ratio returns a named object if named vectors of hi and mhq are given", {
  # 'hi' and/or 'mhq' as named vectors
  expect_named(maximum_cumulative_ratio(hi  = c(s1=1, s2=1, s3=1, s4=1, s5=1),
                                        mhq = c(s1=1, s2=1, s3=1, s4=1, s5=1)),
               c("s1", "s2", "s3", "s4", "s5"))
  expect_named(maximum_cumulative_ratio(hi = c(s1=1, s2=1, s3=1, s4=1, s5=1), mhq = 1:5),
               c("s1", "s2", "s3", "s4", "s5"))
  expect_named(maximum_cumulative_ratio(hi = 1:5, mhq = c(s1=1, s2=1, s3=1, s4=1, s5=1)),
               c("s1", "s2", "s3", "s4", "s5"))
})

test_that("maximum_cumulative_ratio computes the maximum cumulative ratio", {
  # 'values' as a vector
  expect_equal(maximum_cumulative_ratio(1:5, 1:5),          5)
  expect_equal(maximum_cumulative_ratio(1:5, c(1,1,1,1,1)), 3)
  
  # 'values' as a matrix
  expect_equal(maximum_cumulative_ratio(matrix(c(1,2,3,4,5, 10,10,9,10,10), nrow = 2, byrow = TRUE),
                                        1:5),
               c(5, 2.25))
  expect_equal(maximum_cumulative_ratio(matrix(1:10, nrow = 2, byrow = TRUE),
                                        c(1,1,1,1,1)),
               c(3, 4))
  
  # 'hi' and 'mhq'
  expect_equal(maximum_cumulative_ratio(hi = c(1,1,1,1,1), mhq = 1:5), c(1,0.5,1/3,0.25,0.2))
  expect_equal(maximum_cumulative_ratio(hi = 1:5, mhq = c(1,1,1,1,1)), c(1,2,3,4,5))
  
  # Values containing only 0s
  expect_equal(maximum_cumulative_ratio(values = rep(0,5), references = 1:5),
               0)
  expect_equal(maximum_cumulative_ratio(values = matrix(rep(0,10), nrow = 2),
                                        references = 1:5),
               c(0, 0))
})

test_that("maximum_cumulative_ratio returns an identical result whatever the chosen usage", {
  values_vector = 1:5
  values_matrix = matrix(1:10, nrow = 2, byrow = TRUE)
  references = 1:5
  
  expect_identical(maximum_cumulative_ratio(values_vector, references),
                   maximum_cumulative_ratio(hi = hazard_index(values_vector, references),
                                            mhq = maximum_hazard_quotient(values_vector, references)))
  expect_identical(maximum_cumulative_ratio(values_matrix, references),
                   maximum_cumulative_ratio(hi = hazard_index(values_matrix, references),
                                            mhq = maximum_hazard_quotient(values_matrix, references)))
})


##### missed_toxicity #####

test_that("missed_toxicity returns the right number of values", {
  # 'values' as a vector
  expect_length(missed_toxicity(1:5, 1:5), 1)
  
  # 'values' as a matrix
  expect_length(missed_toxicity(matrix(1:10, nrow = 2, byrow = TRUE), 1:5), 2)
  expect_length(missed_toxicity(matrix(1:5,  nrow = 1), 1:5), 1)
  
  # 'mcr'
  expect_length(missed_toxicity(mcr = 1),   1)
  expect_length(missed_toxicity(mcr = 1:5), 5)
})

test_that("missed_toxicity returns an unnamed object if a vector of values is given", {
  # 'values' as a vector
  expect_named(missed_toxicity(c(a=1, b=2, c=3, d=4, e=5), 1:5), NULL)
})

test_that("missed_toxicity returns an unnamed object if an unnamed matrix is given", {
  # 'values' as an unnamed matrix
  expect_named(missed_toxicity(matrix(1:10, nrow = 2, byrow = TRUE), 1:5), NULL)
})

test_that("missed_toxicity returns a named object if a named matrix is given", {
  # 'values' as a named matrix
  expect_named(missed_toxicity(matrix(1:10, nrow = 2, byrow = TRUE,
                                      dimnames = list(c("s1", "s2"))),
                               1:5),
               c("s1", "s2"))
})

test_that("missed_toxicity returns an unnamed object if an unnamed vector of mcr is given", {
  # 'mcr' as an unnamed vector
  expect_named(missed_toxicity(mcr = 1:5), NULL)
})

test_that("missed_toxicity returns a named object if a named vector of mcr is given", {
  # 'mcr' as a named vector
  expect_named(missed_toxicity(mcr = c(s1=1, s2=1, s3=1, s4=1, s5=1)),
               c("s1", "s2", "s3", "s4", "s5"))
})

test_that("missed_toxicity computes the missed toxicity", {
  # 'values' as a vector
  expect_equal(missed_toxicity(1:5, 1:5),          0.8)
  expect_equal(missed_toxicity(1:5, c(1,1,1,1,1)), 2/3)
  
  # 'values' as a matrix
  expect_equal(missed_toxicity(matrix(c(1,2,3,4,5), nrow = 1), 1:5),                0.8)
  expect_equal(missed_toxicity(matrix(1:10, nrow = 2, byrow = TRUE), c(1,1,1,1,1)), c(2/3, 0.75))
  
  # 'mcr'
  expect_equal(missed_toxicity(mcr = 1),   0)
  expect_equal(missed_toxicity(mcr = 1:5), c(0,0.5,2/3,0.75,0.8))
  
  # Values containing only 0s
  expect_equal(missed_toxicity(values = rep(0,5), references = 1:5),
               0)
  expect_equal(missed_toxicity(values = matrix(rep(0,10), nrow = 2),
                               references = 1:5),
               c(0, 0))
})

test_that("missed_toxicity returns an identical result whatever the chosen usage", {
  values_vector = 1:5
  values_matrix = matrix(1:10, nrow = 2, byrow = TRUE)
  references = 1:5
  
  expect_identical(missed_toxicity(values_vector, references),
                   missed_toxicity(mcr = maximum_cumulative_ratio(values_vector, references)))
  expect_identical(missed_toxicity(values_matrix, references),
                   missed_toxicity(mcr = maximum_cumulative_ratio(values_matrix, references)))
})



#### Maximum Cumulative Ratio approach - additional indicators ####

##### reciprocal_of_mcr #####

test_that("reciprocal_of_mcr returns the right number of values", {
  # 'values' as a vector
  expect_length(reciprocal_of_mcr(1:5, 1:5), 1)
  
  # 'values' as a matrix
  expect_length(reciprocal_of_mcr(matrix(1:10, nrow = 2, byrow = TRUE), 1:5), 2)
  expect_length(reciprocal_of_mcr(matrix(1:5,  nrow = 1), 1:5), 1)
  
  # 'hi' and 'mhq'
  expect_length(reciprocal_of_mcr(hi = 1,   mhq = 1),   1)
  expect_length(reciprocal_of_mcr(hi = 1:5, mhq = 1:5), 5)
  
  # 'mcr'
  expect_length(reciprocal_of_mcr(mcr = 1),   1)
  expect_length(reciprocal_of_mcr(mcr = 1:5), 5)
})

test_that("reciprocal_of_mcr returns an unnamed object if a vector of values is given", {
  # 'values' as a vector
  expect_named(reciprocal_of_mcr(c(a=1, b=2, c=3, d=4, e=5), 1:5), NULL)
})

test_that("reciprocal_of_mcr returns an unnamed object if a unnamed matrix is given", {
  # 'values' as an unnamed matrix
  expect_named(reciprocal_of_mcr(matrix(1:10, nrow = 2, byrow = TRUE), 1:5), NULL)
})

test_that("reciprocal_of_mcr returns a named object if a named matrix is given", {
  # 'values' as a named matrix
  expect_named(reciprocal_of_mcr(matrix(1:10, nrow = 2, byrow = TRUE,
                                        dimnames = list(c("s1", "s2"))),
                                 1:5),
               c("s1", "s2"))
})

test_that("reciprocal_of_mcr returns an unnamed object if unnamed vectors of hi and mhq are given", {
  # 'hi' and 'mhq' as unnamed vectors
  expect_named(reciprocal_of_mcr(hi = 1:5, mhq = 1:5), NULL)
})

test_that("reciprocal_of_mcr returns a named object if named vectors of hi and mhq are given", {
  # 'hi' and/or 'mhq' as named vectors
  expect_named(reciprocal_of_mcr(hi  = c(s1=1, s2=1, s3=1, s4=1, s5=1),
                                 mhq = c(s1=1, s2=1, s3=1, s4=1, s5=1)),
               c("s1", "s2", "s3", "s4", "s5"))
  expect_named(reciprocal_of_mcr(hi = c(s1=1, s2=1, s3=1, s4=1, s5=1), mhq = 1:5),
               c("s1", "s2", "s3", "s4", "s5"))
  expect_named(reciprocal_of_mcr(hi = 1:5, mhq = c(s1=1, s2=1, s3=1, s4=1, s5=1)),
               c("s1", "s2", "s3", "s4", "s5"))
})

test_that("reciprocal_of_mcr returns an unnamed object if an unnamed vector of mcr is given", {
  # 'mcr' as an unnamed vector
  expect_named(reciprocal_of_mcr(mcr = 1:5), NULL)
})

test_that("reciprocal_of_mcr returns a named object if a named vector of mcr is given", {
  # 'mcr' as a named vector
  expect_named(reciprocal_of_mcr(mcr = c(s1=1, s2=1, s3=1, s4=1, s5=1)),
               c("s1", "s2", "s3", "s4", "s5"))
})

test_that("reciprocal_of_mcr computes the reciprocal of the maximum cumulative ratio", {
  # 'values' as a vector
  expect_equal(reciprocal_of_mcr(1:5, 1:5),          0.2)
  expect_equal(reciprocal_of_mcr(1:5, c(1,1,1,1,1)), 1/3)
  
  # 'values' as a matrix
  expect_equal(reciprocal_of_mcr(matrix(c(1,2,3,4,5), nrow = 1), 1:5),                0.2)
  expect_equal(reciprocal_of_mcr(matrix(1:10, nrow = 2, byrow = TRUE), c(1,1,1,1,1)), c(1/3, 0.25))
  
  # 'hi' and 'mhq'
  expect_equal(reciprocal_of_mcr(hi = c(1,1,1,1,1), mhq = 1:5), c(1,2,3,4,5))
  expect_equal(reciprocal_of_mcr(hi = 1:5, mhq = c(1,1,1,1,1)), c(1,0.5,1/3,0.25,0.2))
  
  # 'mcr'
  expect_equal(reciprocal_of_mcr(mcr = 1),   1)
  expect_equal(reciprocal_of_mcr(mcr = 1:5), c(1,0.5,1/3,0.25,0.2))
  
  # Values containing only 0s
  expect_equal(reciprocal_of_mcr(values = rep(0,5), references = 1:5),
               0)
  expect_equal(reciprocal_of_mcr(values = matrix(rep(0,10), nrow = 2),
                                 references = 1:5),
               c(0, 0))
})

test_that("reciprocal_of_mcr returns an identical result whatever the chosen usage", {
  values_vector = 1:5
  values_matrix = matrix(1:10, nrow = 2, byrow = TRUE)
  references = 1:5
  
  expect_identical(reciprocal_of_mcr(values_vector, references),
                   reciprocal_of_mcr(hi = hazard_index(values_vector, references),
                                     mhq = maximum_hazard_quotient(values_vector, references)))
  expect_identical(reciprocal_of_mcr(values_vector, references),
                   reciprocal_of_mcr(mcr = maximum_cumulative_ratio(values_vector, references)))
  
  expect_identical(reciprocal_of_mcr(values_matrix, references),
                   reciprocal_of_mcr(hi = hazard_index(values_matrix, references),
                                     mhq = maximum_hazard_quotient(values_matrix, references)))
  expect_identical(reciprocal_of_mcr(values_matrix, references),
                   reciprocal_of_mcr(mcr = maximum_cumulative_ratio(values_matrix, references)))
})


##### top_hazard_quotient #####

test_that("top_hazard_quotient returns at most as many top hazard quotients as hazard quotients considered", {
  # 'values' as a vector
  expect_length(top_hazard_quotient(1:5, 1:5, k = 500), 5)
  expect_length(top_hazard_quotient(rep(0,5), 1:5, k = 500, ignore_zero = FALSE), 5)
  expect_length(top_hazard_quotient(c(0,0,0,1,1), 1:5, k = 500, ignore_zero = TRUE), 2)
  
  # 'values' as a matrix
  expect_equal(
    lengths(top_hazard_quotient(matrix(c(1:5, rep(0,5)), nrow = 2, byrow = TRUE),
                                references = 1:5, k = 500, ignore_zero = FALSE)),
    c(5,5)
  )
  expect_equal(
    lengths(top_hazard_quotient(matrix(c(1:5, c(0,0,0,1,1)), nrow = 2, byrow = TRUE),
                                references = 1:5, k = 500, ignore_zero = TRUE)),
    c(5,2)
  )
  expect_equal(lengths(top_hazard_quotient(matrix(1:5, nrow = 1), 1:5, k = 500)), 5)
  
  # 'hq' as a vector
  expect_length(top_hazard_quotient(hq = 1:5, k = 500), 5)
  expect_length(top_hazard_quotient(hq = rep(0,5), k = 500, ignore_zero = FALSE), 5)
  expect_length(top_hazard_quotient(hq = c(0,0,0,1,1), k = 500, ignore_zero = TRUE), 2)
  
  # 'hq' as a matrix
  expect_equal(
    lengths(top_hazard_quotient(hq = matrix(c(1:5, rep(0,5)), nrow = 2, byrow = TRUE),
                                k = 500, ignore_zero = FALSE)),
    c(5,5)
  )
  expect_equal(
    lengths(top_hazard_quotient(hq = matrix(c(1:5, c(0,0,0,1,1)), nrow = 2, byrow = TRUE),
                                k = 500, ignore_zero = TRUE)),
    c(5,2)
  )
  expect_equal(lengths(top_hazard_quotient(hq = matrix(1:5, nrow = 1), k = 500)), 5)
})

test_that("top_hazard_quotient returns, by default if all values are equal to 0, a single value", {
  # 'values' as a vector
  expect_length(top_hazard_quotient(rep(0,5), 1:5, ignore_zero = TRUE), 1)
  expect_length(top_hazard_quotient(rep(0,5), 1:5, ignore_zero = FALSE), 1)
  
  # 'values' as a matrix
  expect_equal(lengths(top_hazard_quotient(matrix(rep(0,10), nrow = 2, byrow = TRUE),
                                           references = 1:5, ignore_zero = TRUE)),
               c(1,1))
  expect_equal(lengths(top_hazard_quotient(matrix(rep(0,10), nrow = 2, byrow = TRUE),
                                           references = 1:5, ignore_zero = FALSE)),
               c(1,1))
  expect_equal(lengths(top_hazard_quotient(matrix(rep(0,5), nrow = 1),
                                           references = 1:5, ignore_zero = TRUE)),
               1)
  expect_equal(lengths(top_hazard_quotient(matrix(rep(0,5), nrow = 1),
                                           references = 1:5, ignore_zero = FALSE)),
               1)
  
  # 'hq' as a vector
  expect_length(top_hazard_quotient(hq = rep(0,5), ignore_zero = TRUE), 1)
  expect_length(top_hazard_quotient(hq = rep(0,5), ignore_zero = FALSE), 1)
  
  # 'hq' as a matrix
  expect_equal(lengths(top_hazard_quotient(hq = matrix(rep(0,10), nrow = 2, byrow = TRUE),
                                           ignore_zero = TRUE)),
               c(1,1))
  expect_equal(lengths(top_hazard_quotient(hq = matrix(rep(0,10), nrow = 2, byrow = TRUE),
                                           ignore_zero = FALSE)),
               c(1,1))
  expect_equal(lengths(top_hazard_quotient(hq = matrix(rep(0,5), nrow = 1),
                                           ignore_zero = TRUE)),
               1)
  expect_equal(lengths(top_hazard_quotient(hq = matrix(rep(0,5), nrow = 1),
                                           ignore_zero = FALSE)),
               1)
})

test_that("top_hazard_quotient returns, by default, as many top hazard quotients as the integer part of the maximum cumulative ratio", {
  # 'values' as a vector
  expect_length(top_hazard_quotient(1:5, 1:5), 5)
  
  # 'values' as a matrix
  expect_equal(lengths(top_hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE), 1:5)), c(5,2))
  expect_equal(lengths(top_hazard_quotient(matrix(1:5,  nrow = 1), 1:5)), 5)
  
  # 'hq' as a vector
  expect_length(top_hazard_quotient(hq = 1:5), 3)
  
  # 'hq' as a matrix
  expect_equal(lengths(top_hazard_quotient(hq = matrix(1:10, nrow = 2, byrow = TRUE))), c(3,4))
  expect_equal(lengths(top_hazard_quotient(hq = matrix(1:5,  nrow = 1))), 3)
})

test_that("top_hazard_quotient only allows to find at least 1 top hazard quotient", {
  expect_error(top_hazard_quotient(1:5, 1:5,                                  k = 0), "greater")
  expect_error(top_hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE), 1:5, k = -1), "greater")
  expect_error(top_hazard_quotient(hq = 1:5,                                  k = -50), "greater")
  expect_error(top_hazard_quotient(hq = matrix(1:10, nrow = 2, byrow = TRUE), k = -Inf), "greater")
  
  expect_error(top_hazard_quotient(1:5, 1:5,                                  k = 1), NA)
  expect_error(top_hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE), 1:5, k = 10), NA)
  expect_error(top_hazard_quotient(hq = 1:5,                                  k = 100), NA)
  expect_error(top_hazard_quotient(hq = matrix(1:10, nrow = 2, byrow = TRUE), k = Inf), NA)
})

test_that("top_hazard_quotient returns as many top hazard quotients as requested", {
  # 'values' as a vector
  expect_length(top_hazard_quotient(1:5, 1:5, k = 3), 3)
  expect_length(top_hazard_quotient(rep(0,5), 1:5, k = 3, ignore_zero = FALSE), 3)
  
  # 'values' as a matrix
  expect_equal(lengths(top_hazard_quotient(matrix(c(1:5, rep(0,5)), nrow = 2, byrow = TRUE),
                                           references = 1:5, k = 3, ignore_zero = FALSE)),
               c(3,3))
  expect_equal(lengths(top_hazard_quotient(matrix(1:5, nrow = 1), 1:5, k = 3)),
               3)
  
  # 'hq' as a vector
  expect_length(top_hazard_quotient(hq = 1:5, k = 3), 3)
  expect_length(top_hazard_quotient(hq = rep(0,5), k = 3, ignore_zero = FALSE), 3)
  
  # 'hq' as a matrix
  expect_equal(lengths(top_hazard_quotient(hq = matrix(c(1:5, rep(0,5)), nrow = 2, byrow = TRUE),
                                           k = 3, ignore_zero = FALSE)),
               c(3,3))
  expect_equal(lengths(top_hazard_quotient(hq = matrix(1:5, nrow = 1))),
               3)
})

test_that("top_hazard_quotient returns the right data structure", {
  # 'values' as a vector
  expect_true(is.vector(top_hazard_quotient(1:5, 1:5)))
  expect_false(is.list(top_hazard_quotient(1:5, 1:5)))
  
  # 'values' as a matrix
  expect_true(is.list(top_hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE), 1:5)))
  expect_true(is.list(top_hazard_quotient(matrix(1:5,  nrow = 1), 1:5)))
  
  # 'hq' as a vector
  expect_true(is.vector(top_hazard_quotient(hq = 1:5)))
  expect_false(is.list(top_hazard_quotient(hq = 1:5)))
  
  # 'hq' as a matrix
  expect_true(is.list(top_hazard_quotient(hq = matrix(1:10, nrow = 2, byrow = TRUE))))
  expect_true(is.list(top_hazard_quotient(hq = matrix(1:5,  nrow = 1))))
})

test_that("top_hazard_quotient returns as many sets of top hazard quotients as there are sets of values or of hq", {
  # 'values' as a matrix
  expect_length(top_hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE), 1:5), 2)
  expect_length(top_hazard_quotient(matrix(1:5,  nrow = 1), 1:5), 1)
  
  # 'hq' as a matrix
  expect_length(top_hazard_quotient(hq = matrix(1:10, nrow = 2, byrow = TRUE)), 2)
  expect_length(top_hazard_quotient(hq = matrix(1:5,  nrow = 1)), 1)
})

test_that("top_hazard_quotient returns named values if the given values or hq are named", {
  # 'values' as a vector
  expect_named(top_hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), 1:5))
  expect_named(top_hazard_quotient(c(a=0, b=0, c=0, d=0, e=0), 1:5))
  
  # 'values' as a matrix
  expect_named(unlist(top_hazard_quotient(matrix(c(1:10, rep(0,5)), nrow = 3, byrow = TRUE,
                                                 dimnames = list(NULL, letters[1:5])),
                                          1:5)))
  expect_named(unlist(top_hazard_quotient(matrix(1:5,  nrow = 1,
                                                 dimnames = list(NULL, letters[1:5])),
                                          1:5)))
  
  # 'hq' as a vector
  expect_named(top_hazard_quotient(hq = c(a=1, b=2, c=3, d=4, e=5)))
  expect_named(top_hazard_quotient(hq = c(a=0, b=0, c=0, d=0, e=0)))
  
  # 'hq' as a matrix
  expect_named(unlist(top_hazard_quotient(hq = matrix(c(1:10, rep(0,5)), nrow = 3, byrow = TRUE,
                                                      dimnames = list(NULL, letters[1:5])))))
  expect_named(unlist(top_hazard_quotient(hq = matrix(1:5,  nrow = 1,
                                                      dimnames = list(NULL, letters[1:5])))))
})

test_that("top_hazard_quotient returns named sets of values if the given values or hq matrix is named", {
  # 'values' as a matrix
  expect_named(top_hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE,
                                          dimnames = list(c("s1", "s2"))),
                                   1:5),
               c("s1", "s2"))
  expect_named(top_hazard_quotient(matrix(1:5,  nrow = 1, dimnames = list("s1")), 1:5),
               "s1")
  
  # 'hq' as a matrix
  expect_named(top_hazard_quotient(hq = matrix(1:10, nrow = 2, byrow = TRUE,
                                               dimnames = list(c("s1", "s2")))),
               c("s1", "s2"))
  expect_named(top_hazard_quotient(hq = matrix(1:5,  nrow = 1, dimnames = list("s1"))),
               "s1")
})

test_that("top_hazard_quotient returns decreasing values", {
  is_desc_sorted = function(x) !is.unsorted(rev(x))
  are_all_desc_sorted = function(x) all(sapply(x, is_desc_sorted))
  
  # 'values' as a vector
  expect_true(is_desc_sorted(top_hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), 1:5)))
  
  # 'values' as a matrix
  expect_true(are_all_desc_sorted(top_hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE,
                                                             dimnames = list(NULL, letters[1:5])),
                                                      1:5)))
  expect_true(are_all_desc_sorted(top_hazard_quotient(matrix(1:5, nrow = 1,
                                                             dimnames = list(NULL, letters[1:5])),
                                                      1:5)))
  
  # 'hq' as a vector
  expect_true(is_desc_sorted(top_hazard_quotient(hq = c(a=1, b=2, c=3, d=4, e=5))))
  
  # 'hq' as a matrix
  expect_true(are_all_desc_sorted(top_hazard_quotient(hq = matrix(1:10, nrow = 2, byrow = TRUE,
                                                                  dimnames = list(NULL, letters[1:5])))))
  expect_true(are_all_desc_sorted(top_hazard_quotient(hq = matrix(1:5, nrow = 1,
                                                                  dimnames = list(NULL, letters[1:5])))))
})

test_that("top_hazard_quotient finds the top hazard quotients", {
  # 'values' as a vector
  expect_equal(top_hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), 1:5),
               c(a=1, b=1, c=1, d=1, e=1))
  expect_equal(top_hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), c(1,1,1,1,1)),
               c(e=5, d=4, c=3))
  
  # 'values' as a matrix
  expect_equal(top_hazard_quotient(matrix(1:10, nrow = 2, byrow = TRUE,
                                          dimnames = list(c("s1", "s2"), letters[1:5])),
                                   c(1,1,1,1,1)),
               list(s1 = c(e=5, d=4, c=3),
                    s2 = c(e=10, d=9, c=8, b=7)))
  expect_equal(top_hazard_quotient(matrix(1:5, nrow = 1, dimnames = list("s1", letters[1:5])), 1:5),
               list(s1 = c(a=1, b=1, c=1, d=1, e=1)))
  
  # 'hq' as a vector
  expect_equal(top_hazard_quotient(hq = c(a=1, b=1, c=1, d=1, e=1)),
               c(a=1, b=1, c=1, d=1, e=1))
  expect_equal(top_hazard_quotient(hq = c(a=1, b=2, c=3, d=4, e=5)),
               c(e=5, d=4, c=3))
  
  # 'hq' as a matrix
  expect_equal(top_hazard_quotient(hq = matrix(1:10, nrow = 2, byrow = TRUE,
                                               dimnames = list(c("s1", "s2"), letters[1:5]))),
               list(s1 = c(e=5, d=4, c=3),
                    s2 = c(e=10, d=9, c=8, b=7)))
  expect_equal(top_hazard_quotient(hq = matrix(1:5, nrow = 1, dimnames = list("s1", letters[1:5]))),
               list(s1 = c(e=5, d=4, c=3)))
})

test_that("top_hazard_quotient returns an identical result whatever the chosen usage", {
  values_vector = c(a=1, b=2, c=3, d=4, e=5)
  values_matrix = matrix(1:10, nrow = 2, byrow = TRUE, dimnames = list(c("s1", "s2"), letters[1:5]))
  references = 1:5
  
  expect_identical(top_hazard_quotient(values_vector, references),
                   top_hazard_quotient(hq = hazard_quotient(values_vector, references)))
  expect_identical(top_hazard_quotient(values_matrix, references),
                   top_hazard_quotient(hq = hazard_quotient(values_matrix, references)))
})


##### classify_mixture #####

test_that("classify_mixture returns the right number of values", {
  # 'values' as a vector
  expect_length(classify_mixture(1:5, 1:5), 1)
  
  # 'values' as a matrix
  expect_length(classify_mixture(matrix(1:10, nrow = 2, byrow = TRUE), 1:5), 2)
  expect_length(classify_mixture(matrix(1:5,  nrow = 1), 1:5), 1)
  
  # 'hi' and 'mhq'
  expect_length(classify_mixture(hi = 1,   mhq = 1),   1)
  expect_length(classify_mixture(hi = 1:5, mhq = 1:5), 5)
  
  # 'hi', 'mhq' and 'mcr'
  expect_length(classify_mixture(hi = 1,   mhq = 1,   mcr = 1),            1)
  expect_length(classify_mixture(hi = 1:5, mhq = 1:5, mcr = c(1,1,1,1,1)), 5)
})

test_that("classify_mixture returns an unnamed object if a vector of values is given", {
  # 'values' as a vector
  expect_named(classify_mixture(c(a=1, b=2, c=3, d=4, e=5), 1:5), NULL)
})

test_that("classify_mixture returns an unnamed object if a unnamed matrix is given", {
  # 'values' as an unnamed matrix
  expect_named(classify_mixture(matrix(1:10, nrow = 2, byrow = TRUE), 1:5), NULL)
})

test_that("classify_mixture returns a named object if a named matrix is given", {
  # 'values' as a named matrix
  expect_named(classify_mixture(matrix(1:10, nrow = 2, byrow = TRUE,
                                       dimnames = list(c("s1", "s2"))),
                                1:5),
               c("s1", "s2"))
})

test_that("classify_mixture returns an unnamed object if unnamed vectors of hi, mhq and mcr are given", {
  # 'hi' and 'mhq' as unnamed vectors
  expect_named(classify_mixture(hi = 1:5, mhq = 1:5), NULL)
  
  # 'hi', 'mhq' and 'mcr' as unnamed vectors
  expect_named(classify_mixture(hi = 1:5, mhq = 1:5, mcr = c(1,1,1,1,1)), NULL)
})

test_that("classify_mixture returns an named object if named vectors of hi, mhq and mcr are given", {
  # 'hi' and/or 'mhq' as named vectors
  expect_named(classify_mixture(hi  = c(s1=1, s2=1, s3=1, s4=1, s5=1),
                                mhq = c(s1=1, s2=1, s3=1, s4=1, s5=1)),
               c("s1", "s2", "s3", "s4", "s5"))
  expect_named(classify_mixture(hi = c(s1=1, s2=1, s3=1, s4=1, s5=1), mhq = 1:5),
               c("s1", "s2", "s3", "s4", "s5"))
  expect_named(classify_mixture(hi = 1:5, mhq = c(s1=1, s2=1, s3=1, s4=1, s5=1)),
               c("s1", "s2", "s3", "s4", "s5"))
  
  # 'hi' and/or 'mhq' and/or 'mcr' as named vectors
  expect_named(classify_mixture(hi  = c(s1=1, s2=1, s3=1, s4=1, s5=1),
                                mhq = c(s1=1, s2=1, s3=1, s4=1, s5=1),
                                mcr = c(s1=1, s2=1, s3=1, s4=1, s5=1)),
               c("s1", "s2", "s3", "s4", "s5"))
  expect_named(classify_mixture(hi = c(s1=1, s2=1, s3=1, s4=1, s5=1), mhq = 1:5, mcr = c(1,0.5,1/3,0.25,0.2)),
               c("s1", "s2", "s3", "s4", "s5"))
  expect_named(classify_mixture(hi = 1:5, mhq = c(s1=1, s2=1, s3=1, s4=1, s5=1), mcr = 1:5),
               c("s1", "s2", "s3", "s4", "s5"))
  expect_named(classify_mixture(hi = 1:5, mhq = 1:5, mcr = c(s1=1, s2=1, s3=1, s4=1, s5=1)),
               c("s1", "s2", "s3", "s4", "s5"))
})

test_that("classify_mixture classifies into the MIAT groups", {
  # Expected conditions for groups
  # mhq >= 1; mhq < 1 and hi <= 1; mhq < 1, hi > 1 and mcr < 2; mhq < 1, hi > 1 and mcr >= 2
  # Therefore, limit values must be applied in the following order: mhq, hi, mcr
  
  # Tests on the limit values (in order of application)
  expect_equal(classify_mixture(mhq = c(2, 1, 0),       hi = c(2, 1, 0),     mcr = c(1, 1, NaN)),
               c("I","I","II"))
  expect_equal(classify_mixture(mhq = c(0.8, 0.8, 0.8), hi = c(0, 1, 1.5),   mcr = c(0, 1.250, 1.875)),
               c("II","II","IIIA"))
  expect_equal(classify_mixture(mhq = c(0.8, 0.8, 0.8), hi = c(1.5, 1.6, 2), mcr = c(1.875, 2, 2.5)),
               c("IIIA","IIIB","IIIB"))
  expect_equal(classify_mixture(matrix(c(1,   1,
                                         0.5, 0.5,
                                         0.5, 0.8,
                                         0.9, 0.9), ncol = 2, byrow = TRUE), c(1,1)),
               c("I","II","IIIA", "IIIB"))
  
  # Test the order of application of these limits
  expect_equal(classify_mixture(mhq = c(1, 1, 1), hi = c(2, 2, 2), mcr = c(3, 2, 1)),
               c("I","I","I"))
  expect_equal(classify_mixture(mhq = c(1, 1, 1), hi = c(2, 1, 0), mcr = c(3, 3, 3)),
               c("I","I","I"))
  expect_equal(classify_mixture(mhq = c(0, 0, 0), hi = c(0, 1, 2), mcr = c(3, 3, 3)),
               c("II","II","IIIB"))
  expect_equal(classify_mixture(mhq = c(1, 0, 0, 0), hi = c(1, 1, 2, 2), mcr = c(1, 1, 1, 2)),
               c("I","II","IIIA", "IIIB"))
  
  # Values containing only 0s
  expect_equal(classify_mixture(values = rep(0,5), references = 1:5),
               "II")
  expect_equal(classify_mixture(values = matrix(rep(0,10), nrow = 2),
                                references = 1:5),
               c("II", "II"))
})

test_that("classify_mixture returns an identical result whatever the chosen usage", {
  values_matrix = matrix(c(1,   1,
                           0.5, 0.5,
                           0.5, 0.8,
                           0.9, 0.9), ncol = 2, byrow = TRUE)
  references = c(1,1)
  
  # 'values' as a vector
  expect_identical(classify_mixture(values_matrix[1, ], references),
                   classify_mixture(hi  = hazard_index(values_matrix[1, ], references),
                                    mhq = maximum_hazard_quotient(values_matrix[1, ], references)))
  expect_identical(classify_mixture(values_matrix[1, ], references),
                   classify_mixture(hi  = hazard_index(values_matrix[1, ], references),
                                    mhq = maximum_hazard_quotient(values_matrix[1, ], references),
                                    mcr = maximum_cumulative_ratio(values_matrix[1, ], references)))
  
  expect_identical(classify_mixture(values_matrix[2, ], references),
                   classify_mixture(hi  = hazard_index(values_matrix[2, ], references),
                                    mhq = maximum_hazard_quotient(values_matrix[2, ], references)))
  expect_identical(classify_mixture(values_matrix[2, ], references),
                   classify_mixture(hi  = hazard_index(values_matrix[2, ], references),
                                    mhq = maximum_hazard_quotient(values_matrix[2, ], references),
                                    mcr = maximum_cumulative_ratio(values_matrix[2, ], references)))
  
  expect_identical(classify_mixture(values_matrix[3, ], references),
                   classify_mixture(hi  = hazard_index(values_matrix[3, ], references),
                                    mhq = maximum_hazard_quotient(values_matrix[3, ], references)))
  expect_identical(classify_mixture(values_matrix[3, ], references),
                   classify_mixture(hi  = hazard_index(values_matrix[3, ], references),
                                    mhq = maximum_hazard_quotient(values_matrix[3, ], references),
                                    mcr = maximum_cumulative_ratio(values_matrix[3, ], references)))
  
  expect_identical(classify_mixture(values_matrix[4, ], references),
                   classify_mixture(hi  = hazard_index(values_matrix[4, ], references),
                                    mhq = maximum_hazard_quotient(values_matrix[4, ], references)))
  expect_identical(classify_mixture(values_matrix[4, ], references),
                   classify_mixture(hi  = hazard_index(values_matrix[4, ], references),
                                    mhq = maximum_hazard_quotient(values_matrix[4, ], references),
                                    mcr = maximum_cumulative_ratio(values_matrix[4, ], references)))
  
  # 'values' as a matrix
  expect_identical(classify_mixture(values_matrix, references),
                   classify_mixture(hi  = hazard_index(values_matrix, references),
                                    mhq = maximum_hazard_quotient(values_matrix, references)))
  expect_identical(classify_mixture(values_matrix, references),
                   classify_mixture(hi  = hazard_index(values_matrix, references),
                                    mhq = maximum_hazard_quotient(values_matrix, references),
                                    mcr = maximum_cumulative_ratio(values_matrix, references)))
})



#### Maximum Cumulative Ratio approach - summary functions ####

##### mcr_summary #####

test_that("mcr_summary requires that references have the same sizes as values if they are two lists", {
  expect_error(mcr_summary(list(s1 = c(1, 2), s2 = c(2), s3 = c(3, 4)),
                           list(1, 2, 3)),
               "length")
  expect_error(mcr_summary(list(s1 = c(1, 2), s2 = c(2), s3 = c(3, 4)),
                           list(c(1, 2), 1, c(2, 3))),
               NA)
})

test_that("mcr_summary requires values and references to have named values if they are a list and a vector", {
  expect_error(mcr_summary(list(s1 = c(1, 2), s2 = c(2), s3 = c(3, 4)),
                           c(a = 1, b = 2, c = 3)),
               "name")
  expect_error(mcr_summary(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                           c(1, 2, 3)),
               "name")
  expect_error(mcr_summary(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                           c(a = 1, b = 2, c = 3)),
               NA)
})

test_that("mcr_summary returns the right data structure", {
  # Vector -> List
  expect_true(is.list(mcr_summary(c(a=1, b=2, c=3, d=4, e=5), 1:5)))
  
  # Matrix (several or a single set of values) -> Data frame
  expect_true(is.data.frame(mcr_summary(matrix(1:15, nrow = 3, byrow = TRUE,
                                               dimnames = list(c("s1", "s2", "s3"), letters[1:5])),
                                        1:5)))
  expect_true(is.data.frame(mcr_summary(matrix(1:5, nrow = 1,
                                               dimnames = list("s1", letters[1:5])),
                                        1:5)))
  
  # List (several or a single set of values) -> Data frame
  expect_true(is.data.frame(mcr_summary(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                                        c(a = 1, b = 2, c = 3))))
  expect_true(is.data.frame(mcr_summary(list(s1 = c(a=1, b=2, c=3)),
                                        c(a = 1, b = 2, c = 3))))
})

test_that("mcr_summary returns a named object", {
  col_names = c("n", "n_zero", "HI", "MCR", "Reciprocal", "Group", "THQ", "MHQ", "Missed")
  
  # 'values' as a vector
  expect_named(mcr_summary(c(a=1, b=2, c=3, d=4, e=5), 1:5, ignore_zero = TRUE), col_names)
  expect_named(mcr_summary(c(a=1, b=2, c=3, d=4, e=5), 1:5, ignore_zero = FALSE), col_names[-2])
  
  # 'values' as a matrix
  expect_equal(colnames(mcr_summary(matrix(1:15, nrow = 3, byrow = TRUE,
                                           dimnames = list(c("s1", "s2", "s3"), letters[1:5])),
                                    references = 1:5,
                                    ignore_zero = TRUE)),
               col_names)
  expect_equal(colnames(mcr_summary(matrix(1:15, nrow = 3, byrow = TRUE,
                                           dimnames = list(c("s1", "s2", "s3"), letters[1:5])),
                                    references = 1:5,
                                    ignore_zero = FALSE)),
               col_names[-2])
  expect_equal(colnames(mcr_summary(matrix(1:5, nrow = 1, dimnames = list("s1", letters[1:5])),
                                    references = 1:5,
                                    ignore_zero = TRUE)),
               col_names)
  expect_equal(colnames(mcr_summary(matrix(1:5, nrow = 1, dimnames = list("s1", letters[1:5])),
                                    references = 1:5,
                                    ignore_zero = FALSE)),
               col_names[-2])
  
  # 'values' as a list
  expect_equal(colnames(mcr_summary(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                                    references = c(a = 1, b = 2, c = 3),
                                    ignore_zero = TRUE)),
               col_names)
  expect_equal(colnames(mcr_summary(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                                    references = c(a = 1, b = 2, c = 3),
                                    ignore_zero = FALSE)),
               col_names[-2])
  expect_equal(colnames(mcr_summary(list(s1 = c(a=1, b=2, c=3)),
                                    references = c(a = 1, b = 2, c = 3),
                                    ignore_zero = TRUE)),
               col_names)
  expect_equal(colnames(mcr_summary(list(s1 = c(a=1, b=2, c=3)),
                                    references = c(a = 1, b = 2, c = 3),
                                    ignore_zero = FALSE)),
               col_names[-2])
})

test_that("mcr_summary returns an object whose rows are named if the given sets of values are named", {
  # 'values' as a matrix
  expect_equal(rownames(mcr_summary(matrix(1:15, nrow = 3, byrow = TRUE,
                                           dimnames = list(c("s1", "s2", "s3"), letters[1:5])), 1:5)),
               c("s1", "s2", "s3"))
  expect_equal(rownames(mcr_summary(matrix(1:5, nrow = 1, dimnames = list("s1", letters[1:5])), 1:5)),
               "s1")
  
  # 'values' as a list
  expect_equal(rownames(mcr_summary(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                                    c(a = 1, b = 2, c = 3))),
               c("s1", "s2", "s3"))
  expect_equal(rownames(mcr_summary(list(s1 = c(a=1, b=2, c=3)), c(a = 1, b = 2, c = 3))),
               "s1")
})

test_that("mcr_summary returns NA THQ if the given values are unnamed", {
  all_na = function(x) all(is.na(x))
  
  # 'values' as a vector
  expect_true(is.na(mcr_summary(c(1, 2, 3, 4, 5), 1:5)$THQ))
  expect_false(is.na(mcr_summary(c(a=1, b=2, c=3, d=4, e=5), 1:5)$THQ))

  # 'values' as a matrix
  expect_true(all_na(mcr_summary(matrix(1:15, nrow = 3, byrow = TRUE,
                                        dimnames = list(c("s1", "s2", "s3"))),
                                 1:5)$THQ))
  expect_false(all_na(mcr_summary(matrix(1:15, nrow = 3, byrow = TRUE,
                                         dimnames = list(c("s1", "s2", "s3"), letters[1:5])),
                                  1:5)$THQ))
  expect_true(all_na(mcr_summary(matrix(1:5, nrow = 1, dimnames = list("s1")), 1:5)$THQ))
  expect_false(all_na(mcr_summary(matrix(1:5, nrow = 1, dimnames = list("s1", letters[1:5])), 1:5)$THQ))

  # 'values' as a list
  expect_true(all_na(mcr_summary(list(s1 = c(1, 2), s2 = c(2), s3 = c(3, 4)),
                                 list(c(1, 2), 1, c(2, 3)))$THQ))
  expect_false(all_na(mcr_summary(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                                  list(c(1, 2), 1, c(2, 3)))$THQ))
  expect_true(all_na(mcr_summary(list(s1 = c(1, 2, 3)), list(c(1, 2, 3)))$THQ))
  expect_false(all_na(mcr_summary(list(s1 = c(a=1, b=2, c=3)), list(c(1, 2, 3)))$THQ))
})

test_that("mcr_summary returns NA THQ if the given values are all equal to 0, regardless of ignore_zero", {
  all_na = function(x) all(is.na(x))
  
  # 'values' as a vector
  expect_true(is.na(mcr_summary(c(a=0, b=0, c=0, d=0, e=0), 1:5, ignore_zero = TRUE)$THQ))
  expect_true(is.na(mcr_summary(c(a=0, b=0, c=0, d=0, e=0), 1:5, ignore_zero = FALSE)$THQ))

  # 'values' as a matrix
  expect_equal(mcr_summary(values = matrix(c(1,2,0,4,10, 0,0,0,0,0), nrow = 2, byrow = TRUE,
                                           dimnames = list(c("s1", "s2"), letters[1:5])),
                           references = 1:5,
                           ignore_zero = TRUE)$THQ,
               c("e", NA_character_))
  expect_equal(mcr_summary(values = matrix(c(1,2,0,4,10, 0,0,0,0,0), nrow = 2, byrow = TRUE,
                                           dimnames = list(c("s1", "s2"), letters[1:5])),
                           references = 1:5,
                           ignore_zero = FALSE)$THQ,
               c("e", NA_character_))

  # 'values' as a list
  expect_equal(mcr_summary(values = list(s1 = c(a=1, b=2, c=6), s2 = c(a=0, c=0)),
                           references = c(a = 1, b = 2, c = 3),
                           ignore_zero = TRUE)$THQ,
               c("c", NA_character_))
  expect_equal(mcr_summary(values = list(s1 = c(a=1, b=2, c=6), s2 = c(a=0, c=0)),
                           references = c(a = 1, b = 2, c = 3),
                           ignore_zero = FALSE)$THQ,
               c("c", NA_character_))
})

test_that("mcr_summary differentiates values equal to 0 if ignore_zero is TRUE", {
  values_vector = c(a=1, b=2, c=0, d=4, e=5)
  values_matrix = matrix(c(1,2,0,4,5, 0,4,0,8,10, 3,6,9,12,15), nrow = 3, byrow = TRUE,
                         dimnames = list(c("s1", "s2", "s3"), letters[1:5]))
  values_list = list(s1 = c(a=1, b=2), s2 = c(a=2, c=0), s3 = c(a=0, b=4, c=6))
  
  # 'values' as a vector
  expect_equal(mcr_summary(values_vector, 1:5, ignore_zero = FALSE)$n, 5)
  expect_null(mcr_summary(values_vector, 1:5, ignore_zero = FALSE)$n_zero)
  expect_equal(mcr_summary(values_vector, 1:5, ignore_zero = TRUE)$n, 4)
  expect_equal(mcr_summary(values_vector, 1:5, ignore_zero = TRUE)$n_zero, 1)
  
  # 'values' as a matrix
  expect_equal(mcr_summary(values_matrix, references = 1:5, ignore_zero = FALSE)$n,
               c(5, 5, 5))
  expect_null(mcr_summary(values_matrix, references = 1:5, ignore_zero = FALSE)$n_zero)
  expect_equal(mcr_summary(values_matrix, references = 1:5, ignore_zero = TRUE)$n,
               c(4, 3, 5))
  expect_equal(mcr_summary(values_matrix, references = 1:5, ignore_zero = TRUE)$n_zero,
               c(1, 2, 0))
  
  # 'values' as a list
  expect_equal(mcr_summary(values_list, references = c(a=1, b=2, c=3), ignore_zero = FALSE)$n,
               c(2, 2, 3))
  expect_null(mcr_summary(values_list, references = c(a=1, b=2, c=3), ignore_zero = FALSE)$n_zero)
  expect_equal(mcr_summary(values_list, references = c(a=1, b=2, c=3), ignore_zero = TRUE)$n,
               c(2, 1, 2))
  expect_equal(mcr_summary(values_list, references = c(a=1, b=2, c=3), ignore_zero = TRUE)$n_zero,
               c(0, 1, 1))
})

test_that("mcr_summary computes the indicators of the MCR approach", {
  # 'values' as a vector
  expect_equal(mcr_summary(c(a=1, b=2, c=3, d=4, e=5), 1:5, ignore_zero = FALSE),
               list(n = 5, HI = 5, MCR = 5, Reciprocal = 0.2,
                    Group = "I", THQ = "a", MHQ = 1, Missed = 0.8))
  
  # 'values' as a matrix
  expect_equal(mcr_summary(matrix(c(1,2,3,4,5, 2,4,6,8,10, 3,6,9,12,15), nrow = 3, byrow = TRUE,
                                  dimnames = list(NULL, letters[1:5])),
                           references = 1:5,
                           ignore_zero = FALSE),
               data.frame(n = c(5, 5, 5),
                          HI = c(5, 10, 15),
                          MCR = c(5, 5, 5),
                          Reciprocal = c(0.2, 0.2, 0.2),
                          Group = c("I", "I", "I"),
                          THQ = c("a", "a", "a"),
                          MHQ = c(1, 2, 3),
                          Missed = c(0.8, 0.8, 0.8)))
  expect_equal(mcr_summary(matrix(1:5, nrow = 1, dimnames = list(NULL, letters[1:5])),
                           references = 1:5,
                           ignore_zero = FALSE),
               data.frame(n = 5, HI = 5, MCR = 5, Reciprocal = 0.2,
                          Group = "I", THQ = "a", MHQ = 1, Missed = 0.8))

  # 'values' as a list
  expect_equal(mcr_summary(list(c(a=1, b=2), c(a=2), c(b=4, c=6)),
                           references = c(a = 1, b = 2, c = 3),
                           ignore_zero = FALSE),
               data.frame(n = c(2, 1, 2),
                          HI = c(2, 2, 4),
                          MCR = c(2, 1, 2),
                          Reciprocal = c(0.5, 1, 0.5),
                          Group = c("I", "I", "I"),
                          THQ = c("a", "a", "b"),
                          MHQ = c(1, 2, 2),
                          Missed = c(0.5, 0, 0.5)))
  expect_equal(mcr_summary(list(c(a=1, b=2, c=3)),
                           references = c(a = 1, b = 2, c = 3),
                           ignore_zero = FALSE),
               data.frame(n = 3, HI = 3, MCR = 3, Reciprocal = 1/3,
                          Group = "I", THQ = "a", MHQ = 1, Missed = 2/3))
})

test_that("mcr_summary returns an identical result whatever the structure of references", {
  # 'values' as a list: 'references' as a vector or a list
  values = list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4))
  references_vector = c(a=1, b=2, c=3)
  references_list = list(c(1,2), 1, c(2,3))

  expect_identical(mcr_summary(values, references_vector),
                   mcr_summary(values, references_list))
})


##### mcr_chart #####

test_that("mcr_chart requires that references have the same sizes as values if they are two lists", {
  pdf(NULL)
  values_list = list(s1 = c(a = 1, b = 1),     s2 = c(a = 0.5, c = 0.5),
                     s3 = c(b = 0.5, c = 0.8), s4 = c(a = 0.9, c = 0.9))
  
  expect_error(mcr_chart(values_list, list(1, 2, 3, 4)),
               "length")
  expect_error(mcr_chart(values_list, list(c(1, 2), c(1, 3), c(2, 3), c(1,3))),
               NA)
  dev.off()
})

test_that("mcr_chart requires references to have named values if they are a vector and values are a list", {
  pdf(NULL)
  values_list = list(s1 = c(a = 1, b = 1),     s2 = c(a = 0.5, c = 0.5),
                     s3 = c(b = 0.5, c = 0.8), s4 = c(a = 0.9, c = 0.9))
  
  expect_error(mcr_chart(values_list, c(1, 1, 1)),
               "name")
  expect_error(mcr_chart(values_list, c(a = 1, b = 1, c = 1)),
               NA)
  dev.off()
})

test_that("mcr_chart requires values to be named", {
  pdf(NULL)
  references_1 = c(a = 1, b = 1)
  references_2 = c(a = 1, b = 1, c = 1)
  hi = c(2, 1, 1.3, 1.8)
  mcr = c(2, 2, 1.625, 2)
  
  # 'values' as a matrix
  expect_error(mcr_chart(values = matrix(c(1,1, 0.5,0.5, 0.5,0.8, 0.9,0.9), ncol = 2, byrow = TRUE),
                         references = references_1),
               "name")
  expect_error(mcr_chart(values = matrix(c(1,1, 0.5,0.5, 0.5,0.8, 0.9,0.9), ncol = 2, byrow = TRUE,
                                         dimnames = list(NULL, letters[1:2])),
                         references = references_1),
               NA)
  
  # 'values' as a list
  expect_error(mcr_chart(values = list(c(1, 1), c(0.5, 0.5), c(0.5, 0.8), c(0.9, 0.9)),
                         references = references_2),
               "name")
  expect_error(mcr_chart(values = list(c(a = 1, b = 1),     c(a = 0.5, c = 0.5),
                                       c(b = 0.5, c = 0.8), c(a = 0.9, c = 0.9)),
                         references = references_2),
               NA)
  
  # 'thq' as a vector
  expect_error(mcr_chart(hi = hi, mcr = mcr, thq = c(1, 0.5, 0.8, 0.9)),
               "name")
  expect_error(mcr_chart(hi = hi, mcr = mcr, thq = c(a = 1, a = 0.5, c = 0.8, a = 0.9)),
               NA)
  
  # 'thq' as a list
  expect_error(mcr_chart(hi = hi, mcr = mcr,
                         thq = list(c(1, 1), c(0.5, 0.5), c(0.8), c(0.9, 0.9))),
               "name")
  expect_error(mcr_chart(hi = hi, mcr = mcr,
                         thq = list(c(a=1, b=1), c(a=0.5, c=0.5), c(c=0.8), c(a=0.9, c=0.9))),
               NA)
  dev.off()
})

test_that("mcr_chart generates a warning if some points cannot be plotted because of logarithm", {
  pdf(NULL)
  hi = c(1,1,2,2)
  thq = c(a = 1, b = 1, c = 1, a = 1)
  
  expect_warning(mcr_chart(hi = hi, mcr = c(1, 1, 2, 2), thq = thq),
                 "not been plotted")
  expect_warning(mcr_chart(hi = hi, mcr = c(1.5, 1.5, 2, 2), thq = thq),
                 NA)
  
  dev.off()
})

test_that("mcr_chart returns NULL with a warning if no points can be plotted because of logarithm", {
  pdf(NULL)
  hi = c(1,1,2,2)
  thq = c(a = 1, b = 1, c = 2, a = 2)
  
  expect_null(suppressWarnings(
    mcr_chart(hi = hi, mcr = c(1, 1, 1, 1), thq = thq)
  ))
  expect_false(is.null(mcr_chart(hi = hi, mcr = c(1.5, 1.5, 2, 2), thq = thq)))
  
  expect_warning(mcr_chart(hi = hi, mcr = c(1, 1, 1, 1), thq = thq),
                 "No points can be plotted")
  expect_warning(mcr_chart(hi = hi, mcr = c(1.5, 1.5, 2, 2), thq = thq),
                 NA)
  dev.off()
})

test_that("mcr_chart returns NULL with a warning if there is no points to plot", {
  pdf(NULL)
  values_matrix = matrix(rep(0,10), nrow = 2, dimnames = list(NULL, letters[1:5]))
  references = 1:5
  
  expect_null(suppressWarnings(
    mcr_chart(values_matrix, references)
  ))
  expect_warning(mcr_chart(values_matrix, references),
                 "There is no points to plot")
  dev.off()
})

test_that("mcr_chart returns a ggplot object", {
  pdf(NULL)
  expect_s3_class(mcr_chart(values = list(s1 = c(a = 1, b = 1),     s2 = c(a = 0.5, c = 0.5),
                                          s3 = c(b = 0.5, c = 0.8), s4 = c(a = 0.9, c = 0.9)),
                            references = c(a=1, b=1, c=1)),
                  "ggplot")
  dev.off()
})


##### thq_pairs #####

test_that("thq_pairs requires that references have the same sizes as values if they are two lists", {
  expect_error(thq_pairs(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                         list(1, 2, 3)),
               "length")
  expect_error(thq_pairs(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                         list(c(1, 2), 1, c(2, 3))),
               NA)
})

test_that("thq_pairs requires references to have named values if they are a vector and values are a list", {
  expect_error(thq_pairs(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                         c(1, 2, 3)),
               "name")
  expect_error(thq_pairs(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                         c(a = 1, b = 2, c = 3)),
               NA)
})

test_that("thq_pairs requires values to be named", {
  # 'values' as a matrix
  expect_error(thq_pairs(values = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE),
                         references = c(a = 1, b = 1)),
               "name")
  expect_error(thq_pairs(values = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE,
                                         dimnames = list(NULL, letters[1:2])),
                         references = c(a = 1, b = 1)),
               NA)
  
  # 'values' as a list
  expect_error(thq_pairs(values = list(c(1, 1),
                                       c(1, 1),
                                       c(1, 1)),
                         references = c(a = 1, b = 1)),
               "name")
  expect_error(thq_pairs(values = list(c(a = 1, a = 1),
                                       c(b = 1, b = 1),
                                       c(a = 1, b = 1)),
                         references = c(a = 1, b = 1)),
               NA)
  
  # 'hq' as a matrix
  expect_error(thq_pairs(hq = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE),
                         hi = c(1, 2, 1)),
               "name")
  expect_error(thq_pairs(hq = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE,
                                     dimnames = list(NULL, letters[1:2])),
                         hi = c(1, 2, 1)),
               NA)
  
  # 'hq' as a list
  expect_error(thq_pairs(hq = list(c(1, 1),
                                   c(1, 1),
                                   c(1, 1)),
                         hi = c(2, 2, 2)),
               "name")
  expect_error(thq_pairs(hq = list(c(a = 1, a = 1),
                                   c(b = 1, b = 1),
                                   c(a = 1, b = 1)),
                         hi = c(2, 2, 2)),
               NA)
})

test_that("thq_pairs returns a symmetric matrix", {
  # 'values' as a matrix
  expect_true(isSymmetric.matrix(
    thq_pairs(values = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE,
                              dimnames = list(NULL, letters[1:2])),
              references = c(a = 1, b = 1))
  ))
  
  # 'values' as a list
  expect_true(isSymmetric.matrix(
    thq_pairs(values = list(c(a = 1),
                            c(b = 1, b = 1),
                            c(a = 1, b = 1)),
              references = c(a = 1, b = 1))
  ))
})

test_that("thq_pairs returns NULL in 3 cases", {
  # Three cases:
  # 1. alone is FALSE and no set of values has more than one element considered
  #    a. ignore_zero is FALSE and no set of values has more than one element
  #    b. ignore_zero is TRUE and no set of values has more than one element different from 0
  # 2. threshold is TRUE and no related hazard index is greater than 1
  # 3. threshold is TRUE, alone is FALSE and no set of values meets the two conditions
  #    a. ignore_zero is FALSE and no set of values has more than on element or a HI > 1
  #    b. ignore_zero is TRUE and no set of values has more than on element different from 0 or a HI > 1
  
  # 'values' as a matrix
  # Case 1.a
  expect_null(thq_pairs(values = matrix(c(0, 1, 2), nrow = 3, byrow = TRUE,
                                        dimnames = list(NULL, letters[1])),
                        references = c(a = 1),
                        threshold = FALSE, alone = FALSE, ignore_zero = FALSE))
  expect_false(is.null(
    thq_pairs(values = matrix(c(0,0, 0,0, 0,0), nrow = 3, byrow = TRUE,
                              dimnames = list(NULL, letters[1:2])),
              references = c(a = 1, b = 1),
              threshold = FALSE, alone = FALSE, ignore_zero = FALSE)
  ))
  # Case 1.b
  expect_null(thq_pairs(values = matrix(c(1,0, 1,0, 0,1), nrow = 3, byrow = TRUE,
                                        dimnames = list(NULL, letters[1:2])),
                        references = c(a = 1, b = 1),
                        threshold = FALSE, alone = FALSE, ignore_zero = TRUE))
  expect_false(is.null(
    thq_pairs(values = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE,
                              dimnames = list(NULL, letters[1:2])),
              references = c(a = 1, b = 1),
              threshold = FALSE, alone = FALSE, ignore_zero = TRUE)
  ))
  
  # Case 2
  expect_null(thq_pairs(values = matrix(c(1,0, 1,0, 0,1), nrow = 3, byrow = TRUE,
                                        dimnames = list(NULL, letters[1:2])),
                        references = c(a = 1, b = 1),
                        threshold = TRUE, alone = TRUE))
  expect_false(is.null(
    thq_pairs(values = matrix(c(2,0, 1,1, 0,2), nrow = 3, byrow = TRUE,
                              dimnames = list(NULL, letters[1:2])),
              references = c(a = 1, b = 1),
              threshold = TRUE, alone = TRUE)
  ))
  
  # Case 3.a.
  # If values is a matrix, these cases are equivalent to the union of the cases 1.a and 2
  
  # Case 3.b
  expect_null(thq_pairs(values = matrix(c(2,0, 0.5,0.5, 0,2), nrow = 3, byrow = TRUE,
                                        dimnames = list(NULL, letters[1:2])),
                        references = c(a = 1, b = 1),
                        threshold = TRUE, alone = FALSE, ignore_zero = TRUE))
  expect_false(is.null(
    thq_pairs(values = matrix(c(2,0, 1,1, 0,2), nrow = 3, byrow = TRUE,
                              dimnames = list(NULL, letters[1:2])),
              references = c(a = 1, b = 1),
              threshold = TRUE, alone = FALSE, ignore_zero = TRUE)
  ))
  
  
  # 'values' as a list
  # Case 1.a
  expect_null(thq_pairs(values = list(c(a = 0),
                                      c(b = 0),
                                      c(c = 0)),
                        references = c(a = 1, b = 1, c = 1),
                        threshold = FALSE, alone = FALSE, ignore_zero = FALSE))
  expect_false(is.null(
    thq_pairs(values = list(c(a = 0, b = 0),
                            c(b = 0, c = 0),
                            c(c = 0)),
              references = c(a = 1, b = 1, c = 1),
              threshold = FALSE, alone = FALSE, ignore_zero = FALSE)
  ))
  # Case 1.b
  expect_null(thq_pairs(values = list(c(a = 1, b = 0),
                                      c(b = 1, c = 0),
                                      c(c = 1)),
                        references = c(a = 1, b = 1, c = 1),
                        threshold = FALSE, alone = FALSE, ignore_zero = TRUE))
  expect_false(is.null(
    thq_pairs(values = list(c(a = 1, b = 1),
                            c(b = 1, c = 1),
                            c(c = 1)),
              references = c(a = 1, b = 1, c = 1),
              threshold = FALSE, alone = FALSE, ignore_zero = TRUE)
  ))
  
  # Case 2
  expect_null(thq_pairs(values = list(c(a = 1, b = 0),
                                      c(b = 1, c = 0),
                                      c(c = 1)),
                        references = c(a = 1, b = 1, c = 1),
                        threshold = TRUE, alone = TRUE))
  expect_false(is.null(
    thq_pairs(values = list(c(a = 1, b = 1),
                            c(b = 1, c = 1),
                            c(c = 1)),
              references = c(a = 1, b = 1, c = 1),
              threshold = TRUE, alone = TRUE)
  ))
  
  # Case 3.a
  expect_null(thq_pairs(values = list(c(a = 2),
                                      c(b = 0.5, c = 0.5),
                                      c(c = 1)),
                        references = c(a = 1, b = 1, c = 1),
                        threshold = TRUE, alone = FALSE, ignore_zero = FALSE))
  expect_false(is.null(
    thq_pairs(values = list(c(a = 2, b = 0),
                            c(b = 0.5, c = 0.5),
                            c(c = 1)),
              references = c(a = 1, b = 1, c = 1),
              threshold = TRUE, alone = FALSE, ignore_zero = FALSE)
  ))
  # Case 3.b
  expect_null(thq_pairs(values = list(c(a = 2, b = 0),
                                      c(b = 0.5, c = 0.5),
                                      c(c = 2)),
                        references = c(a = 1, b = 1, c = 1),
                        threshold = TRUE, alone = FALSE, ignore_zero = TRUE))
  expect_false(is.null(
    thq_pairs(values = list(c(a = 2),
                            c(b = 1, c = 1),
                            c(c = 1)),
              references = c(a = 1, b = 1, c = 1),
              threshold = TRUE, alone = FALSE, ignore_zero = TRUE)
  ))
})

test_that("thq_pairs ignores values equal to 0 if ignore_zero is TRUE", {
  # 'values' as a matrix
  expect_equal(
    thq_pairs(values = matrix(c(1,0,1, 0,1,0, 0,0,1), nrow = 3, byrow = TRUE,
                              dimnames = list(NULL, letters[1:3])),
              references = c(a = 1, b = 1, c = 1),
              threshold = FALSE, alone = TRUE, ignore_zero = TRUE),
    as.table(matrix(c(0,0,1,0, 0,0,0,1, 1,0,0,1, 0,1,1,0), nrow = 4, byrow = TRUE,
                    dimnames = list(c(letters[1:3], "NULL"), c(letters[1:3], "NULL"))))
  )
  expect_equal(
    thq_pairs(values = matrix(c(1,0,1, 0,1,0, 0,0,1), nrow = 3, byrow = TRUE,
                              dimnames = list(NULL, letters[1:3])),
              references = c(a = 1, b = 1, c = 1),
              threshold = FALSE, alone = TRUE, ignore_zero = FALSE),
    as.table(matrix(c(0,1,2,0, 1,0,0,0, 2,0,0,0, 0,0,0,0), nrow = 4, byrow = TRUE,
                    dimnames = list(c(letters[1:3], "NULL"), c(letters[1:3], "NULL"))))
  )
  
  # 'values' as a list
  expect_equal(
    thq_pairs(values = list(c(a = 1, c = 1),
                            c(b = 1),
                            c(c = 1, d = 0)),
              references = c(a = 1, b = 1, c = 1, d = 1),
              threshold = FALSE, alone = TRUE, ignore_zero = TRUE),
    as.table(matrix(c(0,0,1,0, 0,0,0,1, 1,0,0,1, 0,1,1,0), nrow = 4, byrow = TRUE,
                    dimnames = list(c(letters[1:3], "NULL"), c(letters[1:3], "NULL"))))
  )
  expect_equal(
    thq_pairs(values = list(c(a = 1, c = 1),
                            c(b = 1),
                            c(c = 1, d = 0)),
              references = c(a = 1, b = 1, c = 1, d = 1),
              threshold = FALSE, alone = TRUE, ignore_zero = FALSE),
    as.table(matrix(c(0,0,1,0,0, 0,0,0,0,1, 1,0,0,1,0, 0,0,1,0,0, 0,1,0,0,0), nrow = 5, byrow = TRUE,
                    dimnames = list(c(letters[1:4], "NULL"), c(letters[1:4], "NULL"))))
  )
})

test_that("thq_pairs output has one additional ending row and column if alone is TRUE", {
  # Two specific cases:
  # 1. alone is TRUE and one value name is alphabetically after NULL, there are elements that are alone
  # 2. alone is TRUE and one value name is alphabetically after NULL, but no element is alone
  
  # 'values' as a matrix
  # Case 1
  expect_equal(
    unique(nth_values(dimnames(
      thq_pairs(values = matrix(c(1,0,0, 1,0,1, 0,1,0), nrow = 3, byrow = TRUE,
                                dimnames = list(NULL, c("a","b","z"))),
                references = c(a = 1, b = 1, z = 1),
                threshold = FALSE, alone = TRUE)
    ), n = "last")),
    "NULL")
  
  # Case 2
  expect_equal(
    unique(nth_values(dimnames(
      thq_pairs(values = matrix(c(1,1,0, 1,0,1, 0,1,1), nrow = 3, byrow = TRUE,
                                dimnames = list(NULL, c("a","b","z"))),
                references = c(a = 1, b = 1, z = 1),
                threshold = FALSE, alone = TRUE)
      ), n = "last")),
    "NULL")
  
  # 'values' as a list
  # Case 1
  expect_equal(
    unique(nth_values(dimnames(
      thq_pairs(values = list(c(a = 1, b = 1),
                              c(b = 1, z = 1),
                              c(c = 1)),
                references = c(a = 1, b = 1, c = 1, z = 1),
                threshold = FALSE, alone = TRUE)
      ), n = "last")),
    "NULL")
  
  # Case 2
  expect_equal(
    unique(nth_values(dimnames(
      thq_pairs(values = list(c(a = 1, b = 1),
                              c(b = 1, z = 1),
                              c(c = 1)),
                references = c(a = 1, b = 1, c = 1, z = 1),
                threshold = FALSE, alone = TRUE)
    ), n = "last")),
    "NULL")
})

test_that("thq_pairs ignores or considers value names according to the argument levels", {
  values_matrix = matrix(c(1,0,1, 1,0,0, 0,0,1), nrow = 3, byrow = TRUE,
                         dimnames = list(NULL, letters[1:3]))
  values_list = list(c(a = 1, b = 0.5, c = 1),
                     c(a = 2),
                     c(c = 2))
  references = c(a = 1, b = 1, c = 1)
  
  # 'values' as a matrix
  expect_equal(
    rownames(thq_pairs(values = values_matrix,
                       references = references,
                       levels = NULL)),
    c("a", "c")
  )
  expect_equal(
    rownames(thq_pairs(values = values_matrix,
                       references = references,
                       levels = c("a", "b", "c"))),
    c("a", "b", "c")
  )
  
  # 'values' as a list
  expect_equal(
    rownames(thq_pairs(values = values_list,
                       references = references,
                       levels = NULL)),
    c("a", "c")
  )
  expect_equal(
    rownames(thq_pairs(values = values_list,
                       references = references,
                       levels = c("a", "b", "c"))),
    c("a", "b", "c")
  )
})

test_that("thq_pairs counts the top hazard quotients pairs", {
  
  # Argument levels
  values_matrix = matrix(c(1,0,1, 1,0,0, 0,0,1), nrow = 3, byrow = TRUE,
                         dimnames = list(NULL, letters[1:3]))
  values_list = list(c(a = 1, b = 0.5, c = 1),
                     c(a = 2),
                     c(c = 2))
  references = c(a = 1, b = 1, c = 1)
  
  expect_equal(
    thq_pairs(values = values_matrix,
              references = references,
              levels = NULL),
    as.table(matrix(c(0,1, 1,0), nrow = 2, byrow = TRUE,
                    dimnames = list(c("a","c"), c("a","c"))))
  )
  expect_equal(
    thq_pairs(values = values_matrix,
              references = references,
              levels = c("a", "b", "c")),
    as.table(matrix(c(0,0,1, 0,0,0, 1,0,0), nrow = 3, byrow = TRUE,
                    dimnames = list(letters[1:3], letters[1:3])))
  )
  
  expect_equal(
    thq_pairs(values = values_list,
              references = references,
              levels = NULL),
    as.table(matrix(c(0,1, 1,0), nrow = 2, byrow = TRUE,
                    dimnames = list(c("a","c"), c("a","c"))))
  )
  expect_equal(
    thq_pairs(values = values_list,
              references = references,
              levels = c("a", "b", "c")),
    as.table(matrix(c(0,0,1, 0,0,0, 1,0,0), nrow = 3, byrow = TRUE,
                    dimnames = list(letters[1:3], letters[1:3])))
  )
  
  
  # Arguments threshold, alone and ignore_zero, on the following matrix (and an equivalent list):
  #      a   b
  # s1 0.5 0.5
  # s2 0.0 2.0
  # s3 2.0 0.0
  # If threshold = T, alone = F, ignore_zero = T: NULL
  # If threshold = T, alone = F, ignore_zero = F: sets 2 and 3 is considered (ab = 2)
  # If threshold = F, alone = F, ignore_zero = T: set 1 is considered (ab = 1)
  # If threshold = F, alone = F, ignore_zero = F: set 1, 2 and 3 are considered (ab = 3)
  # If threshold = T, alone = T, ignore_zero = T: sets 2 and 3 are considered (aNULL = 1, bNULL = 1)
  # If threshold = T, alone = T, ignore_zero = F: sets 2 and 3 are considered (ab = 2)
  # If threshold = F, alone = T, ignore_zero = T: sets 1, 2 and 3 are considered (ab = 1, aNULL = 1, bNULL = 1)
  # If threshold = F, alone = T, ignore_zero = F: set 1, 2 and 3 are considered (ab = 3)
  values_matrix = matrix(c(0.5,0.5,
                           0,  2,
                           2,  0), nrow = 3, byrow = TRUE,
                         dimnames = list(c("s1", "s2", "s3"), letters[1:2]))
  values_list = list(s1 = c(a = 0.5, b = 0.5),
                     s2 = c(a = 0, b = 2),
                     s3 = c(a = 2, b = 0))
  references = c(a = 1, b = 1)
  
  expect_null(thq_pairs(values = values_matrix, references = references,
                        threshold = TRUE, alone = FALSE, ignore_zero = TRUE))
  expect_equal(
    thq_pairs(values = values_matrix, references = references,
              threshold = TRUE, alone = FALSE, ignore_zero = FALSE),
    as.table(matrix(c(0,2, 2,0), nrow = 2, byrow = TRUE,
                    dimnames = list(letters[1:2], letters[1:2])))
  )
  expect_equal(
    thq_pairs(values = values_matrix, references = references,
              threshold = FALSE, alone = FALSE, ignore_zero = TRUE),
    as.table(matrix(c(0,1, 1,0), nrow = 2, byrow = TRUE,
                    dimnames = list(letters[1:2], letters[1:2])))
  )
  expect_equal(
    thq_pairs(values = values_matrix, references = references,
              threshold = FALSE, alone = FALSE, ignore_zero = FALSE),
    as.table(matrix(c(0,3, 3,0), nrow = 2, byrow = TRUE,
                    dimnames = list(letters[1:2], letters[1:2])))
  )
  expect_equal(
    thq_pairs(values = values_matrix, references = references,
              threshold = TRUE, alone = TRUE, ignore_zero = TRUE),
    as.table(matrix(c(0,0,1, 0,0,1, 1,1,0), nrow = 3, byrow = TRUE,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  expect_equal(
    thq_pairs(values = values_matrix, references = references,
              threshold = TRUE, alone = TRUE, ignore_zero = FALSE),
    as.table(matrix(c(0,2,0, 2,0,0, 0,0,0), nrow = 3, byrow = TRUE,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  expect_equal(
    thq_pairs(values = values_matrix, references = references,
              threshold = FALSE, alone = TRUE, ignore_zero = TRUE),
    as.table(matrix(c(0,1,1, 1,0,1, 1,1,0), nrow = 3, byrow = TRUE,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  expect_equal(
    thq_pairs(values = values_matrix, references = references,
              threshold = FALSE, alone = TRUE, ignore_zero = FALSE),
    as.table(matrix(c(0,3,0, 3,0,0, 0,0,0), nrow = 3, byrow = TRUE,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  
  expect_null(thq_pairs(values = values_list, references = references,
                        threshold = TRUE, alone = FALSE, ignore_zero = TRUE))
  expect_equal(
    thq_pairs(values = values_list, references = references,
              threshold = TRUE, alone = FALSE, ignore_zero = FALSE),
    as.table(matrix(c(0,2, 2,0), nrow = 2, byrow = TRUE,
                    dimnames = list(letters[1:2], letters[1:2])))
  )
  expect_equal(
    thq_pairs(values = values_list, references = references,
              threshold = FALSE, alone = FALSE, ignore_zero = TRUE),
    as.table(matrix(c(0,1, 1,0), nrow = 2, byrow = TRUE,
                    dimnames = list(letters[1:2], letters[1:2])))
  )
  expect_equal(
    thq_pairs(values = values_list, references = references,
              threshold = FALSE, alone = FALSE, ignore_zero = FALSE),
    as.table(matrix(c(0,3, 3,0), nrow = 2, byrow = TRUE,
                    dimnames = list(letters[1:2], letters[1:2])))
  )
  expect_equal(
    thq_pairs(values = values_list, references = references,
              threshold = TRUE, alone = TRUE, ignore_zero = TRUE),
    as.table(matrix(c(0,0,1, 0,0,1, 1,1,0), nrow = 3, byrow = TRUE,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  expect_equal(
    thq_pairs(values = values_list, references = references,
              threshold = TRUE, alone = TRUE, ignore_zero = FALSE),
    as.table(matrix(c(0,2,0, 2,0,0, 0,0,0), nrow = 3, byrow = TRUE,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  expect_equal(
    thq_pairs(values = values_list, references = references,
              threshold = FALSE, alone = TRUE, ignore_zero = TRUE),
    as.table(matrix(c(0,1,1, 1,0,1, 1,1,0), nrow = 3, byrow = TRUE,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  expect_equal(
    thq_pairs(values = values_list, references = references,
              threshold = FALSE, alone = TRUE, ignore_zero = FALSE),
    as.table(matrix(c(0,3,0, 3,0,0, 0,0,0), nrow = 3, byrow = TRUE,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  
  
  # Values containing only 0s
  values_matrix = matrix(rep(0,10), nrow = 2,
                            dimnames = list(c("s1", "s2"), letters[1:5]))
  values_list = list(s1 = c(a = 0, b = 0, c = 0),
                     s2 = c(b = 0),
                     s3 = c(a = 0, c = 0))
  references = c(a = 1, b = 1, c = 1, d = 1, e = 1)
  
  expect_equal(
    thq_pairs(values = values_matrix, references = references,
              threshold = FALSE, ignore_zero = FALSE),
    as.table(matrix(c(0,2, 2,0), nrow = 2, byrow = TRUE,
                    dimnames = list(letters[1:2], letters[1:2])))
  )
  expect_null(thq_pairs(values = values_matrix, references = references,
                        threshold = FALSE, ignore_zero = TRUE))
  
  expect_equal(
    thq_pairs(values = values_list, references = references,
              threshold = FALSE, alone = TRUE, ignore_zero = FALSE),
    as.table(matrix(c(0,1,1,0, 1,0,0,1, 1,0,0,0, 0,1,0,0), nrow = 4, byrow = TRUE,
                    dimnames = list(c(letters[1:3], "NULL"), c(letters[1:3], "NULL"))))
  )
  expect_null(thq_pairs(values = values_list, references = references,
                        threshold = FALSE, alone = TRUE, ignore_zero = TRUE))
  
  
  # Matrix having only one value per set
  expect_equal(
    thq_pairs(values = matrix(c(1,1,1,1,1),
                              nrow = 5, dimnames = list(NULL, "a")),
              references = c(a = 1),
              threshold = FALSE, alone = TRUE),
    as.table(matrix(c(0,5, 5,0), nrow = 2, byrow = TRUE,
                    dimnames = list(c("a", "NULL"), c("a", "NULL"))))
  )
  
  # Matrix having only one set of values
  expect_equal(
    thq_pairs(values = matrix(c(1, 1),
                              nrow = 1, dimnames = list(NULL, letters[1:2])),
              references = c(a = 1, b = 1),
              threshold = FALSE, alone = TRUE),
    as.table(matrix(c(0,1,0, 1,0,0, 0,0,0), nrow = 3, byrow = TRUE,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  
  
  # List having only one value per set
  expect_equal(
    thq_pairs(values = list(c(a = 1),
                            c(b = 1),
                            c(c = 1)),
              references = c(a = 1, b = 1, c = 1),
              threshold = FALSE, alone = TRUE),
    as.table(matrix(c(0,0,0,1, 0,0,0,1, 0,0,0,1, 1,1,1,0), nrow = 4, byrow = TRUE,
                    dimnames = list(c(letters[1:3], "NULL"), c(letters[1:3], "NULL"))))
  )
  
  # List having only one set of values
  expect_equal(
    thq_pairs(values = list(c(a = 1, b = 1)),
              references = c(a = 1, b = 1),
              threshold = FALSE, alone = TRUE),
    as.table(matrix(c(0,1,0, 1,0,0, 0,0,0), nrow = 3, byrow = TRUE,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  
  
  # Diagonal
  expect_equal(
    diag(thq_pairs(values = list(c(a = 1, a = 1),
                                 c(a = 1),
                                 c(b = 1, b = 1),
                                 c(b = 1, c = 1)),
                   references = c(a = 1, b = 1, c = 1))),
    c(a = 1, b = 1, c = 0)
  )
})

test_that("thq_pairs returns an identical result whatever the structure of references", {
  # 'values' as a list: 'references' as a vector or a list
  values = list(s1 = c(a=1, b=2, c=3), s2 = c(a=2, b=1, c=3), s3 = c(b=3, c=4))
  references_vector = c(a=1, b=2, c=3)
  references_list = list(c(1,2,3), c(1,2,3), c(2,3))

  expect_identical(thq_pairs(values, references_vector),
                   thq_pairs(values, references_list))
})

test_that("thq_pairs returns an identical result whatever the chosen usage", {
  values_matrix = matrix(c(1,2,1,2,1,
                           2,1,1,1,2,
                           0,1,2,1,1), nrow = 3, byrow = TRUE,
                         dimnames = list(c("s1", "s2", "s3"), letters[1:5]))
  values_list = list(s1 = c(a=1, b=2, c=1, d=2, e=1),
                     s2 = c(a=2, b=1, c=1, d=1, e=2),
                     s3 = c(a=0, b=1, c=2, d=1, e=1))
  references = c(a=1, b=1, c=1, d=1, e=1)

  expect_identical(thq_pairs(values_matrix, references),
                   thq_pairs(hq = hazard_quotient(values_matrix, references),
                             hi = hazard_index(values_matrix, references)))
  expect_identical(thq_pairs(values_list, references),
                   thq_pairs(hq = t(sapply(values_list, hazard_quotient, references)),
                             hi = t(t(sapply(values_list, hazard_index, references)))))
})


##### thq_by_group #####

test_that("thq_by_group requires that references have the same sizes as values if they are two lists", {
  expect_error(thq_by_group(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                            list(1, 2, 3)),
               "length")
  expect_error(thq_by_group(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                            list(c(1, 2), 1, c(2, 3))),
               NA)
})

test_that("thq_by_group requires references to have named values if they are a vector and values are a list", {
  expect_error(thq_by_group(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                            c(1, 2, 3)),
               "name")
  expect_error(thq_by_group(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                            c(a = 1, b = 2, c = 3)),
               NA)
})

test_that("thq_by_group requires values to be named", {
  # 'values' as a matrix
  expect_error(thq_by_group(values = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE),
                            references = c(a = 1, b = 1)),
               "name")
  expect_error(thq_by_group(values = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE,
                                            dimnames = list(NULL, letters[1:2])),
                            references = c(a = 1, b = 1)),
               NA)
  
  # 'values' as a list
  expect_error(thq_by_group(values = list(c(1, 1),
                                          c(1, 1),
                                          c(1, 1)),
                            references = c(a = 1, b = 1)),
               "name")
  expect_error(thq_by_group(values = list(c(a = 1, a = 1),
                                          c(b = 1, b = 1),
                                          c(a = 1, b = 1)),
                            references = c(a = 1, b = 1)),
               NA)
  
  # 'hq' as a matrix
  expect_error(thq_by_group(hq = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE),
                            groups = c("I", "I", "I")),
               "name")
  expect_error(thq_by_group(hq = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE,
                                        dimnames = list(NULL, letters[1:2])),
                            groups = c("I", "I", "I")),
               NA)
  
  # 'hq' as a list
  expect_error(thq_by_group(hq = list(c(1, 1),
                                      c(1, 1),
                                      c(1, 1)),
                            groups = c("I", "I", "I")),
               "name")
  expect_error(thq_by_group(hq = list(c(a = 1, a = 1),
                                      c(b = 1, b = 1),
                                      c(a = 1, b = 1)),
                            groups = c("I", "I", "I")),
               NA)
  
  # 'thq' as a vector
  expect_error(thq_by_group(thq = c(1, 1, 1),
                            groups = c("I", "I", "I")),
               "name")
  expect_error(thq_by_group(thq = c(a = 1, b = 1, a = 1),
                            groups = c("I", "I", "I")),
               NA)
  
  # 'thq' as a list
  expect_error(thq_by_group(thq = list(c(2, 1, 0.5),
                                       c(2, 1),
                                       c(1)),
                            groups = c("I", "I", "I")),
               "name")
  expect_error(thq_by_group(thq = list(c(b = 2, a = 1, c = 0.5),
                                       c(a = 2, b = 1),
                                       c(a = 1)),
                            groups = c("I", "I", "I")),
               NA)
})

test_that("thq_by_group ignores values equal to 0 if ignore_zero is TRUE", {
  values_matrix = matrix(c(1,0,1, 0,0,0, 0,0,1), nrow = 3, byrow = TRUE,
                         dimnames = list(NULL, letters[1:3]))
  values_list = list(c(a = 1, b = 1),
                     c(b = 0),
                     c(a = 0, c = 1))
  references = c(a = 1, b = 1, c = 1)
  
  # 'values' as a matrix
  expect_equal(
    sum(thq_by_group(values = values_matrix,
                     references = references,
                     ignore_zero = TRUE)),
    2
  )
  expect_equal(
    sum(thq_by_group(values = values_matrix,
                     references = references,
                     ignore_zero = FALSE)),
    3
  )
  
  # 'values' as a list
  expect_equal(
    sum(thq_by_group(values = values_list,
                     references = references,
                     ignore_zero = TRUE)),
    2
  )
  expect_equal(
    sum(thq_by_group(values = values_list,
                     references = references,
                     ignore_zero = FALSE)),
    3
  )
  
  # Values containing only 0s
  expect_equal(
    dim(thq_by_group(values = matrix(rep(0,6), nrow = 2,
                                     dimnames = list(c("s1", "s2"), letters[1:3])),
                     references = references,
                     ignore_zero = TRUE)),
    c(0, 4)
  )
  expect_equal(
    dim(thq_by_group(values = list(s1 = c(a = 0, b = 0, c = 0),
                                   s2 = c(b = 0),
                                   s3 = c(a = 0, c = 0)),
                     references = references,
                     ignore_zero = TRUE)),
    c(0, 4)
  )
})

test_that("thq_by_group ignores or considers value names according to the argument levels", {
  values_matrix = matrix(c(1,1,0, 1,0,0, 0,0,1), nrow = 3, byrow = TRUE,
                         dimnames = list(NULL, letters[1:3]))
  values_list = list(c(a = 1, b = 1),
                     c(a = 1),
                     c(c = 1))
  references = c(a = 1, b = 1, c = 1)
  
  # 'values' as a matrix
  expect_equal(
    rownames(thq_by_group(values = values_matrix,
                         references = references,
                         levels = NULL)),
    c("a", "c")
  )
  expect_equal(
    rownames(thq_by_group(values = values_matrix,
                          references = references,
                          levels = c("a", "b", "c"))),
    c("a", "b", "c")
  )
  
  # 'values' as a list
  expect_equal(
    rownames(thq_by_group(values = values_list,
                          references = references,
                          levels = NULL)),
    c("a", "c")
  )
  expect_equal(
    rownames(thq_by_group(values = values_list,
                          references = references,
                          levels = c("a", "b", "c"))),
    c("a", "b", "c")
  )
  
  # Values containing only 0s
  expect_equal(
    rownames(thq_by_group(values = matrix(rep(0,6), nrow = 2,
                                          dimnames = list(c("s1", "s2"), letters[1:3])),
                          references = references,
                          ignore_zero = TRUE, levels = c("a", "b", "c"))),
    c("a", "b", "c")
  )
  expect_equal(
    rownames(thq_by_group(values = list(s1 = c(a = 0, b = 0, c = 0),
                                   s2 = c(b = 0),
                                   s3 = c(a = 0, c = 0)),
                     references = references,
                     ignore_zero = TRUE, levels = c("a", "b", "c"))),
    c("a", "b", "c")
  )
})

test_that("thq_by_group counts the top hazard quotients by group", {
  
  values_matrix = matrix(c(1,2,0,0, 0,1,0,2, 0.5,0,0,3.2, 0.9,1.8,0,0),
                         nrow = 4, byrow = TRUE, dimnames = list(NULL, letters[1:4]))
  values_list = list(c(a=1, b=2, c=0.1), c(b=1, d=2), c(a=0.5, d=3.2), c(a=0.9, a=0.9))
  references = c(a=1, b=2, c=10, d=4)
  
  expect_equal(
    thq_by_group(values = values_matrix,
                 references = references),
    as.table(matrix(c(1,0,0, 0,1,0, 0,0,1, 1,0,0), ncol = 4,
                    dimnames = list(c("a","b","d"), c("I","II","IIIA","IIIB"))))
  )
  expect_equal(
    thq_by_group(values = values_list,
                 references = references),
    as.table(matrix(c(1,0,0, 0,1,0, 0,0,1, 1,0,0), ncol = 4,
                    dimnames = list(c("a","b","d"), c("I","II","IIIA","IIIB"))))
  )
  
  
  # Argument levels
  values_matrix = matrix(c(1,1,0, 1,0,0, 0,0,1), nrow = 3, byrow = TRUE,
                         dimnames = list(NULL, letters[1:3]))
  values_list = list(c(a = 1, b = 1),
                     c(a = 1),
                     c(c = 1))
  references = c(a = 1, b = 1, c = 1)
  
  expect_equal(
    thq_by_group(values = values_matrix,
                 references = references,
                 levels = NULL),
    as.table(matrix(c(2,1, 0,0, 0,0, 0,0), ncol = 4,
                    dimnames = list(c("a","c"), c("I","II","IIIA","IIIB"))))
  )
  expect_equal(
    thq_by_group(values = values_matrix,
                 references = references,
                 levels = c("a", "b", "c")),
    as.table(matrix(c(2,0,1, 0,0,0, 0,0,0, 0,0,0), ncol = 4,
                    dimnames = list(c("a","b","c"), c("I","II","IIIA","IIIB"))))
  )
  
  expect_equal(
    thq_by_group(values = values_list,
                 references = references,
                 levels = NULL),
    as.table(matrix(c(2,1, 0,0, 0,0, 0,0), ncol = 4,
                    dimnames = list(c("a","c"), c("I","II","IIIA","IIIB"))))
  )
  expect_equal(
    thq_by_group(values = values_list,
                 references = references,
                 levels = c("a", "b", "c")),
    as.table(matrix(c(2,0,1, 0,0,0, 0,0,0, 0,0,0), ncol = 4,
                    dimnames = list(c("a","b","c"), c("I","II","IIIA","IIIB"))))
  )
  
  
  # Matrix having only one value per set
  expect_equal(
    thq_by_group(values = matrix(c(1,1,1,1,1), nrow = 5, dimnames = list(NULL, "a")),
                 references = c(a = 1)),
    as.table(matrix(c(5,0,0,0), ncol = 4, dimnames = list("a", c("I","II","IIIA","IIIB"))))
  )
  
  # Matrix having only one set of values
  expect_equal(
    thq_by_group(values = matrix(c(1, 1), nrow = 1, dimnames = list(NULL, letters[1:2])),
              references = c(a = 1, b = 1)),
    as.table(matrix(c(1,0,0,0), ncol = 4, dimnames = list("a", c("I","II","IIIA","IIIB"))))
  )
  
  
  # List having only one value per set
  expect_equal(
    thq_by_group(values = list(c(a = 1), c(b = 1), c(c = 1)),
                references = c(a = 1, b = 1, c = 1)),
    as.table(matrix(c(1,1,1, 0,0,0, 0,0,0, 0,0,0), ncol = 4,
                    dimnames = list(letters[1:3], c("I","II","IIIA","IIIB"))))
  )
  
  # List having only one set of values
  expect_equal(
    thq_by_group(values = list(c(a = 1, b = 1)),
                 references = c(a = 1, b = 1)),
    as.table(matrix(c(1,0,0,0), ncol = 4, dimnames = list("a", c("I","II","IIIA","IIIB"))))
  )
})

test_that("thq_by_group returns an identical result whatever the structure of references", {
  # 'values' as a list: 'references' as a vector or a list
  values = list(s1 = c(a=1, b=2), s2 = c(b=1, c=2), s3 = c(a=0.5, c=3.2), s4 = c(a=0.9, a=0.9))
  references_vector = c(a=1, b=2, c=4)
  references_list = list(c(1,2), c(2,4), c(1,4), c(1,1))
  
  expect_identical(thq_by_group(values, references_vector),
                   thq_by_group(values, references_list))
})

test_that("thq_by_group returns an identical result whatever the chosen usage", {
  values_matrix = matrix(c(1,2,0, 0,1,2, 0.5,0,3.2, 0.9,1.8,0),
                         nrow = 4, byrow = TRUE, dimnames = list(NULL, letters[1:3]))
  values_list = list(s1 = c(a=1, b=2), s2 = c(b=1, c=2), s3 = c(a=0.5, c=3.2), s4 = c(a=0.9, a=0.9))
  references_vector = c(a=1, b=2, c=4)
  references_list = list(c(1,2), c(2,4), c(1,4), c(1,1))
  
  # 'values' as a matrix
  expect_identical(thq_by_group(values_matrix, references_vector),
                   thq_by_group(hq     = hazard_quotient(values_matrix, references_vector),
                                groups = classify_mixture(values_matrix, references_vector)))
  expect_identical(thq_by_group(values_matrix, references_vector),
                   thq_by_group(thq    = top_hazard_quotient(values_matrix, references_vector),
                                groups = classify_mixture(values_matrix, references_vector)))
  
  # 'values' as a list
  expect_identical(
    thq_by_group(values_list, references_list),
    thq_by_group(hq = lapply(seq_along(values_list),
                             function(v) hazard_quotient(values_list[[v]], references_list[[v]])),
                 groups = sapply(seq_along(values_list),
                                 function(v) classify_mixture(values_list[[v]], references_list[[v]])))
  )
  expect_identical(
    thq_by_group(values_list, references_list),
    thq_by_group(thq = lapply(seq_along(values_list),
                              function(v) top_hazard_quotient(values_list[[v]], references_list[[v]])),
                 groups = sapply(seq_along(values_list),
                                 function(v) classify_mixture(values_list[[v]], references_list[[v]])))
  )
})



#### Application of the MCR approach according to classes ####

##### mcr_summary_by_class #####

test_that("mcr_summary_by_class requires that references have the same sizes as values if they are two lists", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  values_list = list(s1 = c(A = 1, B = 2), s2 = c(C = 2), s3 = c(B = 3, D = 4))
  
  expect_error(mcr_summary_by_class(values = values_list,
                                    references = list(1, 2, 3),
                                    classes = classes),
               "length")
  expect_error(mcr_summary_by_class(values = values_list,
                                    references = list(c(1, 2), 1, c(2, 3)),
                                    classes = classes),
               NA)
})

test_that("mcr_summary_by_class requires references to have named values if they are a vector and values are a list", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  
  expect_error(mcr_summary_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, C=4)),
                                    references = c(1, 2, 3),
                                    classes = classes),
               "name")
  expect_error(mcr_summary_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, C=4)),
                                    references = c(A = 1, B = 2, C = 3),
                                    classes = classes),
               NA)
})

test_that("mcr_summary_by_class requires values to be named", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  # 'values' as a vector
  expect_error(mcr_summary_by_class(values = c(1, 2, 3, 4),
                                    references = references,
                                    classes = classes),
               "name")
  expect_error(mcr_summary_by_class(values = c(A = 1, B = 2, C = 3, D = 4),
                                    references = references,
                                    classes = classes),
               NA)
  
  # 'values' as a matrix
  expect_error(mcr_summary_by_class(values = matrix(c(1,2,3,4, 1,2,3,4), nrow = 2, byrow = TRUE,
                                                    dimnames = list(c("s1", "s2"))),
                                    references = references,
                                    classes = classes),
               "name")
  expect_error(mcr_summary_by_class(values = matrix(c(1,2,3,4, 1,2,3,4), nrow = 2, byrow = TRUE,
                                                    dimnames = list(c("s1", "s2"), letters[1:4])),
                                    references = references,
                                    classes = classes),
               NA)
  
  # 'values' as a list
  expect_error(mcr_summary_by_class(values = list(s1 = c(1, 2), s2 = c(2), s3 = c(3, 4)),
                                    references = references,
                                    classes = classes),
               "name")
  expect_error(mcr_summary_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4)),
                                    references = references,
                                    classes = classes),
               NA)
})

test_that("mcr_summary_by_class returns the right data structure", {
  is_real_list = function(x) is.list(x) && !is.data.frame(x)
  all_df = function(x) all(sapply(x, class) == "data.frame")
  
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  values_vector = c(A = 1, B = 2, C = 3, D = 4)
  values_matrix = matrix(c(1,2,3,4, 1,2,3,4), nrow = 2, byrow = TRUE,
                         dimnames = list(c("s1", "s2"), LETTERS[1:4]))
  values_list = list(s1 = c(A = 1, B = 2, C = 3, D = 4),
                     s2 = c(A = 1, B = 2, C = 3, D = 4),
                     s3 = c(A = 1, B = 2, C = 3, D = 4))
  
  
  ## 'by_set' is TRUE
  # Vector -> Data frame
  expect_s3_class(mcr_summary_by_class(values = values_vector,
                                       references = references,
                                       classes = classes,
                                       by_set = TRUE),
                  "data.frame")
  
  # Matrix (several or a single set of values) -> List of data frames
  expect_true(is_real_list(mcr_summary_by_class(values = values_matrix,
                                                references = references,
                                                classes = classes,
                                                by_set = TRUE)))
  expect_true(all_df(mcr_summary_by_class(values = values_matrix,
                                          references = references,
                                          classes = classes,
                                          by_set = TRUE)))
  expect_true(is_real_list(mcr_summary_by_class(values = values_matrix[1, , drop = FALSE],
                                                references = references,
                                                classes = classes,
                                                by_set = TRUE)))
  expect_true(all_df(mcr_summary_by_class(values = values_matrix[1, , drop = FALSE],
                                          references = references,
                                          classes = classes,
                                          by_set = TRUE)))
  
  # List (several or a single set of values) -> List of data frames
  expect_true(is_real_list(mcr_summary_by_class(values = values_list,
                                                references = references,
                                                classes = classes,
                                                by_set = TRUE)))
  expect_true(all_df(mcr_summary_by_class(values = values_list,
                                          references = references,
                                          classes = classes,
                                          by_set = TRUE)))
  expect_true(is_real_list(mcr_summary_by_class(values = values_list[1],
                                                references = references,
                                                classes = classes,
                                                by_set = TRUE)))
  expect_true(all_df(mcr_summary_by_class(values = values_list[1],
                                          references = references,
                                          classes = classes,
                                          by_set = TRUE)))
  
  
  ## 'by_set' is FALSE
  # Matrix -> List of data frames
  expect_true(is_real_list(mcr_summary_by_class(values = values_matrix,
                                                references = references,
                                                classes = classes,
                                                by_set = FALSE)))
  expect_true(all_df(mcr_summary_by_class(values = values_matrix,
                                          references = references,
                                          classes = classes,
                                          by_set = FALSE)))
  
  # List -> List of data frames
  expect_true(is_real_list(mcr_summary_by_class(values = values_list,
                                                references = references,
                                                classes = classes,
                                                by_set = FALSE)))
  expect_true(all_df(mcr_summary_by_class(values = values_list,
                                          references = references,
                                          classes = classes,
                                          by_set = FALSE)))
})

test_that("mcr_summary_by_class returns as many objects as there are sets of values if by_set is TRUE", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  values_matrix = matrix(c(1,2,3,4, 1,2,3,4), nrow = 2, byrow = TRUE,
                         dimnames = list(c("s1", "s2"), LETTERS[1:4]))
  values_list = list(s1 = c(A = 1, B = 2, C = 3, D = 4),
                     s2 = c(A = 1, B = 2, C = 3, D = 4),
                     s3 = c(A = 1, B = 2, C = 3, D = 4))
  
  # 'values' as a matrix
  expect_length(mcr_summary_by_class(values = values_matrix,
                                     references = references,
                                     classes = classes,
                                     by_set = TRUE),
                2)
  
  # 'values' as a list
  expect_length(mcr_summary_by_class(values = values_list,
                                     references = references,
                                     classes = classes,
                                     by_set = TRUE),
                3)
})

test_that("mcr_summary_by_class returns as many objects as there are classes in the values if by_set is FALSE", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  values_matrix = matrix(c(1,2,3,4, 1,2,3,4), nrow = 2, byrow = TRUE,
                         dimnames = list(c("s1", "s2"), LETTERS[1:4]))
  values_list = list(s1 = c(A = 1, B = 2, C = 3),
                     s2 = c(A = 1, B = 2, C = 3),
                     s3 = c(A = 1, B = 2, C = 3))
  
  # 'values' as a matrix
  expect_length(mcr_summary_by_class(values = values_matrix,
                                     references = references,
                                     classes = classes,
                                     by_set = FALSE),
                5)
  
  # 'values' as a list
  expect_length(mcr_summary_by_class(values = values_list,
                                     references = references,
                                     classes = classes,
                                     by_set = FALSE),
                4)
})

test_that("mcr_summary_by_class differentiates values equal to 0 if ignore_zero is TRUE", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  values_vector = c(A = 1, B = 0, C = 3, D = 0)
  values_matrix = matrix(c(1,0,3,0, 1,2,3,4), nrow = 2, byrow = TRUE,
                         dimnames = list(c("s1", "s2"), LETTERS[1:4]))
  values_list = list(s1 = c(A = 1, C = 3, D = 0),
                     s2 = c(A = 1, B = 2, C = 3, D = 4),
                     s3 = c(A = 0, C = 2))
  
  # 'values' as a vector
  expect_equal(mcr_summary_by_class(values = values_vector,
                                    references = references,
                                    classes = classes,
                                    ignore_zero = FALSE)$n,
               c(1,2,2,3,4))
  expect_null(mcr_summary_by_class(values = values_vector,
                                   references = references,
                                   classes = classes,
                                   ignore_zero = FALSE)$n_zero)
  expect_equal(mcr_summary_by_class(values = values_vector,
                                    references = references,
                                    classes = classes,
                                    ignore_zero = TRUE)$n,
               c(0,1,0,1,2))
  expect_equal(mcr_summary_by_class(values = values_vector,
                                    references = references,
                                    classes = classes,
                                    ignore_zero = TRUE)$n_zero,
               c(1,1,2,2,2))
  
  # 'values' as a matrix
  expect_equal(lapply(mcr_summary_by_class(values = values_matrix,
                                           references = references,
                                           classes = classes,
                                           ignore_zero = FALSE,
                                           by_set = TRUE),
                      function(s) s$n),
               list(s1 = c(1,2,2,3,4), s2 = c(1,2,2,3,4)))
  expect_equal(lapply(mcr_summary_by_class(values = values_matrix,
                                           references = references,
                                           classes = classes,
                                           ignore_zero = FALSE,
                                           by_set = TRUE),
                      function(s) s$n_zero),
               list(s1 = NULL, s2 = NULL))
  expect_equal(lapply(mcr_summary_by_class(values = values_matrix,
                                           references = references,
                                           classes = classes,
                                           ignore_zero = TRUE,
                                           by_set = TRUE),
                      function(s) s$n),
               list(s1 = c(0,1,0,1,2), s2 = c(1,2,2,3,4)))
  expect_equal(lapply(mcr_summary_by_class(values = values_matrix,
                                           references = references,
                                           classes = classes,
                                           ignore_zero = TRUE,
                                           by_set = TRUE),
                      function(s) s$n_zero),
               list(s1 = c(1,1,2,2,2), s2 = c(0,0,0,0,0)))
  
  # 'values' as a list
  expect_equal(lapply(mcr_summary_by_class(values = values_list,
                                           references = references,
                                           classes = classes,
                                           ignore_zero = FALSE,
                                           by_set = TRUE),
                      function(s) s$n),
               list(s1 = c(1,2,1,2,3), s2 = c(1,2,2,3,4), s3 = c(1,1,2)))
  expect_equal(lapply(mcr_summary_by_class(values = values_list,
                                           references = references,
                                           classes = classes,
                                           ignore_zero = FALSE,
                                           by_set = TRUE),
                      function(s) s$n_zero),
               list(s1 = NULL, s2 = NULL, s3 = NULL))
  expect_equal(lapply(mcr_summary_by_class(values = values_list,
                                           references = references,
                                           classes = classes,
                                           ignore_zero = TRUE,
                                           by_set = TRUE),
                      function(s) s$n),
               list(s1 = c(0,1,0,1,2), s2 = c(1,2,2,3,4), s3 = c(0,0,1)))
  expect_equal(lapply(mcr_summary_by_class(values = values_list,
                                           references = references,
                                           classes = classes,
                                           ignore_zero = TRUE,
                                           by_set = TRUE),
                      function(s) s$n_zero),
               list(s1 = c(1,1,1,1,1), s2 = c(0,0,0,0,0), s3 = c(1,1,1)))
})

test_that("mcr_summary_by_class returns data frames containing as many rows as classes if by_set is TRUE", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  values_vector = c(A = 1, C = 3)
  values_matrix = matrix(c(1,0,3,0, 1,2,3,4), nrow = 2, byrow = TRUE,
                         dimnames = list(c("s1", "s2"), LETTERS[1:4]))
  values_list = list(s1 = c(A = 1, C = 3),
                     s2 = c(A = 1, B = 2, C = 3, D = 4))
  
  # 'all_classes' as FALSE
  expect_equal(nrow(mcr_summary_by_class(values = values_vector,
                                         references = references[c("A", "C")],
                                         classes = classes,
                                         by_set = TRUE,
                                         all_classes = FALSE)),
               3)
  expect_equal(sapply(mcr_summary_by_class(values = values_matrix,
                                           references = references,
                                           classes = classes,
                                           by_set = TRUE,
                                           all_classes = FALSE),
                      nrow),
               c(s1 = 5, s2 = 5))
  expect_equal(sapply(mcr_summary_by_class(values = values_list,
                                           references = references,
                                           classes = classes,
                                           by_set = TRUE,
                                           all_classes = FALSE),
                      nrow),
               c(s1 = 3, s2 = 5))
  
  # 'all_classes' as TRUE
  expect_equal(nrow(mcr_summary_by_class(values = values_vector,
                                         references = references[c("A", "C")],
                                         classes = classes,
                                         by_set = TRUE,
                                         all_classes = TRUE)),
               5)
  expect_equal(sapply(mcr_summary_by_class(values = values_matrix,
                                           references = references,
                                           classes = classes,
                                           by_set = TRUE,
                                           all_classes = TRUE),
                      nrow),
               c(s1 = 5, s2 = 5))
  expect_equal(sapply(mcr_summary_by_class(values = values_list,
                                           references = references,
                                           classes = classes,
                                           by_set = TRUE,
                                           all_classes = TRUE),
                      nrow),
               c(s1 = 5, s2 = 5))
})

test_that("mcr_summary_by_class returns data frames containing as many rows as sets of values if by_set is FALSE", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  values_matrix = matrix(c(1,0,3,0, 1,2,3,4), nrow = 2, byrow = TRUE,
                         dimnames = list(c("s1", "s2"), LETTERS[1:4]))
  values_list = list(s1 = c(A = 1, C = 3),
                     s2 = c(A = 1, B = 2, C = 3, D = 4))
  
  expect_equal(sapply(mcr_summary_by_class(values = values_matrix,
                                           references = references,
                                           classes = classes,
                                           by_set = FALSE),
                      nrow),
               c(C1 = 2, C2 = 2, C3 = 2, C4 = 2, C5 = 2))
  expect_equal(sapply(mcr_summary_by_class(values = values_list,
                                           references = references,
                                           classes = classes,
                                           by_set = FALSE),
                      nrow),
               c(C1 = 1, C2 = 2, C3 = 1, C4 = 2, C5 = 2))
})

test_that("mcr_summary_by_class returns NULL if values do not belong to any class and all_classes is FALSE", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"),
                 E = character(0),
                 F = character(0))
  references = c(A = 1, B = 2, C = 3, D = 4, E = 5, F = 6)
  values_matrix = matrix(c(0,6, 5,6), nrow = 2, byrow = TRUE,
                         dimnames = list(c("s1", "s2"), c("E", "F")))
  values_list = list(s1 = c(E = 5),
                     s2 = c(A = 1, B = 2, C = 3, D = 4, E = 5),
                     s3 = c(A = 0, E = 5))
  
  # 'values' as a vector
  expect_null(mcr_summary_by_class(values = c(E = 5),
                                   references = references["E"],
                                   classes = classes,
                                   by_set = TRUE,
                                   all_classes = FALSE))
  expect_false(is.null(mcr_summary_by_class(values = c(A = 0, E = 5),
                                            references = references[c("A", "E")],
                                            classes = classes,
                                            by_set = TRUE,
                                            all_classes = FALSE)))
  
  # 'values' as a matrix
  expect_equal(mcr_summary_by_class(values = values_matrix,
                                    references = references,
                                    classes = classes,
                                    by_set = TRUE,
                                    all_classes = FALSE),
               list(s1 = NULL, s2 = NULL))
  
  # 'values' as a list
  expect_null(mcr_summary_by_class(values = values_list,
                                   references = references,
                                   classes = classes,
                                   by_set = TRUE,
                                   all_classes = FALSE)$s1)
  expect_false(is.null(mcr_summary_by_class(values = values_list,
                                            references = references,
                                            classes = classes,
                                            by_set = TRUE,
                                            all_classes = FALSE)$s3))
})

test_that("mcr_summary_by_class does not return NULL if values do not belong to any class and all_classes is TRUE", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"),
                 E = character(0),
                 F = character(0))
  references = c(A = 1, B = 2, C = 3, D = 4, E = 5, F = 6)
  values_matrix = matrix(c(0,6, 5,6), nrow = 2, byrow = TRUE,
                         dimnames = list(c("s1", "s2"), c("E", "F")))
  values_list = list(s1 = c(E = 5),
                     s2 = c(A = 1, B = 2, C = 3, D = 4, E = 5),
                     s3 = c(A = 0, E = 5))
  
  
  # 'values' as a vector
  expect_false(is.null(mcr_summary_by_class(values = c(E = 5),
                                            references = references["E"],
                                            classes = classes,
                                            by_set = TRUE,
                                            all_classes = TRUE)))
  expect_false(is.null(mcr_summary_by_class(values = c(A = 0, E = 5),
                                            references = references[c("A", "E")],
                                            classes = classes,
                                            by_set = TRUE,
                                            all_classes = TRUE)))
  
  # 'values' as a matrix
  expect_false(is.null(mcr_summary_by_class(values = values_matrix,
                                            references = references,
                                            classes = classes,
                                            by_set = TRUE,
                                            all_classes = TRUE)$s1))
  
  # 'values' as a list
  expect_false(is.null(mcr_summary_by_class(values = values_list,
                                            references = references,
                                            classes = classes,
                                            by_set = TRUE,
                                            all_classes = TRUE)$s1))
})

test_that("mcr_summary_by_class returns named objects", {
  col_names = c("n", "n_zero", "HI", "MCR", "Reciprocal", "Group", "THQ", "MHQ", "Missed")
  
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  values_vector1 = c(A = 1, C = 3)
  values_vector2 = c(A = 1, B = 2, C = 3, D = 4)
  values_matrix = matrix(c(1,0,3,0, 1,2,3,4), nrow = 2, byrow = TRUE,
                         dimnames = list(c("s1", "s2"), LETTERS[1:4]))
  values_list = list(s1 = c(A = 1, C = 3),
                     s2 = c(A = 1, B = 2, C = 3, D = 4))
  
  ## 'by_set' is TRUE
  # 'values' as a vector
  expect_equal(rownames(mcr_summary_by_class(values = values_vector1,
                                             references = c(A = 1, C = 3),
                                             classes = classes,
                                             by_set = TRUE,
                                             all_classes = FALSE)),
               c("C2", "C4", "C5"))
  expect_equal(rownames(mcr_summary_by_class(values = values_vector2,
                                             references = references,
                                             classes = classes,
                                             by_set = TRUE,
                                             all_classes = FALSE)),
               c("C1", "C2", "C3", "C4", "C5"))
  expect_equal(colnames(mcr_summary_by_class(values = values_vector2,
                                             references = references,
                                             classes = classes,
                                             by_set = TRUE,
                                             all_classes = FALSE)),
               col_names)
  
  # 'values' as a matrix
  expect_named(mcr_summary_by_class(values = values_matrix,
                                    references = references,
                                    classes = classes,
                                    by_set = TRUE,
                                    all_classes = FALSE),
               c("s1", "s2"))
  expect_equal(lapply(mcr_summary_by_class(values = values_matrix,
                                           references = references,
                                           classes = classes,
                                           by_set = TRUE,
                                           all_classes = FALSE),
                      rownames),
               list(s1 = c("C1", "C2", "C3", "C4", "C5"),
                    s2 = c("C1", "C2", "C3", "C4", "C5")))
  expect_equal(lapply(mcr_summary_by_class(values = values_matrix,
                                           references = references,
                                           classes = classes,
                                           by_set = TRUE,
                                           all_classes = FALSE),
                      colnames),
               list(s1 = col_names, s2 = col_names))
  
  # 'values' as a list
  expect_named(mcr_summary_by_class(values = values_list,
                                    references = references,
                                    classes = classes,
                                    by_set = TRUE,
                                    all_classes = FALSE),
               c("s1", "s2"))
  expect_equal(lapply(mcr_summary_by_class(values = values_list,
                                           references = references,
                                           classes = classes,
                                           by_set = TRUE,
                                           all_classes = FALSE),
                      rownames),
               list(s1 = c("C2", "C4", "C5"),
                    s2 = c("C1", "C2", "C3", "C4", "C5")))
  expect_equal(lapply(mcr_summary_by_class(values = values_list,
                                           references = references,
                                           classes = classes,
                                           by_set = TRUE,
                                           all_classes = FALSE),
                      colnames),
               list(s1 = col_names, s2 = col_names))
  
  
  ## 'by_set' is FALSE
  # 'values' as a matrix
  expect_named(mcr_summary_by_class(values = values_matrix,
                                    references = references,
                                    classes = classes,
                                    by_set = FALSE),
               c("C1", "C2", "C3", "C4", "C5"))
  expect_equal(lapply(mcr_summary_by_class(values = values_matrix,
                                           references = references,
                                           classes = classes,
                                           by_set = FALSE),
                      rownames),
               list(C1 = c("s1", "s2"),
                    C2 = c("s1", "s2"),
                    C3 = c("s1", "s2"),
                    C4 = c("s1", "s2"),
                    C5 = c("s1", "s2")))
  expect_equal(lapply(mcr_summary_by_class(values = values_matrix,
                                           references = references,
                                           classes = classes,
                                           by_set = FALSE),
                      colnames),
               list(C1 = col_names, C2 = col_names, C3 = col_names, C4 = col_names, C5 = col_names))
  
  # 'values' as a list
  expect_named(mcr_summary_by_class(values = values_list,
                                    references = references,
                                    classes = classes,
                                    by_set = FALSE),
               c("C1", "C2", "C3", "C4", "C5"))
  expect_equal(lapply(mcr_summary_by_class(values = values_list,
                                           references = references,
                                           classes = classes,
                                           by_set = FALSE),
                       rownames),
               list(C1 = "s2",
                    C2 = c("s1", "s2"),
                    C3 = "s2",
                    C4 = c("s1", "s2"),
                    C5 = c("s1", "s2")))
  expect_equal(lapply(mcr_summary_by_class(values = values_list,
                                           references = references,
                                           classes = classes,
                                           by_set = FALSE),
                      colnames),
               list(C1 = col_names, C2 = col_names, C3 = col_names, C4 = col_names, C5 = col_names))
})


##### mcr_chart_by_class #####

test_that("mcr_chart_by_class requires that references have the same sizes as values if they are two lists", {
  pdf(NULL)
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  values_list = list(s1 = c(A = 1, B = 2), s2 = c(C = 2), s3 = c(B = 3, D = 4))
  
  expect_error(suppressMessages(suppressWarnings(
    mcr_chart_by_class(values = values_list,
                       references = list(1, 2, 3),
                       classes = classes))),
    "length")
  expect_error(suppressMessages(suppressWarnings(
    mcr_chart_by_class(values = values_list,
                       references = list(c(1, 2), 1, c(2, 3)),
                       classes = classes))),
    NA)
  dev.off()
})

test_that("mcr_chart_by_class requires references to have named values if they are a vector and values are a list", {
  pdf(NULL)
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  values_list = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, C=4))
  
  expect_error(suppressMessages(suppressWarnings(
    mcr_chart_by_class(values = values_list,
                       references = c(1, 2, 3),
                       classes = classes))),
    "name")
  expect_error(suppressMessages(suppressWarnings(
    mcr_chart_by_class(values = values_list,
                       references = c(A = 1, B = 2, C = 3),
                       classes = classes))),
    NA)
  dev.off()
})

test_that("mcr_chart_by_class requires values to be named", {
  pdf(NULL)
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  # 'values' as a matrix
  expect_error(suppressMessages(suppressWarnings(
    mcr_chart_by_class(values = matrix(c(1,2,3,4, 1,2,3,4), nrow = 2, byrow = TRUE,
                                       dimnames = list(c("s1", "s2"))),
                       references = references,
                       classes = classes))),
    "name")
  expect_error(suppressMessages(suppressWarnings(
    mcr_chart_by_class(values = matrix(c(1,2,3,4, 1,2,3,4), nrow = 2, byrow = TRUE,
                                       dimnames = list(c("s1", "s2"), letters[1:4])),
                       references = references,
                       classes = classes))),
    NA)
  
  # 'values' as a list
  expect_error(suppressMessages(suppressWarnings(
    mcr_chart_by_class(values = list(s1 = c(1, 2), s2 = c(2), s3 = c(3, 4)),
                       references = references,
                       classes = classes))),
    "name")
  expect_error(suppressMessages(suppressWarnings(
    mcr_chart_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4)),
                       references = references,
                       classes = classes))),
    NA)
  dev.off()
})

test_that("mcr_chart_by_class returns as many objects as there are classes in the values", {
  pdf(NULL)
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  expect_length(suppressMessages(suppressWarnings(
    mcr_chart_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4)),
                       references = references,
                       classes = classes))),
    5)
  
  expect_length(suppressMessages(suppressWarnings(
    mcr_chart_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, C=4)),
                       references = references,
                       classes = classes))),
    4)
  dev.off()
})

test_that("mcr_chart_by_class returns a named list", {
  pdf(NULL)
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  results = suppressMessages(suppressWarnings(
    mcr_chart_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4)),
                       references = references,
                       classes = classes)
  ))
  
  expect_type(results, "list")
  expect_named(results, c("C1", "C2", "C3", "C4", "C5"))
  
  dev.off()
})

test_that("mcr_chart_by_class returns ggplots objects or NULL objects", {
  pdf(NULL)
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  results = suppressMessages(suppressWarnings(
    mcr_chart_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4)),
                       references = references,
                       classes = classes)
  ))
  
  expect_null(results[[1]])
  expect_null(results[[2]])
  expect_s3_class(results[[3]], "ggplot")
  expect_s3_class(results[[4]], "ggplot")
  expect_s3_class(results[[5]], "ggplot")
  
  dev.off()
})

test_that("mcr_chart_by_class generates a message if some points cannot be plotted in a chart", {
  pdf(NULL)
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C1", "C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  expect_message(suppressWarnings(
    mcr_chart_by_class(values = list(s1 = c(A=1, B=2),
                                     s2 = c(A=2),
                                     s3 = c(B=3, D=4)),
                       references = references,
                       classes = classes)),
    "classes")
  expect_message(suppressWarnings(
    mcr_chart_by_class(values = list(s1 = c(A=1, B=2, C=3, D=4),
                                     s2 = c(A=2, B=3, C=4, D=5)),
                       references = references,
                       classes = classes)),
    NA)
  
  dev.off()
})


##### thq_pairs_by_class #####

test_that("thq_pairs_by_class requires that references have the same sizes as values if they are two lists", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  values_list = list(s1 = c(A = 1, B = 2), s2 = c(C = 2), s3 = c(B = 3, D = 4))
  
  expect_error(thq_pairs_by_class(values = values_list,
                                  references = list(1, 2, 3),
                                  classes = classes),
               "length")
  expect_error(thq_pairs_by_class(values = values_list,
                                  references = list(c(1, 2), 1, c(2, 3)),
                                  classes = classes),
               NA)
})

test_that("thq_pairs_by_class requires references to have named values if they are a vector and values are a list", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  values_list = list(s1 = c(A = 1, B = 2), s2 = c(A = 2), s3 = c(B = 3, C = 4))
  
  expect_error(thq_pairs_by_class(values = values_list,
                                  references = c(1, 2, 3),
                                  classes = classes),
               "name")
  expect_error(thq_pairs_by_class(values = values_list,
                                  references = c(A = 1, B = 2, C = 3),
                                  classes = classes),
               NA)
})

test_that("thq_pairs_by_class requires values to be named", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  # 'values' as a matrix
  expect_error(thq_pairs_by_class(values = matrix(c(1,2,3,4, 1,2,3,4), nrow = 2, byrow = TRUE,
                                                  dimnames = list(c("s1", "s2"))),
                                  references = references,
                                  classes = classes),
               "name")
  expect_error(thq_pairs_by_class(values = matrix(c(1,2,3,4, 1,2,3,4), nrow = 2, byrow = TRUE,
                                                  dimnames = list(c("s1", "s2"), letters[1:4])),
                                  references = references,
                                  classes = classes),
               NA)
  
  # 'values' as a list
  expect_error(thq_pairs_by_class(values = list(s1 = c(1, 2), s2 = c(2), s3 = c(3, 4)),
                                  references = references,
                                  classes = classes),
               "name")
  expect_error(thq_pairs_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4)),
                                  references = references,
                                  classes = classes),
               NA)
})

test_that("thq_pairs_by_class returns as many objects as there are classes in the values", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  expect_length(thq_pairs_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4)),
                                   references = references,
                                   classes = classes),
                5)
  
  expect_length(thq_pairs_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, C=4)),
                                   references = references,
                                   classes = classes),
                4)
})

test_that("thq_pairs_by_class returns a named list", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  results = thq_pairs_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4)),
                               references = references,
                               classes = classes)
  
  expect_type(results, "list")
  expect_named(results, c("C1", "C2", "C3", "C4", "C5"))
})

test_that("thq_pairs_by_class returns matrices or NULL objects", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  results = thq_pairs_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4)),
                               references = references,
                               classes = classes,
                               threshold = TRUE, alone = FALSE, ignore_zero = TRUE)
  
  expect_null(results[[1]])
  expect_null(results[[2]])
  expect_true(is.matrix(results[[3]]))
  expect_true(is.matrix(results[[4]]))
  expect_true(is.matrix(results[[5]]))
})


##### thq_by_group_by_class #####

test_that("thq_by_group_by_class requires that references have the same sizes as values if they are two lists", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  values_list = list(s1 = c(A = 1, B = 2), s2 = c(C = 2), s3 = c(B = 3, D = 4))
  
  expect_error(thq_by_group_by_class(values = values_list,
                                     references = list(1, 2, 3),
                                     classes = classes),
               "length")
  expect_error(thq_by_group_by_class(values = values_list,
                                     references = list(c(1, 2), 1, c(2, 3)),
                                     classes = classes),
               NA)
})

test_that("thq_by_group_by_class requires references to have named values if they are a vector and values are a list", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  values_list = list(s1 = c(A = 1, B = 2), s2 = c(A = 2), s3 = c(B = 3, C = 4))
  
  expect_error(thq_by_group_by_class(values = values_list,
                                     references = c(1, 2, 3),
                                     classes = classes),
               "name")
  expect_error(thq_by_group_by_class(values = values_list,
                                     references = c(A = 1, B = 2, C = 3),
                                     classes = classes),
               NA)
})

test_that("thq_by_group_by_class requires values to be named", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  # 'values' as a matrix
  expect_error(thq_by_group_by_class(values = matrix(c(1,2,3,4, 1,2,3,4), nrow = 2, byrow = TRUE,
                                                     dimnames = list(c("s1", "s2"))),
                                     references = references,
                                     classes = classes),
               "name")
  expect_error(thq_by_group_by_class(values = matrix(c(1,2,3,4, 1,2,3,4), nrow = 2, byrow = TRUE,
                                                     dimnames = list(c("s1", "s2"), letters[1:4])),
                                     references = references,
                                     classes = classes),
               NA)
  
  # 'values' as a list
  expect_error(thq_by_group_by_class(values = list(s1 = c(1, 2), s2 = c(2), s3 = c(3, 4)),
                                     references = references,
                                     classes = classes),
               "name")
  expect_error(thq_by_group_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4)),
                                     references = references,
                                     classes = classes),
               NA)
})

test_that("thq_by_group_by_class returns as many objects as there are classes in the values", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  expect_length(thq_by_group_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4)),
                                      references = references,
                                      classes = classes),
                5)
  
  expect_length(thq_by_group_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, C=4)),
                                      references = references,
                                      classes = classes),
                4)
})

test_that("thq_by_group_by_class returns a named list", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  results = thq_by_group_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4)),
                                  references = references,
                                  classes = classes)
  
  expect_type(results, "list")
  expect_named(results, c("C1", "C2", "C3", "C4", "C5"))
})

test_that("thq_by_group_by_class returns matrices", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  results = thq_by_group_by_class(values = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4)),
                                  references = references,
                                  classes = classes)
  
  expect_true(is.matrix(results[[1]]))
  expect_true(is.matrix(results[[2]]))
  expect_true(is.matrix(results[[3]]))
  expect_true(is.matrix(results[[4]]))
  expect_true(is.matrix(results[[5]]))
})


##### mcr_approach_by_class #####

test_that("mcr_approach_by_class accepts functions and characters as argument FUN", {
  pdf(NULL)
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  values_list = list(s1 = c(A = 1, B = 2), s2 = c(C = 2), s3 = c(B = 3, D = 4))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  expect_error(mcr_approach_by_class(values_list, references, classes,
                                     FUN = mcr_summary),
               NA)
  expect_error(mcr_approach_by_class(values_list, references, classes,
                                     FUN = "mcr_summary"),
               NA)
  expect_error(suppressMessages(suppressWarnings(
    mcr_approach_by_class(values_list, references, classes,
                          FUN = mcr_chart))),
    NA)
  expect_error(suppressMessages(suppressWarnings(
    mcr_approach_by_class(values_list, references, classes,
                          FUN = "mcr_chart"))),
    NA)
  expect_error(mcr_approach_by_class(values_list, references, classes,
                                     FUN = thq_pairs),
               NA)
  expect_error(mcr_approach_by_class(values_list, references, classes,
                                     FUN = "thq_pairs"),
               NA)
  expect_error(mcr_approach_by_class(values_list, references, classes,
                                     FUN = thq_by_group),
               NA)
  expect_error(mcr_approach_by_class(values_list, references, classes,
                                     FUN = "thq_by_group"),
               NA)
  dev.off()
})

test_that("mcr_approach_by_class returns the same results as calling the specific functions", {
  pdf(NULL)
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  values_list = list(s1 = c(A = 1, B = 2), s2 = c(C = 2), s3 = c(B = 3, D = 4))
  references = c(A = 1, B = 2, C = 3, D = 4)
  
  expect_identical(mcr_approach_by_class(values_list, references, classes,
                                         FUN = mcr_summary),
                   mcr_summary_by_class(values_list, references, classes))
  expect_true(all.equal(
    suppressMessages(suppressWarnings(
      mcr_approach_by_class(values_list, references, classes,
                            FUN = mcr_chart)
    )),
    suppressMessages(suppressWarnings(
      mcr_chart_by_class(values_list, references, classes)
    ))
  ))
  expect_identical(mcr_approach_by_class(values_list, references, classes,
                                         FUN = thq_pairs),
                   thq_pairs_by_class(values_list, references, classes))
  expect_identical(mcr_approach_by_class(values_list, references, classes,
                                         FUN = thq_by_group),
                   thq_by_group_by_class(values_list, references, classes))
  dev.off()
})



#### Subsets and reduction of sets ####

##### reduce_sets #####

test_that("reduce_sets requires that references have the same sizes as values if they are two lists", {
  expect_error(reduce_sets(values = list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                           references = list(1, 2, 3),
                           FUN = max),
               "length")
  expect_error(reduce_sets(values = list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                           references = list(c(1, 2), 1, c(2, 3)),
                           FUN = max),
               NA)
})

test_that("reduce_sets requires values to be named", {
  # 'values' as a vector
  expect_error(reduce_sets(values = c(1,2,3,4,5),
                           references = 1:5,
                           FUN = max),
               "name")
  expect_error(reduce_sets(values = c(a=1, b=2, c=3, d=4, e=5),
                           references = 1:5,
                           FUN = max),
               NA)
  
  # 'values' as a matrix
  expect_error(reduce_sets(values = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE),
                           references = c(a = 1, b = 1),
                           FUN = max),
               "name")
  expect_error(reduce_sets(values = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE,
                                           dimnames = list(NULL, letters[1:2])),
                           references = c(a = 1, b = 1),
                           FUN = max),
               NA)
  
  # 'values' as a list
  expect_error(reduce_sets(values = list(c(1, 1), c(1, 1), c(1, 1)),
                           references = c(a = 1, b = 1),
                           FUN = max),
               "name")
  expect_error(reduce_sets(values = list(c(a = 1, a = 1), c(b = 1, b = 1), c(a = 1, b = 1)),
                           references = c(a = 1, b = 1),
                           FUN = max),
               NA)
})

test_that("reduce_sets returns an additional variable if references are given", {
  values_vector = c(a = 1, b = 1)
  values_list = list(c(a = 1, a = 1), c(b = 1, b = 1), c(a = 1, b = 1))
  
  # 'references' as NULL
  expect_false(is.list(reduce_sets(values = values_vector, references = NULL, FUN = max)))
  
  # 'references' as a vector
  expect_type(reduce_sets(values = values_vector,
                          references = c(a=1, b=1),
                          FUN = max),
              "list")
  expect_length(reduce_sets(values = values_vector,
                            references = c(a=1, b=1),
                            FUN = max),
                2)
  expect_named(reduce_sets(values = values_vector,
                           references = c(a=1, b=1),
                           FUN = max),
               c("values", "references"))
  
  # 'references' as a list
  expect_type(reduce_sets(values = values_list,
                          references = list(c(1,1), c(1,1), c(1,1)),
                          FUN = max),
              "list")
  expect_length(reduce_sets(values = values_list,
                            references = list(c(1,1), c(1,1), c(1,1)),
                            FUN = max),
                2)
  expect_named(reduce_sets(values = values_list,
                           references = list(c(1,1), c(1,1), c(1,1)),
                           FUN = max),
               c("values", "references"))
})

test_that("reduce_sets ignores values equal to 0 according to the argument ignore_zero", {
  values_vector = c(b = 2, a = 2, b = 3, a = 1, a = 0)
  values_matrix = matrix(c(2,2,1,0, 1,1,3,2, 0,1,1,0),
                         nrow = 3, byrow = TRUE, dimnames = list(NULL, c("a","b","a","c")))
  values_list = list(s1 = c(b = 1, a = 1, a = 3, b = 0),
                     s2 = c(a = 2, c = 0),
                     s3 = c(b = 3, b = 7, b = 1, c = 1, c = 4))
  
  fun_b = function(x, b) { min(x) + max(x) + b }
  
  # 'values' as a vector
  expect_equal(reduce_sets(values = values_vector, FUN = min, ignore_zero = TRUE),
               c(b = 2, a = 1))
  expect_equal(reduce_sets(values = values_vector, FUN = min, ignore_zero = FALSE),
               c(b = 2, a = 0))
  
  # 'values' as a matrix
  expect_equal(reduce_sets(values = values_matrix, FUN = min, ignore_zero = TRUE),
               matrix(c(1,2,0, 1,1,2, 1,1,0),
                      nrow = 3, byrow = TRUE, dimnames = list(NULL, c("a","b","c"))))
  expect_equal(reduce_sets(values = values_matrix, FUN = min, ignore_zero = FALSE),
               matrix(c(1,2,0, 1,1,2, 0,1,0),
                      nrow = 3, byrow = TRUE, dimnames = list(NULL, c("a","b","c"))))
  
  # 'values' as a list
  expect_equal(reduce_sets(values = values_list, FUN = min, ignore_zero = TRUE),
               list(s1 = c(b = 1, a = 1),
                    s2 = c(a = 2),
                    s3 = c(b = 1, c = 1)))
  expect_equal(reduce_sets(values = values_list, FUN = min, ignore_zero = FALSE),
               list(s1 = c(b = 0, a = 1),
                    s2 = c(a = 2, c = 0),
                    s3 = c(b = 1, c = 1)))
  
})

test_that("reduce_sets reduces sets of values", {
  values_vector = c(b = 2, a = 2, b = 3, a = 1)
  values_matrix = matrix(c(2,2,1,0, 1,1,3,2, 0,1,1,0),
                         nrow = 3, byrow = TRUE, dimnames = list(NULL, c("a","b","a","c")))
  values_list = list(s1 = c(b = 1, a = 1, a = 3),
                     s2 = c(a = 2),
                     s3 = c(b = 3, b = 7, b = 1, c = 1, c = 4))
  
  fun_b = function(x, b) { min(x) + max(x) + b }
  
  # 'values' as a vector
  expect_equal(reduce_sets(values = values_vector, FUN = max),
               c(b = 3, a = 2))
  expect_equal(reduce_sets(values = values_vector, FUN = fun_b, b = 1),
               c(b = 6, a = 4))
  
  # 'values' as a matrix
  expect_equal(reduce_sets(values = values_matrix, FUN = max),
               matrix(c(2,2,0, 3,1,2, 1,1,0),
                      nrow = 3, byrow = TRUE, dimnames = list(NULL, c("a","b","c"))))
  expect_equal(reduce_sets(values = values_matrix, FUN = fun_b, b = 1),
               matrix(c(4,5,0, 5,3,5, 3,3,0),
                      nrow = 3, byrow = TRUE, dimnames = list(NULL, c("a","b","c"))))
  
  # 'values' as a list
  expect_equal(reduce_sets(values = values_list, FUN = max),
               list(s1 = c(b = 1, a = 3),
                    s2 = c(a = 2),
                    s3 = c(b = 7, c = 4)))
  expect_equal(reduce_sets(values = values_list, FUN = fun_b, b = 1),
               list(s1 = c(b = 3, a = 5),
                    s2 = c(a = 5),
                    s3 = c(b = 9, c = 6)))
})

test_that("reduce_sets reduces sets of references", {
  values = list(s1 = c(b = 1, a = 1, a = 3),
                s2 = c(a = 2),
                s3 = c(b = 3, b = 7, b = 1, c = 1, c = 4))
  references_vector = c(a = 3, b = 1, c = 2)
  
  # 'values' as a list and 'references' as a vector
  expect_identical(reduce_sets(values = values,
                               references = references_vector,
                               FUN = max)$references,
                   references_vector)
  
  # 'values' as a list and 'references' as a list
  expect_equal(reduce_sets(values = values,
                           references = list(c(1, 3, 3),
                                             c(3),
                                             c(1, 1, 1, 2, 2)),
                           FUN = max)$references,
               list(c(1, 3),
                    c(3),
                    c(1, 2)))
  
  # 'values' as a vector
  expect_equal(reduce_sets(values = c(b = 2, a = 2, b = 3, a = 1, a = 0),
                           references = c(2, 1, 2, 1, 1),
                           FUN = max)$references,
               c(2, 1))
  
  # 'values' as a matrix
  expect_equal(
    reduce_sets(
      values = matrix(c(2,2,1,0, 1,1,3,2, 0,1,1,0),
                      nrow = 3, byrow = TRUE, dimnames = list(NULL, c("a","b","a","c"))),
      references = c(1, 2, 1, 3),
      FUN = max)$references,
    c(1, 2, 3)
  )
})


##### subset_from_class #####

test_that("subset_from_class requires that references have the same sizes as values if they are two lists", {
  values_list = list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4))
  classes_list = list(a = c("C1","C2"), b = c("C2","C3"), c = c("C3", "C4"))
  
  expect_error(subset_from_class(values = values_list,
                                 references = list(1, 2, 3),
                                 classes = classes_list,
                                 class_name = "C3"),
               "length")
  expect_error(subset_from_class(values = values_list,
                                 references = list(c(1, 2), 1, c(2, 3)),
                                 classes = classes_list,
                                 class_name = "C3"),
               NA)
})

test_that("subset_from_class requires references to be named if they are a vector and values are a list", {
  values_list = list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4))
  classes_list = list(a = c("C1","C2"), b = c("C2","C3"), c = c("C3", "C4"))
  
  expect_error(subset_from_class(values = values_list,
                                 references = c(1, 2, 3),
                                 classes = classes_list,
                                 class_name = "C3"),
               "name")
  expect_error(subset_from_class(values = values_list,
                                 references = c(a = 1, b = 2, c = 3),
                                 classes = classes_list,
                                 class_name = "C3"),
               NA)
})

test_that("subset_from_class requires values to be named", {
  classes_list = list(a = c("C1","C2"), b = c("C2","C3"), c = c("C3", "C4"))
  
  # 'values' as a vector
  expect_error(subset_from_class(values = c(1, 1),
                                 references = c(a = 1, b = 1),
                                 classes = classes_list,
                                 class_name = "C3"),
               "name")
  expect_error(subset_from_class(values = c(a = 1, b = 1),
                                 references = c(a = 1, b = 1),
                                 classes = classes_list,
                                 class_name = "C3"),
               NA)
  
  # 'values' as a matrix
  expect_error(subset_from_class(values = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE),
                                 references = c(a = 1, b = 1),
                                 classes = classes_list,
                                 class_name = "C3"),
               "name")
  expect_error(subset_from_class(values = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE,
                                                 dimnames = list(NULL, letters[1:2])),
                                 references = c(a = 1, b = 1),
                                 classes = classes_list,
                                 class_name = "C3"),
               NA)
  
  # 'values' as a list
  expect_error(subset_from_class(values = list(c(1, 1), c(1, 1), c(1, 1)),
                                 references = c(a = 1, b = 1),
                                 classes = classes_list,
                                 class_name = "C3"),
               "name")
  expect_error(subset_from_class(values = list(c(a = 1, a = 1), c(b = 1, b = 1), c(a = 1, b = 1)),
                                 references = c(a = 1, b = 1),
                                 classes = classes_list,
                                 class_name = "C3"),
               NA)
})

test_that("subset_from_class returns an additional variable if references are given", {
  values_matrix = matrix(c(1,0, 1,1, 0,1), nrow = 3, byrow = TRUE,
                         dimnames = list(NULL, letters[1:2]))
  values_list = list(c(a = 1, a = 1), c(b = 1, b = 1), c(a = 1, b = 1))
  classes_list = list(a = c("C1","C2"), b = c("C2","C3"), c = c("C3", "C4"))
  
  # 'references' as NULL
  expect_false(is.list(subset_from_class(values = values_matrix,
                                         references = NULL,
                                         classes = classes_list,
                                         class_name = "C3")))
  
  # 'references' as a vector
  expect_type(subset_from_class(values = values_matrix,
                                references = c(a = 1, b = 1),
                                classes = classes_list,
                                class_name = "C3"), 
              "list")
  expect_length(subset_from_class(values = values_matrix,
                                  references = c(a = 1, b = 1),
                                  classes = classes_list,
                                  class_name = "C3"), 
                2)
  expect_named(subset_from_class(values = values_matrix,
                                 references = c(a = 1, b = 1),
                                 classes = classes_list,
                           class_name = "C3"),
               c("values", "references"))
  
  # 'references' as a list
  expect_type(subset_from_class(values = values_list,
                                references = list(c(1, 1), c(1, 1), c(1, 1)),
                                classes = classes_list,
                                class_name = "C3"), 
              "list")
  expect_length(subset_from_class(values = values_list,
                                  references = list(c(1, 1), c(1, 1), c(1, 1)),
                                  classes = classes_list,
                                  class_name = "C3"), 
                2)
  expect_named(subset_from_class(values = values_list,
                                 references = list(c(1, 1), c(1, 1), c(1, 1)),
                                 classes = classes_list,
                                 class_name = "C3"), 
               c("values", "references"))
})

test_that("subset_from_class extracts the subset of values corresponding to the given class", {
  classes_list = list(a = c("C1","C2"), b = c("C2","C3"), c = c("C3", "C4"), d = "C5")
  values_vector = c(a = 1, b = 1, c = 1)
  values_matrix = matrix(c(1,1,1, 1,0,0, 1,1,0),
                         nrow = 3, byrow = TRUE, dimnames = list(c("s1", "s2", "s3"), letters[1:3]))
  values_list = list(s1 = c(a = 1, b = 1, c = 1),
                     s2 = c(a = 1),
                     s3 = c(a = 1, b = 1))
  
  # 'values' as a vector
  expect_equal(subset_from_class(values = values_vector,
                                 classes = classes_list,
                                 class_name = "C3"),
               c(b = 1, c = 1))
  expect_equal(subset_from_class(values = values_vector,
                                 classes = classes_list,
                                 class_name = "C4"),
               c(c = 1))
  expect_equal(subset_from_class(values = values_vector,
                                 classes = classes_list,
                                 class_name = "C5"),
               stats::setNames(numeric(0), character(0)))
  
  # 'values' as a matrix
  expect_equal(subset_from_class(values = values_matrix,
                                 classes = classes_list,
                                 class_name = "C3"),
               matrix(c(1,1, 0,0, 1,0),
                      nrow = 3, byrow = TRUE, dimnames = list(c("s1", "s2", "s3"), c("b", "c"))))
  expect_equal(subset_from_class(values = values_matrix,
                                 classes = classes_list,
                                 class_name = "C4"),
               matrix(c(1,0,0),
                      nrow = 3, byrow = TRUE, dimnames = list(c("s1", "s2", "s3"), "c")))
  expect_equal(subset_from_class(values = values_matrix,
                                 classes = classes_list,
                                 class_name = "C5"),
               matrix(numeric(0),
                      ncol = 0, nrow = nrow(values_matrix), dimnames = list(c("s1", "s2", "s3"))))
  
  # 'values' as a list
  expect_equal(subset_from_class(values = values_list,
                                 classes = classes_list,
                                 class_name = "C3"),
               list(s1 = c(b = 1, c = 1),
                    s3 = c(b = 1)))
  expect_equal(subset_from_class(values = values_list,
                                 classes = classes_list,
                                 class_name = "C4"),
               list(s1 = c(c = 1)))
  expect_equal(subset_from_class(values = values_list,
                                 classes = classes_list,
                                 class_name = "C5"),
               stats::setNames(list(), character(0)))
})

test_that("subset_from_class does not ignore duplicate value names", {
  classes_list = list(a = c("C1","C2"), b = c("C2","C3"))
  values_vector = c(a = 1, b = 1, a = 2)
  values_matrix = matrix(c(1,1,0, 1,1,1),
                         nrow = 2, byrow = TRUE, dimnames = list(c("s1", "s2"), c("a", "b", "a")))
  values_list = list(s1 = c(a = 1, b = 1),
                     s2 = c(a = 1, b = 1, a = 2))
  
  # 'values' as a vector
  expect_equal(subset_from_class(values = values_vector,
                                 classes = classes_list,
                                 class_name = "C1"),
               c(a = 1, a = 2))
  
  # 'values' as a matrix
  expect_equal(subset_from_class(values = values_matrix,
                                 classes = classes_list,
                                 class_name = "C1"),
               matrix(c(1,0, 1,1),
                      nrow = 2, byrow = TRUE, dimnames = list(c("s1", "s2"), c("a", "a"))))
  
  # 'values' as a list
  expect_equal(subset_from_class(values = values_list,
                                 classes = classes_list,
                                 class_name = "C1"),
               list(s1 = c(a = 1),
                    s2 = c(a = 1, a = 2)))
})

test_that("subset_from_class extracts the subset of references corresponding to the given class", {
  classes_list = list(a = c("C1","C2"), b = c("C2","C3"), c = c("C3", "C4"), d = "C5")
  values_list = list(s1 = c(a = 1, b = 1, c = 1),
                     s2 = c(a = 1),
                     s3 = c(a = 1, b = 1))
  references_vector = c(a = 1, b = 2, c = 3, d = 4)
  references_list = list(c(1, 2, 3),
                         1,
                         c(1, 2))
  
  # 'references' as a vector
  expect_equal(subset_from_class(values = values_list,
                                 references = references_vector,
                                 classes = classes_list,
                                 class_name = "C3")$references,
               c(b = 2, c = 3))
  expect_equal(subset_from_class(values = values_list,
                                 references = references_vector,
                                 classes = classes_list,
                                 class_name = "C4")$references,
               c(c = 3))
  expect_equal(subset_from_class(values = values_list,
                                 references = references_vector,
                                 classes = classes_list,
                                 class_name = "C5")$references,
               c(d = 4))
  
  # 'references' as a list
  expect_equal(subset_from_class(values = values_list,
                                 references = references_list,
                                 classes = classes_list,
                                 class_name = "C3")$references,
               list(c(2, 3),
                    2))
  expect_equal(subset_from_class(values = values_list,
                                 references = references_list,
                                 classes = classes_list,
                                 class_name = "C4")$references,
               list(3))
  expect_equal(subset_from_class(values = values_list,
                                 references = references_list,
                                 classes = classes_list,
                                 class_name = "C5")$references,
               list())
})

test_that("subset_from_class returns an identical result whatever the structure of classes", {
  # 'classes' as a list or a matrix
  classes_list = list(a = c("C1","C2"), b = c("C2","C3"), c = c("C3", "C4"), d = "C5")
  classes_matrix = matrix(c(TRUE, TRUE, FALSE, FALSE, FALSE,
                            FALSE, TRUE, TRUE, FALSE, FALSE,
                            FALSE, FALSE, TRUE, TRUE, FALSE,
                            FALSE, FALSE, FALSE, FALSE, TRUE),
                          ncol = 5, byrow = TRUE, dimnames = list(letters[1:4], paste0("C", 1:5)))
  
  values_list = list(s1 = c(a = 1, b = 1, c = 1),
                     s2 = c(a = 1, b = 1),
                     s3 = c(a = 1))
  references_vector = c(a = 1, b = 1, c = 1)
  
  expect_identical(subset_from_class(values = values_list,
                                     references = references_vector,
                                     classes = classes_list,
                                     class_name = "C3"),
                   subset_from_class(values = values_list,
                                     references = references_vector,
                                     classes = classes_matrix,
                                     class_name = "C3"))
  
  expect_identical(subset_from_class(values = values_list,
                                     references = references_vector,
                                     classes = classes_list,
                                     class_name = "C4"),
                   subset_from_class(values = values_list,
                                     references = references_vector,
                                     classes = classes_matrix,
                                     class_name = "C4"))
  
  expect_identical(subset_from_class(values = values_list,
                                     references = references_vector,
                                     classes = classes_list,
                                     class_name = "C5"),
                   subset_from_class(values = values_list,
                                     references = references_vector,
                                     classes = classes_matrix,
                                     class_name = "C5"))
})

test_that("subset_from_class returns a list if values are a list", {
  classes = list(A = c("C2", "C4", "C5"),
                 B = c("C3", "C4", "C5"),
                 C = c("C5"),
                 D = c("C1", "C2", "C3", "C4", "C5"))
  values_list = list(s1 = c(A=1, B=2), s2 = c(A=2), s3 = c(B=3, D=4))
  
  expect_equal(subset_from_class(values = values_list,
                                 classes = classes,
                                 class_name = "C1"),
               list(s3 = c(D = 4)))
  expect_equal(subset_from_class(values = values_list,
                                 classes = classes,
                                 class_name = "C2"),
               list(s1 = c(A = 1),
                    s2 = c(A = 2),
                    s3 = c(D = 4)))
})


