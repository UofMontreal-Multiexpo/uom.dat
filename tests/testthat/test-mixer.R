## Context: Maximum Cumulative Ratio approach


#### Maximum Cumulative Ratio approach - main indicators ####

##### hazard_quotient #####

test_that("hazard_quotient returns the right data structure", {
  # Vector -> Vector
  expect_true(is.vector(hazard_quotient(1:5, 1:5)))
  
  # Matrix -> Matrix
  expect_true(is.matrix(hazard_quotient(matrix(1:10, ncol = 2), 1:5)))
  expect_true(is.matrix(hazard_quotient(matrix(1:5,  ncol = 1), 1:5)))
})

test_that("hazard_quotient returns a named object if the given one is named", {
  # Vector
  expect_named(hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), 1:5),
               letters[1:5])
  
  # Matrix
  expect_equal(rownames(hazard_quotient(matrix(1:10, ncol = 2, dimnames = list(letters[1:5])), 1:5)),
               letters[1:5])
  expect_equal(colnames(hazard_quotient(matrix(1:10, ncol = 2, dimnames = list(NULL, c("s1", "s2"))), 1:5)),
               c("s1", "s2"))
})

test_that("hazard_quotient returns an unnamed object if the given one is unnamed", {
  # Vector
  expect_named(hazard_quotient(1:5, 1:5), NULL)
  
  # Matrix
  expect_null(rownames(hazard_quotient(matrix(1:10, ncol = 2), 1:5)))
  expect_null(colnames(hazard_quotient(matrix(1:10, ncol = 2), 1:5)))
})

test_that("hazard_quotient computes the hazard quotients", {
  # Vector
  expect_equal(hazard_quotient(1:5, 1:5),          c(1,1,1,1,1))
  expect_equal(hazard_quotient(1:5, c(1,1,1,1,1)), c(1,2,3,4,5))
  
  # Matrix
  expect_equal(hazard_quotient(matrix(c(1,2,3,4,5,10,10,9,10,10), ncol = 2), c(1,2,3,4,5)),
               matrix(c(1,1,1,1,1, 10,5,3,2.5,2), ncol = 2))
  expect_equal(hazard_quotient(matrix(1:10, ncol = 2), c(1,1,1,1,1)),
               matrix(c(1,2,3,4,5, 6,7,8,9,10), ncol = 2))
})


##### hazard_index #####

test_that("hazard_index returns the right number of values", {
  # 'values' as a vector
  expect_length(hazard_index(1:5, 1:5), 1)
  
  # 'values' as a matrix
  expect_length(hazard_index(matrix(1:10, ncol = 2), 1:5), 2)
  expect_length(hazard_index(matrix(1:5,  ncol = 1), 1:5), 1)
  
  # 'hq' as a vector
  expect_length(hazard_index(hq = 1:5), 1)
  
  # 'hq' as a matrix
  expect_length(hazard_index(hq = matrix(1:10, ncol = 2)), 2)
  expect_length(hazard_index(hq = matrix(1:5,  ncol = 1)), 1)
})

test_that("hazard_index returns an unnamed object if a vector is given", {
  # 'values' as a vector
  expect_named(hazard_index(c(a=1, b=2, c=3, d=4, e=5), 1:5), NULL)
  
  # 'hq' as a vector
  expect_named(hazard_index(hq = c(a=1, b=1, c=1, d=1, e=1)), NULL)
})

test_that("hazard_index returns an unnamed object if an unnamed matrix is given", {
  # 'values' as an unnamed matrix
  expect_named(hazard_index(matrix(1:10, ncol = 2), 1:5), NULL)
  
  # 'hq' as an unnamed matrix
  expect_named(hazard_index(hq = matrix(1:10, ncol = 2)), NULL)
})

test_that("hazard_index returns a named object if a named matrix is given", {
  # 'values' as a named matrix
  expect_named(hazard_index(matrix(1:10, ncol = 2, dimnames = list(NULL, c("s1", "s2"))), 1:5),
               c("s1", "s2"))
  
  # 'hq' as a named matrix
  expect_named(hazard_index(hq = matrix(1:10, ncol = 2, dimnames = list(NULL, c("s1", "s2"))), 1:5),
               c("s1", "s2"))
})

test_that("hazard_index computes the hazard indexes", {
  # 'values' as a vector
  expect_equal(hazard_index(1:5, 1:5),          5)
  expect_equal(hazard_index(1:5, c(1,1,1,1,1)), 15)
  
  # 'values' as a matrix
  expect_equal(hazard_index(matrix(c(1,2,3,4,5, 10,10,9,10,10),  ncol = 2), 1:5), c(5, 22.5))
  expect_equal(hazard_index(matrix(1:10, ncol = 2), c(1,1,1,1,1)),                c(15, 40))
  
  # 'hq' as a vector
  expect_equal(hazard_index(hq = c(1,1,1,1,1)), 5)
  expect_equal(hazard_index(hq = 1:5),          15)
  
  # 'hq' as a matrix
  expect_equal(hazard_index(hq = matrix(c(1,2,3,4,5, 10,10,9,10,10),  ncol = 2)), c(15, 49))
  expect_equal(hazard_index(hq = matrix(1:10, ncol = 2)),                         c(15, 40))
})

test_that("hazard_index returns an identical result whatever the chosen usage", {
  values_vector = 1:5
  values_matrix = matrix(1:10, ncol = 2)
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
  expect_length(maximum_hazard_quotient(matrix(1:10, ncol = 2), 1:5), 2)
  expect_length(maximum_hazard_quotient(matrix(1:5,  ncol = 1), 1:5), 1)
  
  # 'hq' as a vector
  expect_length(maximum_hazard_quotient(hq = 1:5), 1)
  
  # 'hq' as a matrix
  expect_length(maximum_hazard_quotient(hq = matrix(1:10, ncol = 2)), 2)
  expect_length(maximum_hazard_quotient(hq = matrix(1:5,  ncol = 1)), 1)
  
})

test_that("maximum_hazard_quotient returns an unnamed object if a vector is given", {
  # 'values' as a vector
  expect_named(maximum_hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), 1:5), NULL)
  
  # 'hq' as a vector
  expect_named(maximum_hazard_quotient(hq = c(a=1, b=1, c=1, d=1, e=1)), NULL)
})

test_that("maximum_hazard_quotient returns an unnamed object if an unnamed matrix is given", {
  # 'values' as an unnamed matrix
  expect_named(maximum_hazard_quotient(matrix(1:10, ncol = 2), 1:5), NULL)
  
  # 'hq' as an unnamed matrix
  expect_named(maximum_hazard_quotient(hq = matrix(1:10, ncol = 2)), NULL)
})

test_that("maximum_hazard_quotient returns a named object if a named matrix is given", {
  # 'values' as a named matrix
  expect_named(maximum_hazard_quotient(matrix(1:10, ncol = 2, dimnames = list(NULL, c("s1", "s2"))), 1:5),
               c("s1", "s2"))
  
  # 'hq' as a named matrix
  expect_named(maximum_hazard_quotient(hq = matrix(1:10, ncol = 2, dimnames = list(NULL, c("s1", "s2"))), 1:5),
               c("s1", "s2"))
})

test_that("maximum_hazard_quotient find the maximum hazard quotient", {
  # 'values' as a vector
  expect_equal(maximum_hazard_quotient(1:5, 1:5),          1)
  expect_equal(maximum_hazard_quotient(1:5, c(1,1,1,1,1)), 5)
  
  # 'values' as a matrix
  expect_equal(maximum_hazard_quotient(matrix(c(1,2,3,4,5, 10,10,9,10,10),  ncol = 2), 1:5), c(1, 10))
  expect_equal(maximum_hazard_quotient(matrix(1:10, ncol = 2), c(1,1,1,1,1)),                c(5, 10))
  
  # 'hq' as a vector
  expect_equal(maximum_hazard_quotient(hq = c(1,1,1,1,1)), 1)
  expect_equal(maximum_hazard_quotient(hq = 1:5),          5)
  
  # 'hq' as a matrix
  expect_equal(maximum_hazard_quotient(hq = matrix(c(1,2,3,4,5, 10,10,9,10,10),  ncol = 2)), c(5, 10))
  expect_equal(maximum_hazard_quotient(hq = matrix(1:10, ncol = 2)),                         c(5, 10))
})

test_that("maximum_hazard_quotient returns an identical result whatever the chosen usage", {
  values_vector = 1:5
  values_matrix = matrix(1:10, ncol = 2)
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
  expect_length(maximum_cumulative_ratio(matrix(1:10, ncol = 2), 1:5), 2)
  expect_length(maximum_cumulative_ratio(matrix(1:5,  ncol = 1), 1:5), 1)
  
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
  expect_named(maximum_cumulative_ratio(matrix(1:10, ncol = 2), 1:5), NULL)
})

test_that("maximum_cumulative_ratio returns a named object if a named matrix is given", {
  # 'values' as a named matrix
  expect_named(maximum_cumulative_ratio(matrix(1:10, ncol = 2, dimnames = list(NULL, c("s1", "s2"))), 1:5),
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
  expect_equal(maximum_cumulative_ratio(matrix(c(1,2,3,4,5, 10,10,9,10,10),  ncol = 2), 1:5), c(5, 2.25))
  expect_equal(maximum_cumulative_ratio(matrix(1:10, ncol = 2), c(1,1,1,1,1)),                c(3, 4))
  
  # 'hi' and 'mhq'
  expect_equal(maximum_cumulative_ratio(hi = c(1,1,1,1,1), mhq = 1:5), c(1,0.5,1/3,0.25,0.2))
  expect_equal(maximum_cumulative_ratio(hi = 1:5, mhq = c(1,1,1,1,1)), c(1,2,3,4,5))
})

test_that("maximum_cumulative_ratio returns an identical result whatever the chosen usage", {
  values_vector = 1:5
  values_matrix = matrix(1:10, ncol = 2)
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
  expect_length(missed_toxicity(matrix(1:10, ncol = 2), 1:5), 2)
  expect_length(missed_toxicity(matrix(1:5,  ncol = 1), 1:5), 1)
  
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
  expect_named(missed_toxicity(matrix(1:10, ncol = 2), 1:5), NULL)
})

test_that("missed_toxicity returns a named object if a named matrix is given", {
  # 'values' as a named matrix
  expect_named(missed_toxicity(matrix(1:10, ncol = 2, dimnames = list(NULL, c("s1", "s2"))), 1:5),
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
  expect_equal(missed_toxicity(matrix(c(1,2,3,4,5), ncol = 1), 1:5),  0.8)
  expect_equal(missed_toxicity(matrix(1:10, ncol = 2), c(1,1,1,1,1)), c(2/3, 0.75))
  
  # 'mcr'
  expect_equal(missed_toxicity(mcr = 1),   0)
  expect_equal(missed_toxicity(mcr = 1:5), c(0,0.5,2/3,0.75,0.8))
})

test_that("missed_toxicity returns an identical result whatever the chosen usage", {
  values_vector = 1:5
  values_matrix = matrix(1:10, ncol = 2)
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
  expect_length(reciprocal_of_mcr(matrix(1:10, ncol = 2), 1:5), 2)
  expect_length(reciprocal_of_mcr(matrix(1:5,  ncol = 1), 1:5), 1)
  
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
  expect_named(reciprocal_of_mcr(matrix(1:10, ncol = 2), 1:5), NULL)
})

test_that("reciprocal_of_mcr returns a named object if a named matrix is given", {
  # 'values' as a named matrix
  expect_named(reciprocal_of_mcr(matrix(1:10, ncol = 2, dimnames = list(NULL, c("s1", "s2"))), 1:5),
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
  expect_equal(reciprocal_of_mcr(matrix(c(1,2,3,4,5), ncol = 1), 1:5),  0.2)
  expect_equal(reciprocal_of_mcr(matrix(1:10, ncol = 2), c(1,1,1,1,1)), c(1/3, 0.25))
  
  # 'hi' and 'mhq'
  expect_equal(reciprocal_of_mcr(hi = c(1,1,1,1,1), mhq = 1:5), c(1,2,3,4,5))
  expect_equal(reciprocal_of_mcr(hi = 1:5, mhq = c(1,1,1,1,1)), c(1,0.5,1/3,0.25,0.2))
  
  # 'mcr'
  expect_equal(reciprocal_of_mcr(mcr = 1),   1)
  expect_equal(reciprocal_of_mcr(mcr = 1:5), c(1,0.5,1/3,0.25,0.2))
})

test_that("reciprocal_of_mcr returns an identical result whatever the chosen usage", {
  values_vector = 1:5
  values_matrix = matrix(1:10, ncol = 2)
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

test_that("top_hazard_quotient returns at most as many top hazard quotients as there are hazard quotients", {
  # 'values' as a vector
  expect_length(top_hazard_quotient(1:5, 1:5, k = 500), 5)
  
  # 'values' as a matrix
  expect_equal(lengths(top_hazard_quotient(matrix(1:10, ncol = 2), 1:5, k = 500)), c(5,5))
  expect_equal(lengths(top_hazard_quotient(matrix(1:5,  ncol = 1), 1:5, k = 500)), 5)
  
  # 'hq' as a vector
  expect_length(top_hazard_quotient(hq = 1:5, k = 500), 5)
  
  # 'hq' as a matrix
  expect_equal(lengths(top_hazard_quotient(hq = matrix(1:10, ncol = 2), k = 500)), c(5,5))
  expect_equal(lengths(top_hazard_quotient(hq = matrix(1:5,  ncol = 1), k = 500)), 5)
})

test_that("top_hazard_quotient returns, by default, as many top hazard quotients as the integer part of the maximum cumulative ratio", {
  # 'values' as a vector
  expect_length(top_hazard_quotient(1:5, 1:5), 5)
  
  # 'values' as a matrix
  expect_equal(lengths(top_hazard_quotient(matrix(1:10, ncol = 2), 1:5)), c(5,2))
  expect_equal(lengths(top_hazard_quotient(matrix(1:5,  ncol = 1), 1:5)), 5)
  
  # 'hq' as a vector
  expect_length(top_hazard_quotient(hq = 1:5), 3)
  
  # 'hq' as a matrix
  expect_equal(lengths(top_hazard_quotient(hq = matrix(1:10, ncol = 2))), c(3,4))
  expect_equal(lengths(top_hazard_quotient(hq = matrix(1:5,  ncol = 1))), 3)
})

test_that("top_hazard_quotient does not allow to find less than 1 top hazard quotient", {
  expect_error(top_hazard_quotient(1:5, 1:5,                    k = 0))
  expect_error(top_hazard_quotient(matrix(1:10, ncol = 2), 1:5, k = -1))
  expect_error(top_hazard_quotient(hq = 1:5,                    k = -50))
  expect_error(top_hazard_quotient(hq = matrix(1:10, ncol = 2), k = -Inf))
})

test_that("top_hazard_quotient returns as many top hazard quotients as requested", {
  # 'values' as a vector
  expect_length(top_hazard_quotient(1:5, 1:5, k = 3), 3)
  
  # 'values' as a matrix
  expect_equal(lengths(top_hazard_quotient(matrix(1:10, ncol = 2), 1:5, k = 3)), c(3,3))
  expect_equal(lengths(top_hazard_quotient(matrix(1:5,  ncol = 1), 1:5, k = 3)), 3)
  
  # 'hq' as a vector
  expect_length(top_hazard_quotient(hq = 1:5, k = 3), 3)
  
  # 'hq' as a matrix
  expect_equal(lengths(top_hazard_quotient(hq = matrix(1:10, ncol = 2), k = 3)), c(3,3))
  expect_equal(lengths(top_hazard_quotient(hq = matrix(1:5,  ncol = 1))), 3)
})

test_that("top_hazard_quotient returns the right data structure", {
  # 'values' as a vector
  expect_true(is.vector(top_hazard_quotient(1:5, 1:5)))
  expect_false(is.list(top_hazard_quotient(1:5, 1:5)))
  
  # 'values' as a matrix
  expect_true(is.list(top_hazard_quotient(matrix(1:10, ncol = 2), 1:5)))
  expect_true(is.list(top_hazard_quotient(matrix(1:5,  ncol = 1), 1:5)))
  
  # 'hq' as a vector
  expect_true(is.vector(top_hazard_quotient(hq = 1:5)))
  expect_false(is.list(top_hazard_quotient(hq = 1:5)))
  
  # 'hq' as a matrix
  expect_true(is.list(top_hazard_quotient(hq = matrix(1:10, ncol = 2))))
  expect_true(is.list(top_hazard_quotient(hq = matrix(1:5,  ncol = 1))))
})

test_that("top_hazard_quotient returns as many sets of top hazard quotients as there are sets of values or of hq", {
  # 'values' as a matrix
  expect_length(top_hazard_quotient(matrix(1:10, ncol = 2), 1:5), 2)
  expect_length(top_hazard_quotient(matrix(1:5,  ncol = 1), 1:5), 1)
  
  # 'hq' as a matrix
  expect_length(top_hazard_quotient(hq = matrix(1:10, ncol = 2)), 2)
  expect_length(top_hazard_quotient(hq = matrix(1:5,  ncol = 1)), 1)
})

test_that("top_hazard_quotient returns named values if the given values or hq are named", {
  # 'values' as a vector
  expect_named(top_hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), 1:5))
  
  # 'values' as a matrix
  expect_named(unlist(top_hazard_quotient(matrix(1:10, ncol = 2, dimnames = list(letters[1:5])), 1:5)))
  expect_named(unlist(top_hazard_quotient(matrix(1:5,  ncol = 1, dimnames = list(letters[1:5])), 1:5)))
  
  # 'hq' as a vector
  expect_named(top_hazard_quotient(hq = c(a=1, b=2, c=3, d=4, e=5)))
  
  # 'hq' as a matrix
  expect_named(unlist(top_hazard_quotient(hq = matrix(1:10, ncol = 2, dimnames = list(letters[1:5])))))
  expect_named(unlist(top_hazard_quotient(hq = matrix(1:5,  ncol = 1, dimnames = list(letters[1:5])))))
})

test_that("top_hazard_quotient returns named sets of values if the given values or hq matrix is named", {
  # 'values' as a matrix
  expect_named(top_hazard_quotient(matrix(1:10, ncol = 2, dimnames = list(NULL, c("s1", "s2"))), 1:5),
               c("s1", "s2"))
  expect_named(top_hazard_quotient(matrix(1:5,  ncol = 1, dimnames = list(NULL, "s1")), 1:5),
               "s1")
  
  # 'hq' as a matrix
  expect_named(top_hazard_quotient(hq = matrix(1:10, ncol = 2, dimnames = list(NULL, c("s1", "s2")))),
               c("s1", "s2"))
  expect_named(top_hazard_quotient(hq = matrix(1:5,  ncol = 1, dimnames = list(NULL, "s1"))),
               "s1")
})

test_that("top_hazard_quotient returns decreasing values", {
  is_desc_sorted = function(x) !is.unsorted(rev(x))
  are_all_desc_sorted = function(x) all(sapply(x, is_desc_sorted))
  
  # 'values' as a vector
  expect_true(is_desc_sorted(top_hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), 1:5)))
  
  # 'values' as a matrix
  expect_true(are_all_desc_sorted(top_hazard_quotient(matrix(1:10, ncol = 2, dimnames = list(letters[1:5])), 1:5)))
  expect_true(are_all_desc_sorted(top_hazard_quotient(matrix(1:5,  ncol = 1, dimnames = list(letters[1:5])), 1:5)))
  
  # 'hq' as a vector
  expect_true(is_desc_sorted(top_hazard_quotient(hq = c(a=1, b=2, c=3, d=4, e=5))))
  
  # 'hq' as a matrix
  expect_true(are_all_desc_sorted(top_hazard_quotient(hq = matrix(1:10, ncol = 2, dimnames = list(letters[1:5])))))
  expect_true(are_all_desc_sorted(top_hazard_quotient(hq = matrix(1:5,  ncol = 1, dimnames = list(letters[1:5])))))
})

test_that("top_hazard_quotient finds the top hazard quotients", {
  # 'values' as a vector
  expect_equal(top_hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), 1:5),
               c(a=1, b=1, c=1, d=1, e=1))
  expect_equal(top_hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), c(1,1,1,1,1)),
               c(e=5, d=4, c=3))
  
  # 'values' as a matrix
  expect_equal(top_hazard_quotient(matrix(1:10, ncol = 2, dimnames = list(letters[1:5], c("s1", "s2"))), c(1,1,1,1,1)),
               list(s1 = c(e=5, d=4, c=3),
                    s2 = c(e=10, d=9, c=8, b=7)))
  expect_equal(top_hazard_quotient(matrix(1:5, ncol = 1, dimnames = list(letters[1:5], "s1")), 1:5),
               list(s1 = c(a=1, b=1, c=1, d=1, e=1)))
  
  # 'hq' as a vector
  expect_equal(top_hazard_quotient(hq = c(a=1, b=1, c=1, d=1, e=1)),
               c(a=1, b=1, c=1, d=1, e=1))
  expect_equal(top_hazard_quotient(hq = c(a=1, b=2, c=3, d=4, e=5)),
               c(e=5, d=4, c=3))
  
  # 'hq' as a matrix
  expect_equal(top_hazard_quotient(hq = matrix(1:10, ncol = 2, dimnames = list(letters[1:5], c("s1", "s2")))),
               list(s1 = c(e=5, d=4, c=3),
                    s2 = c(e=10, d=9, c=8, b=7)))
  expect_equal(top_hazard_quotient(hq = matrix(1:5, ncol = 1, dimnames = list(letters[1:5], "s1"))),
               list(s1 = c(e=5, d=4, c=3)))
})

test_that("top_hazard_quotient returns an identical result whatever the chosen usage", {
  values_vector = c(a=1, b=2, c=3, d=4, e=5)
  values_matrix = matrix(1:10, ncol = 2, dimnames = list(letters[1:5], c("s1", "s2")))
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
  expect_length(classify_mixture(matrix(1:10, ncol = 2), 1:5), 2)
  expect_length(classify_mixture(matrix(1:5,  ncol = 1), 1:5), 1)
  
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
  expect_named(classify_mixture(matrix(1:10, ncol = 2), 1:5), NULL)
})

test_that("classify_mixture returns a named object if a named matrix is given", {
  # 'values' as a named matrix
  expect_named(classify_mixture(matrix(1:10, ncol = 2, dimnames = list(NULL, c("s1", "s2"))), 1:5),
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
  # Therefor, limit values must be applied in the following order: mhq, hi, mcr
  
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
                                         0.9, 0.9), nrow = 2), c(1,1)),
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
})

test_that("classify_mixture returns an identical result whatever the chosen usage", {
  values_matrix = matrix(c(1,   1,
                           0.5, 0.5,
                           0.5, 0.8,
                           0.9, 0.9), nrow = 2)
  references = c(1,1)
  
  expect_identical(classify_mixture(values_matrix[, 1], references),
                   classify_mixture(hi  = hazard_index(values_matrix[, 1], references),
                                    mhq = maximum_hazard_quotient(values_matrix[, 1], references)))
  expect_identical(classify_mixture(values_matrix[, 1], references),
                   classify_mixture(hi  = hazard_index(values_matrix[, 1], references),
                                    mhq = maximum_hazard_quotient(values_matrix[, 1], references),
                                    mcr = maximum_cumulative_ratio(values_matrix[, 1], references)))
  
  expect_identical(classify_mixture(values_matrix[, 2], references),
                   classify_mixture(hi  = hazard_index(values_matrix[, 2], references),
                                    mhq = maximum_hazard_quotient(values_matrix[, 2], references)))
  expect_identical(classify_mixture(values_matrix[, 2], references),
                   classify_mixture(hi  = hazard_index(values_matrix[, 2], references),
                                    mhq = maximum_hazard_quotient(values_matrix[, 2], references),
                                    mcr = maximum_cumulative_ratio(values_matrix[, 2], references)))
  
  expect_identical(classify_mixture(values_matrix[, 3], references),
                   classify_mixture(hi  = hazard_index(values_matrix[, 3], references),
                                    mhq = maximum_hazard_quotient(values_matrix[, 3], references)))
  expect_identical(classify_mixture(values_matrix[, 3], references),
                   classify_mixture(hi  = hazard_index(values_matrix[, 3], references),
                                    mhq = maximum_hazard_quotient(values_matrix[, 3], references),
                                    mcr = maximum_cumulative_ratio(values_matrix[, 3], references)))
  
  expect_identical(classify_mixture(values_matrix[, 4], references),
                   classify_mixture(hi  = hazard_index(values_matrix[, 4], references),
                                    mhq = maximum_hazard_quotient(values_matrix[, 4], references)))
  expect_identical(classify_mixture(values_matrix[, 4], references),
                   classify_mixture(hi  = hazard_index(values_matrix[, 4], references),
                                    mhq = maximum_hazard_quotient(values_matrix[, 4], references),
                                    mcr = maximum_cumulative_ratio(values_matrix[, 4], references)))
  
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
                           list(1, 2, 3)))
  expect_silent(mcr_summary(list(s1 = c(1, 2), s2 = c(2), s3 = c(3, 4)),
                           list(c(1, 2), 1, c(2, 3))))
})

test_that("mcr_summary requires values and references to have named values if they are a list and a vector", {
  expect_error(mcr_summary(list(s1 = c(1, 2), s2 = c(2), s3 = c(3, 4)),
                           c(a = 1, b = 2, c = 3)))
  expect_error(mcr_summary(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                           c(1, 2, 3)))
  expect_silent(mcr_summary(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                            c(a = 1, b = 2, c = 3)))
})

test_that("mcr_summary returns the right data structure", {
  # Vector -> List
  expect_true(is.list(mcr_summary(c(a=1, b=2, c=3, d=4, e=5), 1:5)))
  
  # Matrix (several or a singlet set of values) -> Data frame
  expect_true(is.data.frame(mcr_summary(matrix(1:15, ncol = 3,
                                               dimnames = list(letters[1:5], c("s1", "s2", "s3"))), 1:5)))
  expect_true(is.data.frame(mcr_summary(matrix(1:5, ncol = 1, dimnames = list(letters[1:5], "s1")), 1:5)))
  
  # List (several or a single set of values) -> Data frame
  expect_true(is.data.frame(mcr_summary(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                                        c(a = 1, b = 2, c = 3))))
  expect_true(is.data.frame(mcr_summary(list(s1 = c(a=1, b=2, c=3)),
                                        c(a = 1, b = 2, c = 3))))
})

test_that("mcr_summary returns a named object", {
  colnames = c("n", "HI", "MCR", "Reciprocal", "Group", "THQ", "MHQ", "Missed")
  
  # 'values' as a vector
  expect_named(mcr_summary(c(a=1, b=2, c=3, d=4, e=5), 1:5), colnames)
  
  # 'values' as a matrix
  expect_equal(colnames(mcr_summary(matrix(1:15, ncol = 3,
                                           dimnames = list(letters[1:5], c("s1", "s2", "s3"))), 1:5)),
               colnames)
  expect_equal(colnames(mcr_summary(matrix(1:5, ncol = 1, dimnames = list(letters[1:5], "s1")), 1:5)),
               colnames)
  
  # 'values' as a list
  expect_equal(colnames(mcr_summary(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                                    c(a = 1, b = 2, c = 3))),
               colnames)
  expect_equal(colnames(mcr_summary(list(s1 = c(a=1, b=2, c=3)), c(a = 1, b = 2, c = 3))),
               colnames)
})

test_that("mcr_summary returns an object whose rows are named if the given sets of values are named", {
  # 'values' as a matrix
  expect_equal(rownames(mcr_summary(matrix(1:15, ncol = 3,
                                           dimnames = list(letters[1:5], c("s1", "s2", "s3"))), 1:5)),
               c("s1", "s2", "s3"))
  expect_equal(rownames(mcr_summary(matrix(1:5, ncol = 1, dimnames = list(letters[1:5], "s1")), 1:5)),
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
  expect_true(all_na(mcr_summary(matrix(1:15, ncol = 3,
                                        dimnames = list(NULL, c("s1", "s2", "s3"))), 1:5)$THQ))
  expect_false(all_na(mcr_summary(matrix(1:15, ncol = 3,
                                         dimnames = list(letters[1:5], c("s1", "s2", "s3"))), 1:5)$THQ))
  expect_true(all_na(mcr_summary(matrix(1:5, ncol = 1, dimnames = list(NULL, "s1")), 1:5)$THQ))
  expect_false(all_na(mcr_summary(matrix(1:5, ncol = 1, dimnames = list(letters[1:5], "s1")), 1:5)$THQ))

  # 'values' as a list
  expect_true(all_na(mcr_summary(list(s1 = c(1, 2), s2 = c(2), s3 = c(3, 4)),
                                 list(c(1, 2), 1, c(2, 3)))$THQ))
  expect_false(all_na(mcr_summary(list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4)),
                                  list(c(1, 2), 1, c(2, 3)))$THQ))
  expect_true(all_na(mcr_summary(list(s1 = c(1, 2, 3)), list(c(1, 2, 3)))$THQ))
  expect_false(all_na(mcr_summary(list(s1 = c(a=1, b=2, c=3)), list(c(1, 2, 3)))$THQ))
})

test_that("mcr_summary computes the indicators of the MCR approach", {
  # 'values' as a vector
  expect_equal(mcr_summary(c(a=1, b=2, c=3, d=4, e=5), 1:5),
               list(n = 5, HI = 5, MCR = 5, Reciprocal = 0.2,
                    Group = "I", THQ = "a", MHQ = 1, Missed = 0.8))
  
  # 'values' as a matrix
  expect_equal(mcr_summary(matrix(c(1,2,3,4,5, 2,4,6,8,10, 3,6,9,12,15), ncol = 3,
                                  dimnames = list(letters[1:5])), 1:5),
               data.frame(n = c(5,5,5),
                          HI = c(5, 10, 15),
                          MCR = c(5, 5, 5),
                          Reciprocal = c(0.2, 0.2, 0.2),
                          Group = c("I", "I", "I"),
                          THQ = c("a", "a", "a"),
                          MHQ = c(1, 2, 3),
                          Missed = c(0.8, 0.8, 0.8)))
  expect_equal(mcr_summary(matrix(1:5, ncol = 1, dimnames = list(letters[1:5])), 1:5),
               data.frame(n = 5, HI = 5, MCR = 5, Reciprocal = 0.2,
                          Group = "I", THQ = "a", MHQ = 1, Missed = 0.8))

  # 'values' as a list
  expect_equal(mcr_summary(list(c(a=1, b=2), c(a=2), c(b=4, c=6)), c(a = 1, b = 2, c = 3)),
               data.frame(n = c(2, 1, 2),
                          HI = c(2, 2, 4),
                          MCR = c(2, 1, 2),
                          Reciprocal = c(0.5, 1, 0.5),
                          Group = c("I", "I", "I"),
                          THQ = c("a", "a", "b"),
                          MHQ = c(1, 2, 2),
                          Missed = c(0.5, 0, 0.5)))
  expect_equal(mcr_summary(list(c(a=1, b=2, c=3)), c(a = 1, b = 2, c = 3)),
               data.frame(n = 3, HI = 3, MCR = 3, Reciprocal = 1/3,
                          Group = "I", THQ = "a", MHQ = 1, Missed = 2/3))
})

test_that("mcr_summary returns an identical result whatever the structure of references", {
  # 'values' as a list: references as vector or list
  values = list(s1 = c(a=1, b=2), s2 = c(a=2), s3 = c(b=3, c=4))
  references_vector = c(a=1, b=2, c=3)
  references_list = list(c(1,2), 1, c(2,3))

  expect_identical(mcr_summary(values, references_vector),
                   mcr_summary(values, references_list))
})


##### thq_pairs #####

test_that("thq_pairs requires values to be named", {
  # 'values' as a matrix
  expect_error(thq_pairs(values = matrix(c(1,0, 1,1, 0,1), ncol = 3),
                         references = c(a = 1, b = 1)))
  expect_error(thq_pairs(values = matrix(c(1,0, 1,1, 0,1), ncol = 3, dimnames = list(letters[1:2])),
                         references = c(a = 1, b = 1)),
               NA)
  
  # 'values' as a list
  expect_error(thq_pairs(values = list(c(1, 1),
                                       c(1, 1),
                                       c(1, 1)),
                         references = c(a = 1, b = 1)))
  expect_error(thq_pairs(values = list(c(a = 1, a = 1),
                                       c(b = 1, b = 1),
                                       c(a = 1, b = 1)),
                         references = c(a = 1, b = 1)),
               NA)
  
  # 'hq' as a matrix
  expect_error(thq_pairs(hq = matrix(c(1,0, 1,1, 0,1), ncol = 3),
                         hi = c(1, 2, 1)))
  expect_error(thq_pairs(hq = matrix(c(1,0, 1,1, 0,1), ncol = 3, dimnames = list(letters[1:2])),
                         hi = c(1, 2, 1)),
               NA)
  
  # 'hq' as a list
  expect_error(thq_pairs(hq = list(c(1, 1),
                                   c(1, 1),
                                   c(1, 1)),
                         hi = c(2, 2, 2)))
  expect_error(thq_pairs(hq = list(c(a = 1, a = 1),
                                   c(b = 1, b = 1),
                                   c(a = 1, b = 1)),
                         hi = c(2, 2, 2)),
               NA)
})

test_that("thq_pairs returns a symmetric matrix", {
  # 'values' as a matrix
  expect_true(isSymmetric.matrix(
    thq_pairs(values = matrix(c(1,0, 1,1, 0,1), ncol = 3, dimnames = list(letters[1:2])),
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
  # 1. alone is FALSE and no set of values has more than one element different from 0
  # 2. threshold is TRUE and no related hazard index is greater than 1
  # 3. treshold is TRUE, alone is FALSE and no set of values meets the two conditions
  
  # 'values' as a matrix
  # Case 1
  expect_null(thq_pairs(values = matrix(c(1,0, 1,0, 0,1), ncol = 3, dimnames = list(letters[1:2])),
                        references = c(a = 1, b = 1),
                        threshold = FALSE, alone = FALSE))
  expect_false(is.null(
    thq_pairs(values = matrix(c(1,0, 1,1, 0,1), ncol = 3, dimnames = list(letters[1:2])),
              references = c(a = 1, b = 1),
              threshold = FALSE, alone = FALSE)
  ))
  
  # Case 2
  expect_null(thq_pairs(values = matrix(c(1,0, 1,0, 0,1), ncol = 3, dimnames = list(letters[1:2])),
                        references = c(a = 1, b = 1),
                        threshold = TRUE, alone = TRUE))
  expect_false(is.null(
    thq_pairs(values = matrix(c(2,0, 1,1, 0,2), ncol = 3, dimnames = list(letters[1:2])),
              references = c(a = 1, b = 1),
              threshold = TRUE, alone = TRUE)
  ))
  
  # Case 3
  expect_null(thq_pairs(values = matrix(c(2,0, 0.5,0.5, 0,2), ncol = 3, dimnames = list(letters[1:2])),
                        references = c(a = 1, b = 1),
                        threshold = TRUE, alone = FALSE))
  expect_false(is.null(
    thq_pairs(values = matrix(c(2,0, 1,1, 0,2), ncol = 3, dimnames = list(letters[1:2])),
              references = c(a = 1, b = 1),
              threshold = TRUE, alone = FALSE)
  ))
  
  # 'values' as a list
  # Case 1
  expect_null(thq_pairs(values = list(c(a = 1, b = 0),
                                      c(b = 1, c = 0),
                                      c(c = 1)),
                        references = c(a = 1, b = 1, c = 1),
                        threshold = FALSE, alone = FALSE))
  expect_false(is.null(
    thq_pairs(values = list(c(a = 1, b = 1),
                            c(b = 1, c = 1),
                            c(c = 1)),
              references = c(a = 1, b = 1, c = 1),
              threshold = FALSE, alone = FALSE)
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
  
  # Case 3
  expect_null(thq_pairs(values = list(c(a = 2),
                                      c(b = 0.5, c = 0.5),
                                      c(c = 1)),
                        references = c(a = 1, b = 1, c = 1),
                        threshold = TRUE, alone = FALSE))
  expect_false(is.null(
    thq_pairs(values = list(c(a = 2),
                            c(b = 1, c = 1),
                            c(c = 1)),
              references = c(a = 1, b = 1, c = 1),
              threshold = TRUE, alone = FALSE)
  ))
})

test_that("thq_pairs ignore values equal to 0", {
  # 'values' as a matrix
  expect_equal(
    thq_pairs(values = matrix(c(1,0,1, 0,1,0, 0,0,1), ncol = 3, dimnames = list(letters[1:3])),
              references = c(a = 1, b = 1, c = 1),
              threshold = FALSE, alone = TRUE),
    as.table(matrix(c(0,0,1,0, 0,0,0,1, 1,0,0,1, 0,1,1,0), ncol = 4,
                    dimnames = list(c(letters[1:3], "NULL"), c(letters[1:3], "NULL"))))
  )
  
  # 'values' as a list
  expect_equal(
    thq_pairs(values = list(c(a = 1, c = 1),
                            c(b = 1),
                            c(c = 1, d = 0)),
              references = c(a = 1, b = 1, c = 1, d = 1),
              threshold = FALSE, alone = TRUE),
    as.table(matrix(c(0,0,1,0, 0,0,0,1, 1,0,0,1, 0,1,1,0), ncol = 4,
                    dimnames = list(c(letters[1:3], "NULL"), c(letters[1:3], "NULL"))))
  )
})

test_that("thq_pairs return has one additional ending row and column if alone is TRUE", {
  # Two cases:
  # 1. alone is TRUE and one value name is alphabetically after NULL, there are elements that are alone
  # 2. alone is TRUE and one value name is alphabetically after NULL, but no element is alone
  
  # 'values' as a matrix
  # Case 1
  expect_equal(
    unique(nth_values(dimnames(
      thq_pairs(values = matrix(c(1,0,0, 1,0,1, 0,1,0), ncol = 3, dimnames = list(c("a","b","z"))),
                references = c(a = 1, b = 1, z = 1),
                threshold = FALSE, alone = TRUE)
    ), n = "last")),
    "NULL")
  
  # Case 2
  expect_equal(
    unique(nth_values(dimnames(
      thq_pairs(values = matrix(c(1,1,0, 1,0,1, 0,1,1), ncol = 3, dimnames = list(c("a","b","z"))),
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
  values_matrix = matrix(c(1,0,1, 1,0,0, 0,0,1), ncol = 3, dimnames = list(letters[1:3]))
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
  values_matrix = matrix(c(1,0,1, 1,0,0, 0,0,1), ncol = 3, dimnames = list(letters[1:3]))
  values_list = list(c(a = 1, b = 0.5, c = 1),
                     c(a = 2),
                     c(c = 2))
  references = c(a = 1, b = 1, c = 1)
  
  expect_equal(
    thq_pairs(values = values_matrix,
              references = references,
              levels = NULL),
    as.table(matrix(c(0,1, 1,0), ncol = 2,
                    dimnames = list(c("a","c"), c("a","c"))))
  )
  expect_equal(
    thq_pairs(values = values_matrix,
              references = references,
              levels = c("a", "b", "c")),
    as.table(matrix(c(0,0,1, 0,0,0, 1,0,0), ncol = 3,
                    dimnames = list(letters[1:3], letters[1:3])))
  )
  
  expect_equal(
    thq_pairs(values = values_list,
              references = references,
              levels = NULL),
    as.table(matrix(c(0,1, 1,0), ncol = 2,
                    dimnames = list(c("a","c"), c("a","c"))))
  )
  expect_equal(
    thq_pairs(values = values_list,
              references = references,
              levels = c("a", "b", "c")),
    as.table(matrix(c(0,0,1, 0,0,0, 1,0,0), ncol = 3,
                    dimnames = list(letters[1:3], letters[1:3])))
  )
  
  
  # Arguments threshold and alone, on the following matrix (and an equivalent list):
  #    s1 s2 s3
  # a 0.5  0  2
  # b 0.5  2  0
  # If threshold = T and alone = F: NULL
  # If threshold = F and alone = F: set 1 si considered (ab = 1)
  # If threshold = T and alone = T: sets 2 and 3 are considered (aNULL = 1, bNULL = 1)
  # If threshold = F and alone = T: sets 1, 2 and 3 are considered (ab = 1, aNULL = 1, bNULL = 1)
  values_matrix = matrix(c(0.5,0.5,
                           0,  2,
                           2,  0),
                           ncol = 3, dimnames = list(letters[1:2], c("s1", "s2", "s3")))
  values_list = list(c(a = 0.5, b = 0.5),
                     c(b = 2),
                     c(a = 2))
  references = c(a = 1, b = 1)
  
  expect_identical(
    thq_pairs(values = values_matrix, references = references,
              threshold = TRUE, alone = FALSE),
    NULL
  )
  expect_equal(
    thq_pairs(values = values_matrix, references = references,
              threshold = FALSE, alone = FALSE),
    as.table(matrix(c(0,1, 1,0), ncol = 2,
                    dimnames = list(letters[1:2], letters[1:2])))
  )
  expect_equal(
    thq_pairs(values = values_matrix, references = references,
              threshold = TRUE, alone = TRUE),
    as.table(matrix(c(0,0,1, 0,0,1, 1,1,0), ncol = 3,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  expect_equal(
    thq_pairs(values = values_matrix, references = references,
              threshold = FALSE, alone = TRUE),
    as.table(matrix(c(0,1,1, 1,0,1, 1,1,0), ncol = 3,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  
  expect_identical(
    thq_pairs(values = values_list, references = references,
              threshold = TRUE, alone = FALSE),
    NULL
  )
  expect_equal(
    thq_pairs(values = values_list, references = references,
              threshold = FALSE, alone = FALSE),
    as.table(matrix(c(0,1, 1,0), ncol = 2,
                    dimnames = list(letters[1:2], letters[1:2])))
  )
  expect_equal(
    thq_pairs(values = values_list, references = references,
              threshold = TRUE, alone = TRUE),
    as.table(matrix(c(0,0,1, 0,0,1, 1,1,0), ncol = 3,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  expect_equal(
    thq_pairs(values = values_list, references = references,
              threshold = FALSE, alone = TRUE),
    as.table(matrix(c(0,1,1, 1,0,1, 1,1,0), ncol = 3,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  
  
  # Matrix having only one value per set
  expect_equal(
    thq_pairs(values = matrix(c(1,1,1,1,1),
                              ncol = 5, dimnames = list("a")),
              references = c(a = 1),
              threshold = FALSE, alone = TRUE),
    as.table(matrix(c(0,5, 5,0), ncol = 2,
                    dimnames = list(c("a", "NULL"), c("a", "NULL"))))
  )
  
  # Matrix having only one set of values
  expect_equal(
    thq_pairs(values = matrix(c(1, 1),
                              ncol = 1, dimnames = list(letters[1:2])),
              references = c(a = 1, b = 1),
              threshold = FALSE, alone = TRUE),
    as.table(matrix(c(0,1,0, 1,0,0, 0,0,0), ncol = 3,
                    dimnames = list(c(letters[1:2], "NULL"), c(letters[1:2], "NULL"))))
  )
  
  
  # List having only one value per set
  expect_equal(
    thq_pairs(values = list(c(a = 1),
                            c(b = 1),
                            c(c = 1)),
              references = c(a = 1, b = 1, c = 1),
              threshold = FALSE, alone = TRUE),
    as.table(matrix(c(0,0,0,1, 0,0,0,1, 0,0,0,1, 1,1,1,0), ncol = 4,
                    dimnames = list(c(letters[1:3], "NULL"), c(letters[1:3], "NULL"))))
  )
  
  # List having only one set of values
  expect_equal(
    thq_pairs(values = list(c(a = 1, b = 1)),
              references = c(a = 1, b = 1),
              threshold = FALSE, alone = TRUE),
    as.table(matrix(c(0,1,0, 1,0,0, 0,0,0), ncol = 3,
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
  # 'values' as a list: references as vector or list
  values = list(s1 = c(a=1, b=2, c=3), s2 = c(a=2, b=1, c=3), s3 = c(b=3, c=4))
  references_vector = c(a=1, b=2, c=3)
  references_list = list(c(1,2,3), c(1,2,3), c(2,3))

  expect_identical(thq_pairs(values, references_vector),
                   thq_pairs(values, references_list))
})

test_that("thq_pairs returns an identical result whatever the chosen usage", {
  values_matrix = matrix(c(1,2,1,2,1,
                           2,1,1,1,2,
                           0,1,2,1,1), ncol = 3, dimnames = list(letters[1:5], c("s1", "s2", "s3")))
  values_list = list(s1 = c(a=1, b=2, c=1, d=2, e=1),
                     s2 = c(a=2, b=1, c=1, d=1, e=2),
                     s3 = c(a=0, b=1, c=2, d=1, e=1))
  references = c(a=1, b=1, c=1, d=1, e=1)

  expect_identical(thq_pairs(values_matrix, references),
                   thq_pairs(hq = hazard_quotient(values_matrix, references),
                             hi = hazard_index(values_matrix, references)))
  expect_identical(thq_pairs(values_list, references),
                   thq_pairs(hq = sapply(values_list, hazard_quotient, references),
                             hi = sapply(values_list, hazard_index, references)))
})


