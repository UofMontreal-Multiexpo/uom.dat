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
  expect_equal(colnames(hazard_quotient(matrix(1:10, ncol = 2, dimnames = list(NULL, c("o1", "o2"))), 1:5)),
               c("o1", "o2"))
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
  expect_named(hazard_index(matrix(1:10, ncol = 2, dimnames = list(NULL, c("o1", "o2"))), 1:5),
               c("o1", "o2"))
  
  # 'hq' as a named matrix
  expect_named(hazard_index(hq = matrix(1:10, ncol = 2, dimnames = list(NULL, c("o1", "o2"))), 1:5),
               c("o1", "o2"))
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
  expect_named(maximum_hazard_quotient(matrix(1:10, ncol = 2, dimnames = list(NULL, c("o1", "o2"))), 1:5),
               c("o1", "o2"))
  
  # 'hq' as a named matrix
  expect_named(maximum_hazard_quotient(hq = matrix(1:10, ncol = 2, dimnames = list(NULL, c("o1", "o2"))), 1:5),
               c("o1", "o2"))
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
  expect_named(maximum_cumulative_ratio(matrix(1:10, ncol = 2, dimnames = list(NULL, c("o1", "o2"))), 1:5),
               c("o1", "o2"))
})

test_that("maximum_cumulative_ratio returns an unnamed object if unnamed vectors of hi and mhq are given", {
  # 'hi' and 'mhq' as unnamed vectors
  expect_named(maximum_cumulative_ratio(hi = 1:5, mhq = 1:5), NULL)
})

test_that("maximum_cumulative_ratio returns a named object if named vectors of hi and mhq are given", {
  # 'hi' and/or 'mhq' as named vectors
  expect_named(maximum_cumulative_ratio(hi  = c(o1=1, o2=1, o3=1, o4=1, o5=1),
                                        mhq = c(o1=1, o2=1, o3=1, o4=1, o5=1)),
               c("o1", "o2", "o3", "o4", "o5"))
  expect_named(maximum_cumulative_ratio(hi = c(o1=1, o2=1, o3=1, o4=1, o5=1), mhq = 1:5),
               c("o1", "o2", "o3", "o4", "o5"))
  expect_named(maximum_cumulative_ratio(hi = 1:5, mhq = c(o1=1, o2=1, o3=1, o4=1, o5=1)),
               c("o1", "o2", "o3", "o4", "o5"))
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
  expect_named(missed_toxicity(matrix(1:10, ncol = 2, dimnames = list(NULL, c("o1", "o2"))), 1:5),
               c("o1", "o2"))
})

test_that("missed_toxicity returns an unnamed object if an unnamed vector of mcr is given", {
  # 'mcr' as an unnamed vector
  expect_named(missed_toxicity(mcr = 1:5), NULL)
})

test_that("missed_toxicity returns a named object if a named vector of mcr is given", {
  # 'mcr' as a named vector
  expect_named(missed_toxicity(mcr = c(o1=1, o2=1, o3=1, o4=1, o5=1)),
               c("o1", "o2", "o3", "o4", "o5"))
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
  expect_named(reciprocal_of_mcr(matrix(1:10, ncol = 2, dimnames = list(NULL, c("o1", "o2"))), 1:5),
               c("o1", "o2"))
})

test_that("reciprocal_of_mcr returns an unnamed object if unnamed vectors of hi and mhq are given", {
  # 'hi' and 'mhq' as unnamed vectors
  expect_named(reciprocal_of_mcr(hi = 1:5, mhq = 1:5), NULL)
})

test_that("reciprocal_of_mcr returns a named object if named vectors of hi and mhq are given", {
  # 'hi' and/or 'mhq' as named vectors
  expect_named(reciprocal_of_mcr(hi  = c(o1=1, o2=1, o3=1, o4=1, o5=1),
                                 mhq = c(o1=1, o2=1, o3=1, o4=1, o5=1)),
               c("o1", "o2", "o3", "o4", "o5"))
  expect_named(reciprocal_of_mcr(hi = c(o1=1, o2=1, o3=1, o4=1, o5=1), mhq = 1:5),
               c("o1", "o2", "o3", "o4", "o5"))
  expect_named(reciprocal_of_mcr(hi = 1:5, mhq = c(o1=1, o2=1, o3=1, o4=1, o5=1)),
               c("o1", "o2", "o3", "o4", "o5"))
})

test_that("reciprocal_of_mcr returns an unnamed object if an unnamed vector of mcr is given", {
  # 'mcr' as an unnamed vector
  expect_named(reciprocal_of_mcr(mcr = 1:5), NULL)
})

test_that("reciprocal_of_mcr returns a named object if a named vector of mcr is given", {
  # 'mcr' as a named vector
  expect_named(reciprocal_of_mcr(mcr = c(o1=1, o2=1, o3=1, o4=1, o5=1)),
               c("o1", "o2", "o3", "o4", "o5"))
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

test_that("hazard_quotient returns the right data structure", {
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
  expect_named(top_hazard_quotient(matrix(1:10, ncol = 2, dimnames = list(NULL, c("o1", "o2"))), 1:5),
               c("o1", "o2"))
  expect_named(top_hazard_quotient(matrix(1:5,  ncol = 1, dimnames = list(NULL, "o1")), 1:5),
               "o1")
  
  # 'hq' as a matrix
  expect_named(top_hazard_quotient(hq = matrix(1:10, ncol = 2, dimnames = list(NULL, c("o1", "o2")))),
               c("o1", "o2"))
  expect_named(top_hazard_quotient(hq = matrix(1:5,  ncol = 1, dimnames = list(NULL, "o1"))),
               "o1")
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

test_that("top_hazard_quotient find the top hazard quotients", {
  # 'values' as a vector
  expect_equal(top_hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), 1:5),
               c(a=1, b=1, c=1, d=1, e=1))
  expect_equal(top_hazard_quotient(c(a=1, b=2, c=3, d=4, e=5), c(1,1,1,1,1)),
               c(e=5, d=4, c=3))
  
  # # 'values' as a matrix
  expect_equal(top_hazard_quotient(matrix(1:10, ncol = 2, dimnames = list(letters[1:5], c("o1", "o2"))), c(1,1,1,1,1)),
               list(o1 = c(e=5, d=4, c=3),
                    o2 = c(e=10, d=9, c=8, b=7)))
  expect_equal(top_hazard_quotient(matrix(1:5, ncol = 1, dimnames = list(letters[1:5], "o1")), 1:5),
               list(o1 = c(a=1, b=1, c=1, d=1, e=1)))
  
  # 'hq' as a vector
  expect_equal(top_hazard_quotient(hq = c(a=1, b=1, c=1, d=1, e=1)),
               c(a=1, b=1, c=1, d=1, e=1))
  expect_equal(top_hazard_quotient(hq = c(a=1, b=2, c=3, d=4, e=5)),
               c(e=5, d=4, c=3))
  
  # # 'hq' as a matrix
  expect_equal(top_hazard_quotient(hq = matrix(1:10, ncol = 2, dimnames = list(letters[1:5], c("o1", "o2")))),
               list(o1 = c(e=5, d=4, c=3),
                    o2 = c(e=10, d=9, c=8, b=7)))
  expect_equal(top_hazard_quotient(hq = matrix(1:5, ncol = 1, dimnames = list(letters[1:5], "o1"))),
               list(o1 = c(e=5, d=4, c=3)))
})

test_that("top_hazard_quotient returns an identical result whatever the chosen usage", {
  values_vector = c(a=1, b=2, c=3, d=4, e=5)
  values_matrix = matrix(1:10, ncol = 2, dimnames = list(letters[1:5], c("o1", "o2")))
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
  expect_named(classify_mixture(matrix(1:10, ncol = 2, dimnames = list(NULL, c("o1", "o2"))), 1:5),
               c("o1", "o2"))
})

test_that("classify_mixture returns an unnamed object if unnamed vectors of hi, mhq and mcr are given", {
  # 'hi' and 'mhq' as unnamed vectors
  expect_named(classify_mixture(hi = 1:5, mhq = 1:5), NULL)
  
  # 'hi', 'mhq' and 'mcr' as unnamed vectors
  expect_named(classify_mixture(hi = 1:5, mhq = 1:5, mcr = c(1,1,1,1,1)), NULL)
})

test_that("classify_mixture returns an named object if named vectors of hi, mhq and mcr are given", {
  # 'hi' and/or 'mhq' as named vectors
  expect_named(classify_mixture(hi  = c(o1=1, o2=1, o3=1, o4=1, o5=1),
                                mhq = c(o1=1, o2=1, o3=1, o4=1, o5=1)),
               c("o1", "o2", "o3", "o4", "o5"))
  expect_named(classify_mixture(hi = c(o1=1, o2=1, o3=1, o4=1, o5=1), mhq = 1:5),
               c("o1", "o2", "o3", "o4", "o5"))
  expect_named(classify_mixture(hi = 1:5, mhq = c(o1=1, o2=1, o3=1, o4=1, o5=1)),
               c("o1", "o2", "o3", "o4", "o5"))
  
  # 'hi' and/or 'mhq' and/or 'mcr' as named vectors
  expect_named(classify_mixture(hi  = c(o1=1, o2=1, o3=1, o4=1, o5=1),
                                mhq = c(o1=1, o2=1, o3=1, o4=1, o5=1),
                                mcr = c(o1=1, o2=1, o3=1, o4=1, o5=1)),
               c("o1", "o2", "o3", "o4", "o5"))
  expect_named(classify_mixture(hi = c(o1=1, o2=1, o3=1, o4=1, o5=1), mhq = 1:5, mcr = c(1,0.5,1/3,0.25,0.2)),
               c("o1", "o2", "o3", "o4", "o5"))
  expect_named(classify_mixture(hi = 1:5, mhq = c(o1=1, o2=1, o3=1, o4=1, o5=1), mcr = 1:5),
               c("o1", "o2", "o3", "o4", "o5"))
  expect_named(classify_mixture(hi = 1:5, mhq = 1:5, mcr = c(o1=1, o2=1, o3=1, o4=1, o5=1)),
               c("o1", "o2", "o3", "o4", "o5"))
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


