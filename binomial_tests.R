tests 
library(testthat)

test_that("check_prob", {
  expect_true(check_prob(prob = 0.5))   
  expect_error(check_prob(5))             
  expect_length(check_prob(0.5), 1)      
})

test_that("check_trials", {
  expect_true(check_trials(7))            
  expect_error(check_trials(.4))          
  expect_error(check_trials(-8))         
  expect_length(check_trials(5), 1)       
})

test_that("check_success", {
  expect_true(check_success(c(4), 8))     
  expect_error(check_success(c(8), 4))    
  expect_error(check_success(-8))         
})


test_that("aux_mean", {
  
  expect_equal(aux_mean(10, 0.3), 3)
  expect_type(aux_mean(10, 0.5), 'double')
  expect_length(aux_mean(10, 0.01), 1)
  
})

test_that("aux_variance", {
  
  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_type(aux_mean(10, 0.5), 'double')
  expect_length(aux_mean(10,0.01), 1)
  
})

test_that("aux_mode", {
  
  expect_equal(aux_mode(10, 0.3), 3)
  expect_type(aux_mode(10, 0.5), 'double')
  expect_length(aux_mode(10,0.01), 1)
  
})

test_that("aux_skewness", {
  
  expect_equal(round(aux_skewness(10, 0.3), 3), 0.276)
  expect_type(aux_skewness(10, 0.5), 'double')
  expect_length(aux_skewness(10,0.01), 1)
})

test_that("aux_kurtosis", {
  
  expect_equal(round(aux_kurtosis(10, 0.3), 3), -0.124)
  expect_type(aux_kurtosis(10, 0.5), 'double')
  expect_length(aux_kurtosis(10,0.01), 1)
  
})


test_that("bin_choose", {
  expect_error(bin_choose(2, 5))
  expect_equal(bin_choose(5, 2), 10)
  expect_length(bin_choose(5, 1:3), 3)
})

test_that("bin_probability", {
  expect_error(bin_probability(10, 2, 1))
  expect_type(bin_probability(2, 5, 0.5), 'double')
  expect_length(bin_probability(0:2, 5, 0.5), 3)
})

test_that("bin_distribution", {
  expect_error(bin_distribution(10, 1.1))
  expect_is(bin_distribution(10, 0.5), c("bindis", "data.frame"))
  expect_length(bin_distribution(10, 0.5), 2)
  expect_equal(round(bin_distribution(5, 0.5)[[2]][1], 3), 0.031)
})

