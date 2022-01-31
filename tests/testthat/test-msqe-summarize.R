# Checks the state and test wise summarize functions
# library(multiStateQTLExperiment); library(testthat)
# source("setup.R"); source("test-msqe-summarize.R")

msqe <- mock

test_that("test per state summarize works correctly in the base case", {
  test <- perStateMetrics(msqe)
  expect_equal(test[1, "nTests"], nrow(msqe))
  expect_equal(test$pSignificant, test$nSignificant/test$nTests)
})

# 
# test_that("test per test summarize works correctly in the base case", {
#   test <- perTestMetrics(msqe)
#   expect_equal(test[1, "nStates"], ncol(msqe))
#   expect_equal(test$pSignificant, test$nSignificant/test$nStates)
# })

