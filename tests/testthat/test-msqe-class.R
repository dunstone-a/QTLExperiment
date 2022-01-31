# Checks for proper construction and get/setting multiStateQTLExperiment slots.
# library(multiStateQTLExperiment); library(testthat); source("setup.R"); source("test-msqe-class.R")

context("multiStateQTLExperiment class")

set.seed(49)
nStates <- 10
nQTL <- 100
b <- matrix(rnorm(1000), ncol=nStates)
se <- matrix(abs(rnorm(1000)), ncol=nStates)
p <- matrix(runif(1000), ncol=nStates)


test_that("construction of the MSQE works correctly", {
  msqe <- multiStateQTLExperiment(assay=b)
  expect_equivalent(assay(msqe), b)

  msqe <- multiStateQTLExperiment(assay=list(beta=b, se=se))
  expect_equivalent(assay(msqe, "beta"), b)
  expect_equivalent(assay(msqe, "se"), se)

  assay(msqe, "pval") <- p
  expect_equivalent(assay(msqe, "pval"), p)
})





