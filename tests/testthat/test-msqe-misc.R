# Checks the state and test wise summarize functions
# library(multiStateQTLExperiment); library(testthat)
# source("setup.R"); source("test-msqe-misc")

msqe <- mockMSQE()

test_that("objectVersion works correctly", {
  expect_identical(objectVersion(msqe), packageVersion("multiStateQTLExperiment"))
})

test_that("colLabels GETS and SETS correctly", {
  labels <- sample(letters, ncol(msqe), replace=TRUE)
  colLabels(msqe) <- labels
  expect_identical(colLabels(msqe), labels)

  # Manual deletion.
  colLabels(msqe) <- NULL
  expect_identical(colLabels(msqe), NULL)

  # Additional actions work.
  expect_warning(colLabels(msqe, onAbsence="warn"), "NULL")
  expect_error(colLabels(msqe, onAbsence="error"), "NULL")
})


test_that("mainExpName SETS correctly", {
  mainExpName(msqe) <- "test-that"
  expect_equal(int_metadata(msqe)$mainExpName, "test-that")
  mainExpName(msqe) <- NULL
  expect_null(int_metadata(msqe)$mainExpName)
})

test_that("mainExpName GETS correctly", {
  mainExpName(msqe) <- "test-that"
  expect_equal(mainExpName(msqe), "test-that")
  mainExpName(msqe) <- NULL
  expect_null(mainExpName(msqe))
})
