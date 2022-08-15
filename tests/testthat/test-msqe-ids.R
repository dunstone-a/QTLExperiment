# Checks the state and test wise summarize functions
# library(multiStateQTLExperiment); library(testthat)
# source("setup.R"); source("test-msqe-assays")

set.seed(49)
msqe <- mockMSQE()


test_that("Test that id GETS work correctly", {
  expect_equivalent(feature_id(msqe), rowData(msqe)$feature_id)
  expect_equivalent(variant_id(msqe), rowData(msqe)$variant_id)

})

test_that("Test that id SETS work correctly", {
  msqe2 <- msqe
  new_features <- sample(LETTERS, nrow(msqe), replace=TRUE)

  feature_id(msqe2) <- new_features
  expect_equivalent(new_features, rowData(msqe2)$feature_id)
  expect_equivalent(rowData(msqe2)$feature_id,
                    gsub("\\|.*", "", row.names(msqe2)))

})


test_that("Test that internal ids stay correct", {
  msqe2 <- msqe
  new_features <- sample(LETTERS, nrow(msqe), replace=TRUE)

  rowData(msqe2)$feature_id <- new_features

  feature_id(msqe2) <- new_features
  expect_equivalent(new_features, rowData(msqe2)$feature_id)
  expect_equivalent(rowData(msqe2)$feature_id,
                    gsub("\\|.*", "", row.names(msqe2)))

})

