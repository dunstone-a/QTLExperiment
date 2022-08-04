# Checks for proper construction and get/setting multiStateQTLExperiment slots.
# library(multiStateQTLExperiment); library(testthat); source("setup.R"); source("test-msqe-class.R")

context("multiStateQTLExperiment class")

nStates <- 10
nQTL <- 100
b <- matrix(rnorm(1000), ncol=nStates)
se <- matrix(abs(rnorm(1000)), ncol=nStates)
p <- matrix(runif(1000), ncol=nStates)
feature_ids <- sample(LETTERS[seq(from = 1, to = 10)], nQTL, replace=TRUE)
variant_ids <- paste0("var", sample(seq(1e3:1e5), nQTL))

test_that("construction of the MSQE works correctly", {
  msqe <- multiStateQTLExperiment(assay=list(betas=b, error=se, pval=p, lfsr=p),
                                  rowData=DataFrame(feature_id=feature_ids,
                                                    variant_id=variant_ids))
  expect_equivalent(class(msqe), "multiStateQTLExperiment")
  expect_equivalent(assay(msqe, "betas"), b)
  expect_equivalent(assay(msqe, "error"), se)
  expect_equivalent(assay(msqe, "pval"), p)
  expect_equivalent(assay(msqe, "lfsr"), p)
})

test_that("MSQE valid check works correctly", {

  expect_error(multiStateQTLExperiment(assay=list(error=se),
                                       rowData=DataFrame(feature_id=feature_ids,
                                                         variant_id=variant_ids)),
               "betas: assay needed")
  expect_error(multiStateQTLExperiment(assay=list(betas=b),
                          rowData=DataFrame(feature_id=feature_ids,
                                            variant_id=variant_ids)),
               "error: assay needed")

  expect_error(multiStateQTLExperiment(assay=list(betas=b, error=se),
                          rowData=DataFrame(variant_id=variant_ids)),
               "feature_id: needed in rowData")

  expect_error(multiStateQTLExperiment(assay=list(betas=b, error=se),
                                       rowData=DataFrame(feature_id=feature_id)),
               "variant_id: needed in rowData")
})


test_that("MSQE getting and setting works correctly", {
  labels <- sample(letters, ncol(msqe), replace=TRUE)
  colLabels(msqe) <- labels
  expect_identical(colLabels(msqe), labels)

  # Manual deletion.
  colLabels(msqe) <- NULL
  expect_identical(colLabels(msqe), NULL)
})
