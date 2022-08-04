# Checks the state and test wise summarize functions
# library(multiStateQTLExperiment); library(testthat)
# source("setup.R"); source("test-get-significant.R")

msqe <- mock


test_that("getTopPerFeature selects the correct snp-feature pair", {
  test <- getTopPerFeature(msqe)
  expect_equal(pval(test)[1, 1],
               min(pval(subset(msqe, feature_id == "geneA"))[, 1]))
})

test_that("getSignificant in simple mode works with one and two thresholds", {
  test <- getSignificant(msqe, thresh=0.1, mode="simple")
  expect_equal(rowSums(pval(test) <= 0.1), rowSums(assay(test, "significant")))

  test <- getSignificant(msqe, thresh=0.1, second.thresh=0.5, mode="simple")
  expect_true(all(rowSums(assay(test, "significant")) >= rowSums(pval(test) <= 0.1)))
})


test_that("getSignificant global and feature-wise modes works", {
  test_glob <- getSignificant(msqe, thresh=0.1, mode="global-FDR")
  test_feat <- getSignificant(msqe, thresh=0.1, mode="feature-wise-FDR")
  expect_true(all(test_glob$significance_threshold <= test_feat$significance_threshold))

  test_bonf <- getSignificant(msqe, thresh=0.1, p.adjust.method="bonferroni")
  test_fdr <- getSignificant(msqe, thresh=0.1)
  expect_true(all(test_bonf$significance_threshold <= test_fdr$significance_threshold))

})

