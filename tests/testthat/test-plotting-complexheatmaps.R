# Checks the complexheatmap plots
# library(multiStateQTLExperiment); library(testthat)
# source("setup.R"); source("test-plotting-complexheatmaps.R")

msqe <- mockMSQE()


test_that("produce pairwise sharing plots with complex row and annotations", {
  
  msqe <- run_pairwise_sharing(msqe, thresh=0.1)
  expect_output(class(plotPairwiseSharing(msqe)), "Heatmap")
  
})


test_that("produce upset plots with complex row annotations")