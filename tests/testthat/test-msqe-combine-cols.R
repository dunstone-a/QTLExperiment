# Checks the column-wise combining methods.
# library(multiStateQTLExperiment); library(testthat)
# source("setup.R"); source("test-msqe-combine-cols.R")

msqe <- mock

test_that("cbind works correctly in the basic case", {
  reducedDim(msqe, "PCA") <- d1
  shuffled <- sample(ncol(msqe))
  msqe.alt <- msqe[,shuffled]
  
  
  msqe2 <- cbind(msqe, msqe.alt)
  expect_equivalent(assay(msqe2), cbind(assay(msqe), assay(msqe.alt)))
  expect_identical(rowData(msqe), rowData(msqe2))
  expect_identical(reducedDim(msqe2, "PCA"), 
                   rbind(reducedDim(msqe, "PCA"), 
                         reducedDim(msqe.alt, "PCA")))
  #expect_identical(altExp(msqe2), cbind(altExp(msqe), altExp(msqe.alt)))
})


test_that("cbind gives correct error messages in the basic case", {
  msqe.alt <- msqe
  msqe.alt <- msqe.alt[1:80, ]
  expect_error(cbind(msqe, msqe.alt), "duplicated and the data do not match")
})

#test_that("cbind works in the basic case for col pairs", {
  # dual1 <- SingleCellExperiment:::DualSubset(colPair(msqe))
  # dual2 <- SingleCellExperiment:::DualSubset(colPair(msqe.alt))
  # expect_identical(
  #   SingleCellExperiment:::DualSubset(colPair(msqe2)),
  #   c(dual1, dual2)
  # )
#  multiStateQTLExperiment:::DualSubset(colPair(msqe2)),
#  c(dual1, dual2))
#})


test_that("cbind respects the rowData and gives proper error messages", {
  msqe2 <- msqe
  rowData(msqe2)$X <- runif(nrow(msqe2))
  msqe3 <- cbind(msqe, msqe2)
  expect_identical(rowData(msqe3)$X, rowData(msqe2)$X)
  
  
  rowData(msqe3)$X <- runif(nrow(msqe3))
  expect_error(cbind(msqe2, msqe3), "duplicated and the data do not match")
})


test_that("cbind respects the internal fields correctly", {
  # Respects the internal elementMetadata.
  reducedDim(msqe, "PCA") <- d1
  msqe2 <- msqe
  int_elementMetadata(msqe2)$X <- runif(nrow(msqe2))
  msqe3 <- cbind(msqe, msqe2)
  expect_identical(int_elementMetadata(msqe3)$X, int_elementMetadata(msqe2)$X)
  
  # Respects reducedDims
  expect_identical(reducedDim(msqe3), 
                   rbind(reducedDim(msqe), reducedDim(msqe2)))
  
  # Respects reordered internal colData.
  alpha <- cbind(msqe, msqe)
  alt.msqe <- msqe
  int_colData(alt.msqe) <- int_colData(alt.msqe)[,ncol(int_colData(alt.msqe)):1]
  bravo <- cbind(msqe, alt.msqe)
  expect_identical(alpha, bravo)
  
  
})

test_that("cbind handles errors in the internal fields correctly", {
  # Chokes correctly when presented with errors.
  msqe.err <- msqe
  reducedDim(msqe.err, "PCA") <- NULL
  expect_error(cbind(msqe.err, msqe), "'int_colData'")
  
  # msqe.err <- msqe
  # altExp(msqe.err, 1) <- NULL
  # expect_error(cbind(msqe.err, msqe), "'int_colData'")
  
  # Don't concatenate names when merging metadata().
  msqe4 <- rbind(A=msqe, B=msqe)
  expect_identical(objectVersion(msqe4), objectVersion(msqe))
})





