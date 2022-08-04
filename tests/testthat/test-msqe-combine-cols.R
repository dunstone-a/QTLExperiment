# Checks the column-wise combining methods.
# library(multiStateQTLExperiment); library(testthat)
# source("setup.R"); source("test-msqe-combine-cols.R")

msqe <- mockMSQE()
d1 <- prcomp(t(betas(msqe)), rank=5)$x

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
})


test_that("cbind gives correct error messages in the basic case", {
  msqe.alt <- msqe
  msqe.alt <- msqe.alt[1:80, ]
  expect_error(cbind(msqe, msqe.alt), "duplicated and the data do not match")
})


test_that("cbind respects the rowData and gives proper error messages", {
  msqe2 <- msqe
  rowData(msqe2)$X <- runif(nrow(msqe2))
  msqe3 <- cbind(msqe, msqe2)
  expect_identical(rowData(msqe3)$X, rowData(msqe2)$X)


  rowData(msqe3)$X <- runif(nrow(msqe3))
  expect_error(cbind(msqe2, msqe3), "duplicated and the data do not match")
})


test_that("cbind respects the internal fields correctly", {
  # Respects the internal rowData
  reducedDim(msqe, "PCA") <- d1
  msqe2 <- msqe
  int_rowData(msqe2)$X <- runif(nrow(msqe2))
  msqe3 <- cbind(msqe, msqe2)
  expect_identical(int_rowData(msqe3)$X, int_rowData(msqe2)$X)

  # Respects reducedDims
  expect_identical(reducedDim(msqe3),
                   rbind(reducedDim(msqe), reducedDim(msqe2)))

  # Respects reordered internal colData.
  alpha <- cbind(msqe, msqe)
  alt.msqe <- msqe
  reducedDims(alt.msqe) <- rev(reducedDims(alt.msqe))
  bravo <- cbind(msqe, alt.msqe)
  expect_identical(alpha@int_colData$PCA, bravo@int_colData$PCA)
})

test_that("cbind handles errors in the internal fields correctly", {
  # Chokes correctly when presented with errors.
  msqe.err <- msqe
  reducedDim(msqe.err, "PCA") <- NULL
  expect_error(cbind(msqe.err, msqe), "'int_colData'")

  # Don't concatenate names when merging metadata().
  msqe4 <- rbind(A=msqe, B=msqe)
  expect_identical(objectVersion(msqe4), objectVersion(msqe))
})





