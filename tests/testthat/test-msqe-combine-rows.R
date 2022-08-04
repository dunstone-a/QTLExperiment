# Checks the row-wise combining methods.
# library(multiStateQTLExperiment); library(testthat)
# source("setup.R"); source("test-msqe-combine-rows.R")

msqe <- mockMSQE()

test_that("rbind works correctly in the basic case", {
  shuffled <- sample(nrow(msqe))
  msqe.alt <- msqe[shuffled,]

  msqe2 <- rbind(msqe, msqe.alt)
  expect_equivalent(assay(msqe2), rbind(assay(msqe), assay(msqe.alt)))
  expect_identical(colData(msqe2), colData(msqe))

  expect_identical(reducedDim(msqe2, "PCA"),
                   reducedDim(msqe, "PCA"),
                   reducedDim(msqe.alt, "PCA"))
})


test_that("rbind gives correct error messages in the basic case", {
  msqe.alt <- msqe
  msqe.alt <- msqe[, 1:8]
  expect_error(rbind(msqe, msqe.alt),
               "'...' objects must have the same colnames")
})


test_that("rbind respects the colData and gives proper error messages", {
  msqe2 <- msqe
  colData(msqe2)$X <- runif(ncol(msqe2))
  msqe3 <- rbind(msqe, msqe2)
  expect_identical(colData(msqe3)$X, colData(msqe2)$X)


  colData(msqe3)$X <- runif(ncol(msqe3))
  expect_error(rbind(msqe2, msqe3),
               "column(s) 'X' in ‘colData’ are duplicated and the data do not match",
               fixed = TRUE)
})


test_that("rbind respects rowData order", {
  rowData(msqe) <- cbind(rowData(msqe),
                                 DataFrame(A=runif(nrow(msqe)),
                                           B=runif(nrow(msqe))))
  alpha <- rbind(msqe, msqe)
  alt.msqe <- msqe
  rowData(alt.msqe) <- rowData(alt.msqe)[,ncol(rowData(alt.msqe)):1]
  bravo <- rbind(msqe, alt.msqe)
  expect_identical(alpha, bravo)
})


test_that("rbind respects the internal fields correctly", {
  # Respects the internal colData.
  msqe2 <- msqe
  int_colData(msqe2)$X <- runif(ncol(msqe2))
  msqe3 <- rbind(msqe, msqe2)
  expect_identical(int_colData(msqe3)$X, int_colData(msqe2)$X)

  # Respects reordered internal rowData
  int_rowData(msqe) <- cbind(int_rowData(msqe),
                                     DataFrame(A=runif(nrow(msqe)),
                                               B=runif(nrow(msqe))))
  alpha <- rbind(msqe, msqe)
  alt.msqe <- msqe
  int_rowData(alt.msqe) <- int_rowData(alt.msqe)[,ncol(int_rowData(alt.msqe)):1]
  bravo <- rbind(msqe, alt.msqe)
  expect_identical(alpha, bravo)
})


test_that("rbind handles errors in internal fields correctly", {
  # Throws errors upon mismatch in the internal colData.
  msqe2 <- msqe
  int_colData(msqe)$X <- runif(ncol(msqe))
  int_colData(msqe2)$X <- runif(ncol(msqe2))
  expect_error(rbind(msqe, msqe2), "'int_colData'")

  # Throws errors upon mismatch in the internal rowData.
  msqe.err <- msqe
  int_rowData(msqe.err)$X <- "YAY"
  expect_error(rbind(msqe.err, msqe), "'int_rowData'")

  # Don't concatenate names when merging metadata().
  msqe4 <- rbind(A=msqe, B=msqe)
  expect_identical(objectVersion(msqe4), objectVersion(msqe))
})

