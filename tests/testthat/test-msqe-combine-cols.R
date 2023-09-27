# Checks the column-wise combining methods.
# library(QTLExperiment); library(testthat)
# source("setup.R"); source("test-qtle-combine-cols.R")

qtle <- mockQTLE()

test_that("cbind works correctly in the basic case", {
    shuffled <- sample(ncol(qtle))
    qtle.alt <- qtle[,shuffled]
    state_id(qtle.alt) <- paste0(state_id(qtle.alt), "_alt")


    qtle2 <- cbind(qtle, qtle.alt)
    expect_equivalent(assay(qtle2), cbind(assay(qtle), assay(qtle.alt)))
    expect_identical(rowData(qtle), rowData(qtle2))
})


test_that("cbind gives correct error messages in the basic case", {
    qtle.alt <- qtle
    qtle.alt <- qtle.alt[1:80, ]
    expect_error(cbind(qtle, qtle.alt), "duplicated and the data do not match")
})


test_that("cbind respects the rowData and gives proper error messages", {
    qtle2 <- qtle
    rowData(qtle2)$X <- runif(nrow(qtle2))
    state_id(qtle2) <- paste0(state_id(qtle2), "_2")
    qtle3 <- cbind(qtle, qtle2)
    expect_identical(rowData(qtle3)$X, rowData(qtle2)$X)


    rowData(qtle3)$X <- runif(nrow(qtle3))
    expect_error(cbind(qtle2, qtle3), "duplicated and the data do not match")
})


test_that("cbind respects the internal fields correctly", {
    # Respects the internal rowData
    qtle2 <- qtle
    int_rowData(qtle2)$X <- runif(nrow(qtle2))
    state_id(qtle2) <- paste0(state_id(qtle2), "_2")
    qtle3 <- cbind(qtle, qtle2)
    expect_identical(int_rowData(qtle3)$X, int_rowData(qtle2)$X)
})





