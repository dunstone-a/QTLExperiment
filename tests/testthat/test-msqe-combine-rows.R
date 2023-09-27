# Checks the row-wise combining methods.
# library(QTLExperiment); library(testthat)
# source("setup.R"); source("test-qtle-combine-rows.R")

qtle <- mockQTLE()

test_that("rbind works correctly in the basic case", {
    shuffled <- sample(nrow(qtle))
    qtle.alt <- qtle[shuffled,]
    feature_id(qtle.alt) <- paste0(feature_id(qtle.alt), "_alt")

    qtle2 <- rbind(qtle, qtle.alt)
    expect_equivalent(assay(qtle2), rbind(assay(qtle), assay(qtle.alt)))
    expect_identical(colData(qtle2), colData(qtle))
})


test_that("rbind gives correct error messages in the basic case", {
    qtle.alt <- qtle
    qtle.alt <- qtle[, 1:8]
    feature_id(qtle.alt) <- paste0(feature_id(qtle.alt), "_alt")
    expect_error(rbind(qtle, qtle.alt),
                 "'...' objects must have the same colnames")
})


test_that("rbind respects the colData and gives proper error messages", {
    qtle2 <- qtle
    colData(qtle2)$X <- runif(ncol(qtle2))
    feature_id(qtle2) <- paste0(feature_id(qtle2), "_alt")
    qtle3 <- rbind(qtle, qtle2)
    expect_identical(colData(qtle3)$X, colData(qtle2)$X)


    colData(qtle3)$X <- runif(ncol(qtle3))
    expect_error(rbind(qtle2, qtle3),
                 "column(s) 'X' in ‘colData’ are duplicated and the data do not match",
                 fixed = TRUE)
})


test_that("rbind respects the internal fields correctly", {
    # Respects the internal colData.
    qtle2 <- qtle
    int_colData(qtle2)$X <- runif(ncol(qtle2))
    feature_id(qtle2) <- paste0(feature_id(qtle2), "_alt")
    qtle3 <- rbind(qtle, qtle2)
    expect_identical(int_colData(qtle3)$X, int_colData(qtle2)$X)
})


test_that("rbind handles errors in internal fields correctly", {
    # Throws errors upon mismatch in the internal colData.
    qtle2 <- qtle
    int_colData(qtle)$X <- runif(ncol(qtle))
    int_colData(qtle2)$X <- runif(ncol(qtle2))
    feature_id(qtle2) <- paste0(feature_id(qtle2), "_alt")
    expect_error(rbind(qtle, qtle2), "'int_colData'")

    # Throws errors upon mismatch in the internal rowData.
    qtle.err <- qtle
    int_rowData(qtle.err)$X <- "YAY"
    feature_id(qtle.err) <- paste0(feature_id(qtle.err), "_alt")
    expect_error(rbind(qtle.err, qtle), "'int_rowData'")

})

