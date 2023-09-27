# Checks the state and test wise summarize functions
# library(QTLExperiment); library(testthat)
# source("setup.R"); source("test-qtle-misc")

qtle <- mockQTLE()

test_that("objectVersion works correctly", {
    expect_identical(objectVersion(qtle), packageVersion("QTLExperiment"))
})


test_that("mainExpName SETS correctly", {
    mainExpName(qtle) <- "test-that"
    expect_equal(int_metadata(qtle)$mainExpName, "test-that")
    mainExpName(qtle) <- NULL
    expect_null(int_metadata(qtle)$mainExpName)
})

test_that("mainExpName GETS correctly", {
    mainExpName(qtle) <- "test-that"
    expect_equal(mainExpName(qtle), "test-that")
    mainExpName(qtle) <- NULL
    expect_null(mainExpName(qtle))
})
