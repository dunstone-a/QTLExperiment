# Checks the object validity
# library(QTLExperiment); library(testthat)
# source("setup.R");

context("QTLExperiment validity")
qtle <- mockQTLE()


test_that("subset replacement checks for duplicate feature|variant pairs", {
    to <- 1:10
    from <- 11:20
    qtlex <- qtle
    expect_error(
        qtlex[to, ] <- qtle[from, ],
        paste0(
            "invalid class ",
            dQuote("QTLExperiment"),
            " object: test_ids: duplicate feature|variant rows"))
})