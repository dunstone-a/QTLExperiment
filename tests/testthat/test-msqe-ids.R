# Checks the state and test wise summarize functions
# library(QTLExperiment); library(testthat)
# source("setup.R"); source("test-qtle-assays")

set.seed(49)
qtle <- mockQTLE()


test_that("Test that id GETS work correctly", {
    expect_equivalent(feature_id(qtle), rowData(qtle)$feature_id)
    expect_equivalent(variant_id(qtle), rowData(qtle)$variant_id)

})

test_that("Test that id SETS work correctly", {
    qtle2 <- qtle
    new_features <- sample(LETTERS, nrow(qtle), replace=TRUE)

    feature_id(qtle2) <- new_features
    expect_equivalent(new_features, rowData(qtle2)$feature_id)
    expect_equivalent(rowData(qtle2)$feature_id,
                      gsub("\\|.*", "", row.names(qtle2)))

})


test_that("Test that internal ids stay correct", {
    qtle2 <- qtle
    new_features <- sample(LETTERS, nrow(qtle), replace=TRUE)

    rowData(qtle2)$feature_id <- new_features

    feature_id(qtle2) <- new_features
    expect_equivalent(new_features, rowData(qtle2)$feature_id)
    expect_equivalent(rowData(qtle2)$feature_id,
                      gsub("\\|.*", "", row.names(qtle2)))

})

