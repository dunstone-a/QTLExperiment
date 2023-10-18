# Test some internal functions
# library(testthat)
# source("setup.R");

test_that("Test internal rowData", {
    
    combined <- cbind(rowData(mock), int_rowData(mock))
    expect_equivalent(rowData(mock, internal = TRUE), combined)
    
    int_rowData(mock) <- cbind(int_rowData(mock), rowData(mock))
    
    expect_warning(rowData(mock, internal = TRUE),
        "overlapping names in internal and external rowData (variant_id, feature_id)",
        fixed = TRUE)
})

test_that("Test internal colData", {
    
    combined <- cbind(colData(mock), int_colData(mock))
    expect_equivalent(colData(mock, internal = TRUE), combined)
    
    int_colData(mock) <- cbind(int_colData(mock), colData(mock))
    
    expect_warning(colData(mock, internal = TRUE),
        "overlapping names in internal and external colData (state_id, sample_size)",
        fixed = TRUE)
})
