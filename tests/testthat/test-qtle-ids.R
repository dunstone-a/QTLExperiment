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
    new_features <- LETTERS[1:length(unique(feature_id(qtle)))]
    
    idx <- match(feature_id(qtle), paste0("gene", LETTERS[1:3]))
    
    feature_id(qtle2) <- new_features[idx]
    
    # Test that features were updated
    expect_equivalent(new_features[idx], rowData(qtle2)$feature_id)
    
    # Test that row.names() matches rowData() feature_id. 
    expect_equivalent(rowData(qtle2)$feature_id,
                      gsub("\\|.*", "", row.names(qtle2)))

})


