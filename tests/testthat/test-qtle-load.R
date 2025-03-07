# Checks the row-wise combining methods.
# library(testthat)
# source("setup.R"); source("test-mash-2-qtle.R")

input_path <- system.file("extdata", package =  "QTLExperiment")
state <- c("lung", "thyroid", "spleen", "blood")

# Data frame version
input_df <- data.frame(state = state,
                       path = paste0(input_path, "/GTEx_tx_", state, ".tsv"),
                       source = "GTEx")

# List version
input_list <- as.list(setNames(input_df$path, input_df$state))


test_that("Test that mash SET data (as list) can be coerced to qtle", {
    data <- sumstats2qtle(input_list, feature_id="molecular_trait_id",
                         variant_id="variant", betas = "beta",
                         errors="se", pvalues="pvalue", n_max=100)

    expect_equivalent(class(data), "QTLExperiment")
})

test_that("Test that mash SET data (as data.frame) can be coerced to qtle", {
    data <- sumstats2qtle(input_df, feature_id="molecular_trait_id",
                         variant_id="variant", betas = "beta",
                         errors="se", pvalues="pvalue", n_max=100)

    expect_equivalent(class(data), "QTLExperiment")
})

test_that("Test that additional columns in data.frame append to colData", {
    data <- sumstats2qtle(input_df, feature_id="molecular_trait_id",
                         variant_id="variant", betas = "beta",
                         errors="se", pvalues="pvalue", n_max=100)

    expect_equivalent(colnames(colData(data)), c("state_id", "source"))
})

