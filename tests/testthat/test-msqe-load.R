# Checks the row-wise combining methods.
# library(testthat)
# source("setup.R"); source("test-mash-2-qtle.R")

ebi <- read.csv("https://raw.githubusercontent.com/eQTL-Catalogue/eQTL-Catalogue-resources/master/data_tables/dataset_metadata.tsv", sep = "\t")
ftp <- "http://ftp.ebi.ac.uk/pub/databases/spot/eQTL/sumstats"

# Filter to select the tissue and make url's
ebi <- ebi |>
    dplyr::filter(study_label == "GTEx") |>
    dplyr::filter(tissue_label %in% c("lung", "thyroid", "spleen")) |>
    dplyr::filter(quant_method == "tx") |>
    dplyr::mutate(path = paste(ftp, study_id, dataset_id, dataset_id, sep = "/")) |>
    dplyr::mutate(path = paste0(path, ".cc.tsv.gz")) %>%
    dplyr::rename(state = tissue_label)

# Data frame version
input_df <- ebi |>
    dplyr::select(state, path)


# List version
input_list <- as.list(setNames(ebi$path, ebi$tissue_label))

test_that("Test that mash SET data (as list) can be coerced to qtle", {
    web <- sumstats2qtle(input_list, feature_id="molecular_trait_id",
                         variant_id="variant", betas = "beta",
                         errors="se", pvalues="pvalue", n_max=100)

    expect_equivalent(class(web), "QTLExperiment")
})

test_that("Test that mash SET data (as data.frame) can be coerced to qtle", {
    web <- sumstats2qtle(input_df, feature_id="molecular_trait_id",
                         variant_id="variant", betas = "beta",
                         errors="se", pvalues="pvalue", n_max=100)

    expect_equivalent(class(web), "QTLExperiment")
})

test_that("Test that mash SET data frames with additional colData can be coerced to qtle", {
    web <- sumstats2qtle(ebi, feature_id="molecular_trait_id",
                         variant_id="variant", betas = "beta",
                         errors="se", pvalues="pvalue", n_max=100)

    expect_equivalent(class(web), "QTLExperiment")
})

