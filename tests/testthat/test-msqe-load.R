# Checks the row-wise combining methods.
# library(testthat)
# source("setup.R"); source("test-mash-2-msqe.R")

input <- list(lung="/mnt/mcfiles/Datasets/GTEx/GTEx_Analysis_v8_eQTL/Lung.v8.egenes.txt.gz",
              thyroid="/mnt/mcfiles/Datasets/GTEx/GTEx_Analysis_v8_eQTL/Thyroid.v8.egenes.txt.gz",
              spleen="/mnt/mcfiles/Datasets/GTEx/GTEx_Analysis_v8_eQTL/Spleen.v8.egenes.txt.gz")


input_web <- list(lung="http://ftp.ebi.ac.uk/pub/databases/spot/eQTL/sumstats/GTEx/tx/GTEx_tx_lung.all.tsv.gz",
              thyroid="http://ftp.ebi.ac.uk/pub/databases/spot/eQTL/sumstats/GTEx/tx/GTEx_tx_thyroid.all.tsv.gz",
              spleen="http://ftp.ebi.ac.uk/pub/databases/spot/eQTL/sumstats/GTEx/tx/GTEx_tx_spleen.all.tsv.gz")


test_that("Test that mash SET data can be coerced to msqe", {
  web <- summaryStats_2_msqe(input_web, feature_id="molecular_trait_id",
                              variant_id="variant", betas = "beta",
                              error = "se", pval = "pvalue", n_max=100)

  expect_equivalent(betas(msqe), betas(msqe2))
  expect_error(mash_2_msqe(simdata), "Must specify sep or rowData.")
})


test_
