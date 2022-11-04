# Checks for proper construction and get/setting QTLExperiment slots.
# library(QTLExperiment)
# library(testthat)
# source("setup.R")
# source("test-qtle-class.R")

context("QTLExperiment class")

test_that("construction of the QTLe works correctly - manual", {
  # With metadata explicitly provided
  qtle <- QTLExperiment(assay=list(betas=sumstats_noNames$betas,
                                   error=sumstats_noNames$errors,
                                   pval=sumstats_noNames$pvalues,
                                   lfsr=sumstats_noNames$pvalues),
                        state_id = state_ids,
                        feature_id = feature_ids,
                        variant_id = variant_ids)
  expect_equivalent(class(qtle), "QTLExperiment")
  expect_equivalent(assay(qtle, "betas"), sumstats_noNames$betas)
  expect_equivalent(assay(qtle, "error"), sumstats_noNames$errors)
  expect_equivalent(assay(qtle, "pval"), sumstats_noNames$pvalues)
  expect_equivalent(assay(qtle, "lfsr"), sumstats_noNames$pvalues)
  expect_equivalent(state_id(qtle), state_ids)
  expect_equivalent(feature_id(qtle), feature_ids)
  expect_equivalent(variant_id(qtle), variant_ids)

  # With metadata extracted from input data
  qtle <- QTLExperiment(assay=list(betas=sumstats$betas,
                                   error=sumstats$error,
                                   pval=sumstats$pvalues,
                                   lfsr=sumstats$pvalues))
  expect_equivalent(class(qtle), "QTLExperiment")
  expect_equivalent(assay(qtle, "betas"), sumstats$betas)
  expect_equivalent(state_id(qtle), colnames(sumstats$betas))
})


test_that("construction of the QTLe works correctly - from se", {
  se <- SummarizedExperiment(assays=list(betas=sumstats$betas,
                                         error=sumstats$errors))
  qtle <- as(se, "QTLExperiment")

  expect_equivalent(class(qtle), "QTLExperiment")
  expect_equivalent(assay(qtle, "betas"), sumstats$betas)
  expect_equivalent(assay(qtle, "error"), sumstats$errors)
})


test_that("QTLe valid check works correctly", {

  expect_error(QTLExperiment(assay=list(error=sumstats$errors,
                                        pval=sumstats$pvalues,
                                        lfsr=sumstats$pvalues),
                             state_id = state_ids,
                             feature_id=feature_ids,
                             variant_id=variant_ids),
               "betas: assay needed")

  expect_error(QTLExperiment(assay=list(betas=sumstats$betas,
                                        pval=sumstats$pvalues,
                                        lfsr=sumstats$pvalues),
                             state_id = state_ids,
                             feature_id=feature_ids,
                             variant_id=variant_ids),
               "error: assay needed")
})


test_that("QTLe feature IDs are provided or pulled from rownames of betas", {

  qtle2 <- QTLExperiment(assay=list(betas=sumstats$betas,
                                    error=sumstats$errors,
                                    pval=sumstats$pvalues,
                                    lfsr=sumstats$pvalues),
                         feature_id=feature_ids, variant_id=variant_ids)
  expect_equivalent(state_id(qtle2), state_ids,
                    colnames(sumstats$betas), colnames(qtle2),
                    colnames(betas(qtle2)), colnames(assay(qtle2, "lfsr")))


  expect_equivalent(feature_id(qtle2), feature_ids,
                    rowData(qtle2)$feature_id,
                    gsub("\\|.*", "", row.names(betas(qtle2))))

  expect_equivalent(variant_id(qtle2), variant_ids,
                    rowData(qtle2)$variant_id,
                    gsub(".*\\|", "", row.names(betas(qtle2))))
})

test_that("QTLE metadata ID checks are working", {
  expect_error(QTLExperiment(assay=list(betas=sumstats_noNames$betas,
                                        error=sumstats_noNames$errors),
                             state_id = state_ids, variant_id=variant_ids),
               "Feature/variant IDs should be provided as pipe separated string
             in rownames or using feature_id={...} and variant_id={...}.",
               fixed=TRUE)

  expect_error(QTLExperiment(assay=list(betas=sumstats_noNames$betas,
                                        error=sumstats_noNames$errors),
                             state_id = state_ids,
                             feature_id=feature_ids),
               "Feature/variant IDs should be provided as pipe separated string
             in rownames or using feature_id={...} and variant_id={...}.",
               fixed=TRUE)


  expect_error(QTLExperiment(assay=list(betas=sumstats_noNames$betas,
                                        error=sumstats_noNames$errors),
                             feature_id=feature_ids,
                             variant_id=variant_ids),
               "State IDs should be provided as colnames or state_id={...}.",
               fixed=TRUE)
})


