# Checks the state and test wise summarize functions
# library(QTLExperiment); library(testthat)
# source("setup.R"); source("test-qtle-assays")

set.seed(49)
nStates <- 10
nQTL <- 100
b <- matrix(rnorm(1000), ncol=nStates)
se <- matrix(abs(rnorm(1000)), ncol=nStates)
p <- matrix(runif(1000), ncol=nStates)
feature_ids <- sample(LETTERS[seq(from = 1, to = 10)], nQTL, replace=TRUE)
variant_ids <- paste0("var", sample(seq(1e3:1e5), nQTL))
state_ids <- paste0("state_", 1:nStates)

test_that("Test that assay GETS work correctly", {
  qtle <- QTLExperiment(assay=list(betas=b, error=se, pval=p, lfsr=p),
                        state_id=state_ids,
                        feature_id=feature_ids,
                        variant_id=variant_ids)

  expect_equivalent(betas(qtle), b)
  expect_equivalent(error(qtle), se)
  expect_equivalent(pval(qtle), p)
  expect_equivalent(lfsr(qtle), p)
})

test_that("Test that assay SETS work correctly", {
  qtle <- mockQTLE()
  qtle2 <- qtle
  betas(qtle2) <- betas(qtle) * 2
  expect_equivalent(betas(qtle)*2 , betas(qtle2))

  shuff <- error(qtle)[, sample(1:ncol(qtle))]
  colnames(shuff) <- colnames(qtle)
  error(qtle2) <- shuff
  expect_equivalent(dim(qtle2), dim(qtle))

  lfsr(qtle2) <- NULL
  expect_false("lfsr" %in% names(assays(qtle2)))

  pval(qtle2) <- NULL
  pval(qtle2) <- pval(qtle)
  expect_equivalent(pval(qtle), pval(qtle2))
})


test_that("Test that assay manual set work correctly", {
  qtle <- mockQTLE()
  assay(qtle, "beta2") <- betas(qtle)*2

  expect_equivalent(betas(qtle)*2, assay(qtle, "beta2"))
})
