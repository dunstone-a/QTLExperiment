# Checks the state and test wise summarize functions
# library(multiStateQTLExperiment); library(testthat)
# source("setup.R"); source("test-msqe-assays")

set.seed(49)
nStates <- 10
nQTL <- 100
b <- matrix(rnorm(1000), ncol=nStates)
se <- matrix(abs(rnorm(1000)), ncol=nStates)
p <- matrix(runif(1000), ncol=nStates)
feature_ids <- sample(LETTERS[seq(from = 1, to = 10)], nQTL, replace=TRUE)
variant_ids <- paste0("var", sample(seq(1e3:1e5), nQTL))


test_that("Test that assay GETS work correctly", {
  msqe <- multiStateQTLExperiment(assay=list(betas=b, error=se, pval=p, lfsr=p),
                                  rowData=DataFrame(feature_id=feature_ids,
                                                    variant_id=variant_ids))

  expect_equivalent(betas(msqe), b)
  expect_equivalent(error(msqe), se)
  expect_equivalent(pval(msqe), p)
  expect_equivalent(lfsr(msqe), p)
})

test_that("Test that assay SETS work correctly", {
  msqe <- mockMSQE()
  msqe2 <- msqe
  betas(msqe2) <- betas(msqe) * 2
  expect_equivalent(betas(msqe)*2 , betas(msqe2))

  shuff <- error(msqe)[, sample(1:ncol(msqe))]
  colnames(shuff) <- colnames(msqe)
  error(msqe2) <- shuff
  expect_equivalent(dim(msqe2), dim(msqe))

  lfsr(msqe2) <- NULL
  expect_false("lfsr" %in% names(assays(msqe2)))

  pval(msqe2) <- NULL
  pval(msqe2) <- pval(msqe)
  expect_equivalent(pval(msqe), pval(msqe2))
})


test_that("Test that assay manual set work correctly", {
  msqe <- mockMSQE()
  assay(msqe, "beta2") <- betas(msqe)*2

  expect_equivalent(betas(msqe)*2, assay(msqe, "beta2"))
})
