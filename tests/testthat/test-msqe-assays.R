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
    qtle <- QTLExperiment(
        assays=list(betas=b, errors=se, pvalues=p, lfsrs=p),
        state_id=state_ids,
        feature_id=feature_ids,
        variant_id=variant_ids)

    expect_equivalent(betas(qtle), b)
    expect_equivalent(errors(qtle), se)
    expect_equivalent(pvalues(qtle), p)
    expect_equivalent(lfsrs(qtle), p)
})

test_that("Test that assay SETS work correctly", {
    qtle <- mockQTLE()
    qtle2 <- qtle
    betas(qtle2) <- betas(qtle) * 2
    expect_equivalent(betas(qtle)*2 , betas(qtle2))

    shuff <- errors(qtle)[, sample(1:ncol(qtle))]
    colnames(shuff) <- colnames(qtle)
    errors(qtle2) <- shuff
    expect_equivalent(dim(qtle2), dim(qtle))

    lfsrs(qtle2) <- NULL
    expect_false("lfsrs" %in% names(assays(qtle2)))

    pvalues(qtle2) <- NULL
    pvalues(qtle2) <- pvalues(qtle)
    expect_equivalent(pvalues(qtle), pvalues(qtle2))
})


test_that("Test that assay manual set work correctly", {
    qtle <- mockQTLE()
    assay(qtle, "beta2") <- betas(qtle)*2

    expect_equivalent(betas(qtle)*2, assay(qtle, "beta2"))
})
