# Checks the row-wise combining methods.
# source("setup.R"); source("test-mash-2-qtle.R")

nTests <- 500
nStates <- 5
simdata <- mockMASHR(nStates = nStates, nQTL = nTests)
m.c <- mockMASHR_FIT(nStates = nStates, nQTL = nTests)
simRowData <- DataFrame(list(feature_id = paste0("F", 1:nTests),
                             variant_id = sample(1:1e10, nTests)))

test_that("Test that mash SET data can be coerced to qtle", {
  qtle <- mash_2_qtle(simdata, sep="\\|")
  qtle2 <- mash_2_qtle(simdata, rowData=simRowData)

  expect_equivalent(betas(qtle), betas(qtle2))
  expect_error(mash_2_qtle(simdata), "Must specify sep or rowData.")
})

test_that("Test that mash FIT data can be coerced to qtle", {
  qtle <- mash_2_qtle(m.c, sep="\\|")
  qtle2 <- mash_2_qtle(m.c, rowData=simRowData)

  expect_equivalent(betas(qtle), betas(qtle2))
  expect_error(mash_2_qtle(simdata), "Must specify sep or rowData.")
})
