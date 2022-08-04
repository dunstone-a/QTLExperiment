# Checks the row-wise combining methods.
# library(mashr); library(testthat)
# source("setup.R"); source("test-mash-2-msqe.R")

nTests <- 500
nStates <- 5
simdata <- mashr::simple_sims(nTests, nStates, 1)
data <- mashr::mash_set_data(simdata$Bhat, simdata$Shat)
U.c <- mashr::cov_canonical(data)
m.c <- mashr::mash(data, U.c)
simRowData <- DataFrame(list(feature_id = paste0("F", 1:nTests),
                             variant_id = sample(1:1e10, nTests)))

test_that("Test that mash SET data can be coerced to msqe", {
  msqe <- mash_2_msqe(simdata, sep="_")
  msqe2 <- mash_2_msqe(simdata, rowData=simRowData)

  expect_equivalent(betas(msqe), betas(msqe2))
  expect_error(mash_2_msqe(simdata), "Must specify sep or rowData.")
})

test_that("Test that mash FIT data can be coerced to msqe", {
  msqe <- mash_2_msqe(m.c, sep="_")
  msqe2 <- mash_2_msqe(m.c, rowData=simRowData)

  expect_equivalent(betas(msqe), betas(msqe2))
  expect_error(mash_2_msqe(simdata), "Must specify sep or rowData.")
})
