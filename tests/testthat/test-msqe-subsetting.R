# Checks the subsetting methods
# library(multiStateQTLExperiment); library(testthat)
# source("setup.R"); source("test-msqe-subsetting.R")

msqe <- mockMSQE()
d1 <- reducedDim(msqe, "PCA", withDimnames=FALSE)

test_that("subsetting by row works correctly", {
  int_rowData(msqe)$indicator <- seq_len(nrow(msqe))
  reducedDim(msqe, "PCA") <- d1
  for (i in 1:3) {
    if (i==1L) {
      by.row <- sample(nrow(msqe), 20)
      sub.msqe <- msqe[by.row,]
      expect_identical(int_rowData(sub.msqe)$indicator, by.row)
    } else if (i==2L) {
      by.row <- rbinom(nrow(msqe), 1, 0.2)==1
      sub.msqe <- msqe[by.row,]
      expect_identical(int_rowData(sub.msqe)$indicator, which(by.row))
    } else if (i==3L) {
      by.row <- rownames(msqe)[sample(nrow(msqe), 100)]
      sub.msqe <- msqe[by.row,]
      expect_identical(int_rowData(sub.msqe)$indicator, match(by.row, rownames(msqe)))
    }
    ind <- int_rowData(sub.msqe)$indicator

    expect_identical(assay(msqe)[ind,,drop=FALSE], assay(sub.msqe))
    expect_identical(rowData(msqe)[ind,], rowData(sub.msqe))
    expect_identical(reducedDims(sub.msqe), reducedDims(msqe))
    expect_identical(int_colData(sub.msqe), int_colData(msqe))
    expect_identical(objectVersion(sub.msqe), objectVersion(msqe))
  }

  expect_error(msqe[nrow(msqe)+1,], "subscript contains out-of-bounds indices", fixed=TRUE)
  expect_error(msqe["A",], "index out of bounds: A")
})

test_that("subsetting by column works correctly", {

  colData(msqe)$indicator <- seq_len(ncol(msqe))
  reducedDim(msqe, "PCA") <- d1
  for (j in 1:3) {
    if (j==1L) {
      by.col <- sample(ncol(msqe), 3)
      sub.msqe <- msqe[,by.col]
      expect_identical(colData(sub.msqe)$indicator, by.col)
    } else if (j==2L) {
      by.col <- rbinom(ncol(msqe), 1, 0.2)==1
      sub.msqe <- msqe[,by.col]
      expect_identical(colData(sub.msqe)$indicator, which(by.col))
    } else if (j==3L) {
      by.col <- colnames(msqe)[sample(ncol(msqe), 8)]
      sub.msqe <- msqe[,by.col]
      expect_identical(colData(sub.msqe)$indicator, match(by.col, colnames(msqe)))
    }
    ind <- colData(sub.msqe)$indicator

    expect_identical(assay(msqe)[,ind,drop=FALSE], assay(sub.msqe)) # check SE elements are subsetted.

    expect_identical(reducedDim(sub.msqe, "PCA", withDimnames=FALSE), d1[ind,,drop=FALSE])

    # Unchanged elements:
    expect_identical(int_rowData(sub.msqe), int_rowData(msqe))
    expect_identical(rowData(sub.msqe), rowData(msqe))
    expect_identical(objectVersion(sub.msqe), objectVersion(msqe))
  }

  expect_error(msqe[,ncol(msqe)+1], "subscript contains out-of-bounds indices", fixed=TRUE)
  expect_error(msqe[,"A"], "index out of bounds: A")
})

test_that("subset replacement by row works correctly for basic cases", {
  msqe.alt <- msqe
  rownames(msqe.alt) <- paste0(rownames(msqe), "x")
  int_metadata(msqe.alt)$whee <- 1

  msqex <- msqe.alt
  to <- 1:10
  from <- 21:30
  msqex[to,] <- msqe[from,]

  expect_identical(assay(msqex)[to,,drop=FALSE], assay(msqe)[from,,drop=FALSE])
  expect_equivalent(assay(msqex)[-to,,drop=FALSE], assay(msqe)[-to,,drop=FALSE])

  # Unchanged elements, to name a few.
  # expect_identical(int_metadata(msqex), int_metadata(msqe)) # I don't know why you would want that to be the same?
  expect_identical(int_colData(msqex), int_colData(msqe))
  expect_identical(colData(msqex), colData(msqe))

  # Again for character
  msqex2 <- msqe.alt
  to <- rownames(msqex2)[1:10]
  from <- rownames(msqe)[21:30]
  msqex2[to,] <- msqe[from,]
  expect_equal(msqex, msqex2)
})

test_that("subset replacement by row handles internal fields correctly", {
  to <- 1:10
  from <- 21:30

  # Handles mismatch.
  msqex2 <- msqe
  int_rowData(msqex2)$ERCC <- seq_len(nrow(msqex2))
  expect_error(msqex2[to,] <- msqe[from,], "'int_rowData'")
})

test_that("subset replacement by column works correctly for basic cases", {
  reducedDim(msqe, "PCA") <- d1
  msqe.alt <- msqe
  colnames(msqe.alt) <- paste0(colnames(msqe), "x")

  msqex <- msqe.alt
  to <- 1:5
  from <- 6:10
  msqex[,to] <- msqe[,from]

  expect_identical(assay(msqex)[,to,drop=FALSE], assay(msqe)[,from,drop=FALSE])
  expect_equivalent(assay(msqex)[,-to,drop=FALSE], assay(msqe)[,-to,drop=FALSE])
  expect_equivalent(reducedDim(msqex[-to], "PCA", withDimnames=FALSE),
                    rbind(reducedDim(msqe[,from], "PCA", withDimnames=FALSE),
                          reducedDim(msqe[,from], "PCA", withDimnames=FALSE)))

  # Unchanged elements.
  expect_identical(int_rowData(msqex), int_rowData(msqe))
  expect_identical(rowData(msqex), rowData(msqe))
  expect_identical(int_metadata(msqex), int_metadata(msqe))
})

test_that("subset replacement by column handles internal fields", {
  to <- 1:5
  from <- 6:10

  # Throws an error upon mismatch.
  msqex2 <- msqe
  int_colData(msqex2)$reducedDims <- NULL
  expect_error(msqex2[,to] <- msqe[,from], "'int_colData'")
})

test_that("subset replacement by both rows and columns work correctly", {
  # Wholesale replacement!
  msqe.alt <- msqe
  rownames(msqe.alt) <- paste0(rownames(msqe), "x")
  colnames(msqe.alt) <- paste0(colnames(msqe), "x")
  int_metadata(msqe.alt)$whee <- 1

  msqex <- msqe.alt
  msqex[] <- msqe
  expect_equal(msqex, msqe)

  # Partial replacement.
  to <- 1:5
  from <- 6:10

  msqex <- msqe.alt
  msqex[to,to] <- msqe[from,from]

  ref <- msqe.alt
  ref[to,] <- msqe[from,]
  expect_identical(int_rowData(ref), int_rowData(msqex))

  ref <- msqe.alt
  ref[,to] <- msqe[,from]
  expect_identical(int_colData(ref), int_colData(msqex))
})

test_that("S4Vectors subsetting works correctly", {
  out <- extractROWS(msqe, 1:10)
  expect_identical(out, msqe[1:10,])

  set.seed(100)
  f <- sample(10, nrow(msqe), replace=TRUE)
  out <- split(msqe, f)
  expect_identical(out[["1"]], msqe[f==1,])
  expect_identical(out[["5"]], msqe[f==5,])
  expect_identical(out[["8"]], msqe[f==8,])
})
