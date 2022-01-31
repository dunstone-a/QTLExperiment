# Checks for proper functioning of the reducedDim methods.
# library(SingleCellExperiment); library(testthat); 
# source("setup.R"); source("test-msqe-reducedDims.R")

msqe <- mockMSQE()
d1 <- reducedDim(msqe, "PCA", withDimnames=FALSE)
d2 <- reducedDim(msqe, "tSNE", withDimnames=FALSE)

test_that("reducedDim getters/setters are functioning with character 'type'", {
  reducedDim(msqe, "PCA") <- d1
  expect_identical(reducedDim(msqe, "PCA", withDimnames=FALSE), d1)
  expect_identical(reducedDims(msqe, withDimnames=FALSE), SimpleList(PCA=d1))
  expect_identical(reducedDimNames(msqe), "PCA")
  
  reducedDim(msqe, "tSNE") <- d2
  expect_identical(reducedDim(msqe, "tSNE", withDimnames=FALSE), d2)
  expect_identical(reducedDims(msqe, withDimnames=FALSE), 
                   SimpleList(PCA=d1, tSNE=d2))
  expect_identical(reducedDimNames(msqe), c("PCA", "tSNE"))
  
  # Clearing values.
  reducedDim(msqe, "PCA") <- NULL
  expect_identical(reducedDim(msqe, "tSNE", withDimnames=FALSE), d2)
  expect_identical(reducedDim(msqe, withDimnames=FALSE), d2)
  expect_identical(reducedDims(msqe, withDimnames=FALSE), SimpleList(tSNE=d2))
  expect_identical(reducedDimNames(msqe), "tSNE")
  
  # Checking for different errors.
  expect_error(reducedDim(msqe, "PCA"), "invalid subscript") 
  expect_error(reducedDim(msqe, 2), "invalid subscript") 
  expect_error(reducedDim(msqe, "DM") <- d1[1:8,], "number of rows")
  expect_error(reducedDim(msqe, 1) <- "huh", "number of rows")
})

test_that("reducedDims getters/setters are functioning", {
  reducedDims(msqe) <- list(PCA=d1, TSNE=d2)
  expect_identical(reducedDimNames(msqe), c("PCA", "TSNE"))
  expect_identical(reducedDim(msqe, "PCA", withDimnames=FALSE), d1)
  expect_identical(reducedDim(msqe, 1, withDimnames=FALSE), d1)
  expect_identical(reducedDim(msqe, "TSNE", withDimnames=FALSE), d2)
  expect_identical(reducedDim(msqe, 2, withDimnames=FALSE), d2)
  
  # Clearing via empty List.
  alt <- msqe
  reducedDims(alt) <- SimpleList()
  expect_identical(reducedDims(alt), setNames(SimpleList(), character(0)))
  
  # Clearing via NULL.
  reducedDims(msqe) <- SimpleList(DM=d1)
  expect_identical(SimpleList(DM=d1), reducedDims(msqe, withDimnames=FALSE))
  expect_identical(d1, reducedDim(msqe, withDimnames=FALSE))
  
  alt <- msqe
  reducedDims(alt) <- NULL
  expect_identical(reducedDims(alt), setNames(SimpleList(), character(0)))
  
  # Setting with an unnamed list works.
  expect_warning(reducedDims(msqe) <- list(d1, d2), "NULL")
  expect_identical(reducedDimNames(msqe), c("unnamed1", "unnamed2"))
  
  expect_warning(reducedDims(msqe) <- list(X=d1, d2), "empty")
  expect_identical(reducedDimNames(msqe), c("X", "unnamed1"))
  
  # Checking for errors.
  expect_error(reducedDims(msqe) <- list(d1, d2[1:8,]), "number of rows")
  expect_error(reducedDims(msqe) <- list(d1[1:8,], d2[1:8,]), "number of rows")
})

test_that("getters/setters respond to dimnames", {
  named <- msqe
  colnames(named) <- paste0("NewStates", seq_len(ncol(named)))
  reducedDims(named) <- list(PCA=d1, TSNE=d2)
  
  expect_identical(rownames(reducedDim(named)), colnames(named))
  expect_identical(rownames(reducedDim(named, 2)), colnames(named))
  expect_identical(rownames(reducedDim(named, withDimnames=FALSE)), NULL)
  
  out <- reducedDims(named)
  expect_identical(rownames(out[[1]]), colnames(named))
  expect_identical(rownames(out[[2]]), colnames(named))
  out <- reducedDims(named, withDimnames=FALSE)
  expect_identical(rownames(out[[1]]), NULL)
  expect_identical(rownames(out[[2]]), NULL)
  
  # withDimnames works on the left hand side.
  rownames(reducedDim(named, "PCA", withDimnames=FALSE)) <- toupper(colnames(named))
  expect_identical(rownames(reducedDim(named)), colnames(named))

  names(reducedDims(named, withDimnames=FALSE)) <- c("alpha", "bravo")
  expect_identical(rownames(reducedDim(named, withDimnames=FALSE)), toupper(colnames(named)))
  
  # No warning when names are the same.
  d1.2 <- d1
  rownames(d1.2) <- colnames(named)
  expect_warning(reducedDim(named) <- d1.2, NA)
  
  # withDimnames raises warnings on non-identity.
  d1.2 <- d1
  rownames(d1.2) <- toupper(colnames(named))
  expect_error(reducedDim(named, "PCA") <- d1.2, "should be the same")
  expect_error(reducedDims(named) <- list(PCA=d1.2), "should be the same")
  expect_warning(reducedDim(named, "PCA") <- d1, NA)
})

test_that("reducedDims getters/setters preserve mcols and metadata", {
  stuff <- List(PCA=d1, TSNE=d2)
  mcols(stuff)$A <- c("one", "two")
  metadata(stuff)$B <- "three"
  
  reducedDims(msqe) <- stuff
  out <- reducedDims(msqe)
  expect_identical(mcols(out), mcols(stuff))
  expect_identical(metadata(out), metadata(stuff))
})

test_that("reducedDim setter creates an unnamed redDim is none are present", {
  # In the absence of of redDim, create an unnamed one (like reducedDims does)
  reducedDim(msqe) <- d1
  expect_identical(reducedDimNames(msqe), "unnamed1")
})

test_that("reducedDim getters/setters work with numeric indices", {
  expect_error(reducedDim(msqe), "no available entries") 
  expect_error(reducedDim(msqe, 2), "invalid subscript 'type'") 
  expect_error(reducedDim(msqe, "PCA"), "invalid subscript") 
  
  expect_error(reducedDim(msqe, 1) <- d1, "out of bounds")
  expect_error(reducedDim(msqe, 2) <- d1, "out of bounds")
  
  # This gets a bit confusing as the order changes when earlier elements are wiped out.
  expect_warning(reducedDims(msqe) <- list(d1, d2), "NULL")
  expect_identical(reducedDim(msqe, withDimnames=FALSE), d1)
  expect_identical(reducedDim(msqe, 2, withDimnames=FALSE), d2)
  expect_identical(reducedDimNames(msqe), c("unnamed1", "unnamed2"))
  
  mult <- d1 * 5
  reducedDim(msqe, "PCA") <- mult # d1 is the second element.
  expect_identical(reducedDim(msqe, 1, withDimnames=FALSE), d1)
  expect_identical(reducedDim(msqe, 2, withDimnames=FALSE), d2)
  expect_identical(reducedDim(msqe, 3, withDimnames=FALSE), mult)
  expect_identical(reducedDimNames(msqe), c("unnamed1", "unnamed2", "PCA"))
  
  reducedDim(msqe, 1) <- NULL # d2 becomes the first element now.
  expect_identical(reducedDim(msqe, withDimnames=FALSE), d2)
  expect_identical(reducedDim(msqe, 1, withDimnames=FALSE), d2)
  expect_identical(reducedDim(msqe, 2), reducedDim(msqe, "PCA"))
  expect_identical(reducedDimNames(msqe), c("unnamed2", "PCA"))
  
  reducedDim(msqe) <- NULL # 'mult' becomes the first element.
  expect_identical(reducedDim(msqe, withDimnames=FALSE), mult)
  expect_identical(reducedDimNames(msqe), "PCA")
  reducedDim(msqe) <- d2 # d2 now overwrites the first element.
  expect_identical(reducedDim(msqe, 1, withDimnames=FALSE), d2)
  expect_identical(reducedDimNames(msqe), "PCA")
  
  expect_error(reducedDim(msqe, 5) <- d1, "out of bounds")
})

test_that("reducedDimNames getters/setters work correctly", {
  expect_warning(reducedDims(msqe) <- list(d1, d2), "NULL")
  expect_identical(reducedDimNames(msqe), c("unnamed1", "unnamed2"))
  reducedDims(msqe) <- list(PCA=d1, TSNE=d2)
  expect_identical(reducedDimNames(msqe), c("PCA", "TSNE"))
  
  # Directly setting.
  reducedDimNames(msqe) <- c("A", "B")
  expect_identical(reducedDimNames(msqe), c("A", "B"))
  
  # Responds to empty names.
  expect_warning(reducedDimNames(msqe) <- c("X", ""), "empty")
  expect_identical(reducedDimNames(msqe), c("X", "unnamed1"))
  
  # When wiped.
  reducedDims(msqe) <- NULL
  expect_identical(reducedDimNames(msqe), character(0))
  
  expect_error(reducedDimNames(msqe) <- c("A", "B"), "more column names")
})