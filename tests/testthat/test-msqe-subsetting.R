# Checks the subsetting methods
# library(QTLExperiment); library(testthat)
# source("setup.R"); source("test-qtle-subsetting.R")

context("QTLExperiment subsetting")
qtle <- mockQTLE()

test_that("subsetting by row works correctly", {
    int_rowData(qtle)$indicator <- seq(nrow(qtle))
    for (i in 1:3) {
        if (i==1L) {
            by.row <- sample(nrow(qtle), 20)
            sub.qtle <- qtle[by.row,]
            expect_identical(int_rowData(sub.qtle)$indicator, by.row)
        } else if (i==2L) {
            by.row <- rbinom(nrow(qtle), 1, 0.2)==1
            sub.qtle <- qtle[by.row,]
            expect_identical(int_rowData(sub.qtle)$indicator, which(by.row))
        } else if (i==3L) {
            by.row <- rownames(qtle)[sample(nrow(qtle), 100)]
            sub.qtle <- qtle[by.row,]
            expect_identical(int_rowData(sub.qtle)$indicator, match(by.row,
                                                                    rownames(qtle)))
        }
        ind <- int_rowData(sub.qtle)$indicator

        expect_identical(assay(qtle)[ind,,drop=FALSE], assay(sub.qtle))
        expect_identical(rowData(qtle)[ind,], rowData(sub.qtle))
        expect_identical(int_colData(sub.qtle), int_colData(qtle))
        expect_identical(objectVersion(sub.qtle), objectVersion(qtle))
    }

    expect_error(qtle[nrow(qtle)+1,], "subscript contains out-of-bounds indices",
                 fixed=TRUE)
    expect_error(qtle["A",], "index out of bounds: A")
})

test_that("subsetting by column works correctly", {

    colData(qtle)$indicator <- seq(ncol(qtle))
    for (j in 1:3) {
        if (j==1L) {
            by.col <- sample(ncol(qtle), 3)
            sub.qtle <- qtle[,by.col]
            expect_identical(colData(sub.qtle)$indicator, by.col)
        } else if (j==2L) {
            by.col <- rbinom(ncol(qtle), 1, 0.2)==1
            sub.qtle <- qtle[,by.col]
            expect_identical(colData(sub.qtle)$indicator, which(by.col))
        } else if (j==3L) {
            by.col <- colnames(qtle)[sample(ncol(qtle), 8)]
            sub.qtle <- qtle[,by.col]
            expect_identical(colData(sub.qtle)$indicator, match(by.col,
                                                                colnames(qtle)))
        }
        ind <- colData(sub.qtle)$indicator
        # check SE elements are subsetted.
        expect_identical(assay(qtle)[,ind,drop=FALSE], assay(sub.qtle))

        # Unchanged elements:
        expect_identical(int_rowData(sub.qtle), int_rowData(qtle))
        expect_identical(rowData(sub.qtle), rowData(qtle))
        expect_identical(objectVersion(sub.qtle), objectVersion(qtle))
    }

    expect_error(qtle[,ncol(qtle)+1], "subscript contains out-of-bounds indices",
                 fixed=TRUE)
    expect_error(qtle[,"A"], "index out of bounds: A")
})

test_that("subset replacement by row works correctly for basic cases", {
    qtle.alt <- qtle
    feature_id(qtle.alt) <- paste0(feature_id(qtle), "x")
    int_metadata(qtle.alt)$whee <- 1

    to <- 1:10
    from <- 21:30
    qtle.alt[to,] <- qtle[from,]

    # Unchanged elements, to name a few.
    expect_identical(assay(qtle.alt)[to,,drop=FALSE], assay(qtle)[from,,drop=FALSE])
    expect_equivalent(assay(qtle.alt)[-to,,drop=FALSE], assay(qtle)[-to,,drop=FALSE])
    expect_identical(int_metadata(qtle.alt), int_metadata(qtle))
    expect_identical(int_colData(qtle.alt), int_colData(qtle))
    expect_identical(colData(qtle.alt), colData(qtle))

    ## Works for string row.names
    #qtle.alt2 <- qtle
    #feature_id(qtle.alt2) <- paste0(feature_id(qtle), "x")
    #int_metadata(qtle.alt2)$whee <- 1
    #to <- row.names(qtle.alt2)[1:10]
    #from <- row.names(qtle)[21:30]
    #qtle.alt2[to,] <- qtle[from,]
    #expect_equal(qtle.alt, qtle.alt2)
})

test_that("subset replacement checks for duplicate feature|variant pairs", {
    to <- 1:10
    from <- 11:20
    qtlex <- qtle
    expect_error(qtlex[to, ] <- qtle[from, ],
                 "invalid class .QTLExperiment. object: test_ids: duplicate feature|variant rows")
})

test_that("subset replacement by column works correctly for basic cases", {
    qtlex <- qtle
    state_id(qtlex) <- paste0(state_id(qtlex), "_x")
    to <- 1:5
    from <- 6:10
    qtlex[, to] <- qtle[, from]

    expect_identical(assay(qtlex)[,to,drop=FALSE], assay(qtle)[,from,drop=FALSE])
    expect_equivalent(assay(qtlex)[,-to,drop=FALSE], assay(qtle)[,-to,drop=FALSE])


    # Unchanged elements.
    expect_identical(int_rowData(qtlex), int_rowData(qtle))
    expect_identical(rowData(qtlex), rowData(qtle))
    expect_identical(int_metadata(qtlex), int_metadata(qtle))
})


test_that("subset replacement by both rows and columns work correctly", {
    # Wholesale replacement!
    qtlex <- qtle
    feature_id(qtlex) <- paste0(feature_id(qtle), "x")
    state_id(qtlex) <- paste0(state_id(qtle), "x")
    int_metadata(qtlex)$whee <- 1

    # Partial replacement.
    to <- 1:5
    from <- 6:10

    qtlex[to,to] <- qtle[from,from]

    ref <- qtlex
    ref[to,] <- qtle[from,]
    expect_identical(int_rowData(ref), int_rowData(qtlex))

    ref <- qtlex
    ref[,to] <- qtle[,from]
    expect_identical(int_colData(ref), int_colData(qtlex))
})

test_that("S4Vectors subsetting works correctly", {
    out <- extractROWS(qtle, 1:10)
    expect_identical(out, qtle[1:10,])

    set.seed(100)
    f <- sample(10, nrow(qtle), replace=TRUE)
    out <- split(qtle, f)
    expect_identical(out[["1"]], qtle[f==1,])
    expect_identical(out[["5"]], qtle[f==5,])
    expect_identical(out[["8"]], qtle[f==8,])
})
