#' @title Mock data for the QTLExperiment object
#'
#' @description
#' Functions to create fake input data for QTLExperiments.
#'
#' @param nStates Number of states
#' @param nQTL Number of QTL associations
#' @param names Logical to include column and row names
#'
#' @docType methods
#' @rdname mock-data
#'
#' @author Christina B Azodi
#' @export
#'
#' @importFrom stats prcomp

mockQTLE <- function(nStates = 10, nQTL = 100, names=TRUE){

  sumstats <- mockSummaryStats(nStates=nStates, nQTL=nQTL,
                               names=names)

  object <- QTLExperiment(assay = list(betas=sumstats$betas,
                                       errors=sumstats$errors,
                                       pvalues=sumstats$pvalues))

  colData(object)$sample_size <- sample(seq(60,120), ncol(object))
  mainExpName(object) <- "mock-example"

  object
}

#' @param nStates Number of states
#' @param nQTL Number of QTL associations
#' @param names Logical to include column and row names
#'
#' @importFrom stats rnorm runif
#' @rdname mock-data
#' @export
#'

mockSummaryStats <- function(nStates = 10, nQTL = 100, names=TRUE){

  betas <- matrix(rnorm(nStates * nQTL), ncol=nStates)
  errors <- matrix(abs(rnorm(nStates * nQTL)), ncol=nStates)
  pvalues <- runif(n=(nStates-1) * nQTL, min=1e-12, max=1)
  pvalues <- c(pvalues, runif(n=nQTL, min=1e-12, max=0.1))
  pvalues <- sample(pvalues)
  pvalues <- matrix(pvalues, ncol=nStates)

  if(names){
    test_ids <- paste(sample(c("geneA", "geneB", "geneC"), nQTL, replace=TRUE),
                      paste0("snp", sample(seq(1e3:1e5), nQTL)), sep="|")
    state_ids <- paste0("state", seq(1, nStates))
    row.names(betas) <- test_ids
    colnames(betas) <- state_ids
    row.names(errors) <- test_ids
    colnames(errors) <- state_ids
    row.names(pvalues) <- test_ids
    colnames(pvalues) <- state_ids
  }

  return(list(betas=betas, errors=errors, pvalues=pvalues))
}


#' Mock mashr data
#'
#' @param nStates Number of states
#' @param nQTL Number of QTL associations
#'
#' @rdname mock-data
#' @export
#'

mockMASHR <- function(nStates = 10, nQTL = 100){

  sumstats <- mockSummaryStats(nStates = nStates, nQTL = nQTL)

  list(B = sumstats$betas, Bhat = sumstats$betas, Shat=sumstats$errors)
}

#' Mock mashr data after fitting
#'
#' @param nStates Number of states
#' @param nQTL Number of QTL associations
#'
#' @rdname mock-data
#' @export
#'

mockMASHR_FIT <- function(nStates = 10, nQTL = 100){

  sumstats <- mockSummaryStats(nStates = nStates, nQTL = nQTL)
  result <- list(PosteriorMean = sumstats$betas,
                  PosteriorSD = sumstats$betas,
                  lfsr = sumstats$pvalues)
  list(result = result)
}
