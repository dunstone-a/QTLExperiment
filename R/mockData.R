#' Mock MSQE object
#'
#' @param nStates Number of states
#' @param nQTL Number of QTL associations
#' @param seed Random seed
#'
#' @rdname mock-data
#' @export
mockMSQE <- function(nStates = 10, nQTL = 100, seed=NULL){
  set.seed(seed)

  betas <- mockBeta(nStates=nStates, nQTL=nQTL, seed=seed)
  se <- mockSE(nStates=nStates, nQTL=nQTL, seed=seed)
  pval <- mockPval(nStates=nStates, nQTL=nQTL, seed=seed)
  feature_ids <- sample(c("geneA", "geneB", "geneC"), nQTL, replace=TRUE)
  var_ids <- paste0("snp", sample(seq(1e3:1e5), nQTL))

  msqe <- multiStateQTLExperiment(assay = list(betas=betas, error=se, pval=pval),
                                  rowData=DataFrame(feature_id=feature_ids,
                                                    variant_id=var_ids))

  rownames(msqe) <- paste(rowData(msqe)$feature_id, rowData(msqe)$variant_id, sep="|")

  colData(msqe) <- DataFrame(state=paste0("state", seq(1, nStates)),
                             sample_size=sample(seq(60,120), ncol(msqe)))
  colnames(msqe) <- msqe$state
  mainExpName(msqe) <- "mock-example"

  msqe
}


#' @importFrom stats rnorm
#' @rdname mock-data
#' @export
mockBeta <- function(nStates = 10, nQTL = 100, seed=NULL){
  matrix(rnorm(nStates * nQTL), ncol=nStates)
}


#' @importFrom stats rnorm
#' @rdname mock-data
#' @export
mockSE <- function(nStates = 10, nQTL = 100, seed=NULL){
  matrix(abs(rnorm(nStates * nQTL)), ncol=nStates)
}


#' @importFrom stats runif
#' @rdname mock-data
#' @export
mockPval <- function(nStates = 10, nQTL = 100, seed=NULL){
  values <- runif(n=(nStates-1) * nQTL, min=1e-12, max=1)
  values <- c(values, runif(n=nQTL, min=1e-12, max=0.1))
  values <- sample(values)
  matrix(values, ncol=nStates)
}

