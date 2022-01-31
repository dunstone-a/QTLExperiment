#' Mock MSQE object
#'
#' @param nStates Number of states
#' @param nQTL Number of QTL associations
#'
#' @export
mockMSQE <- function(nStates = 10, nQTL = 100, seed=NULL){
  set.seed(seed)

  beta <- mockBeta(nStates=nStates, nQTL=nQTL, seed=seed)
  se <- mockSE(nStates=nStates, nQTL=nQTL, seed=seed)
  pval <- mockPval(nStates=nStates, nQTL=nQTL, seed=seed)

  msqe <- multiStateQTLExperiment(assay = list(betas=beta, error=se, pval=pval))
  rowData(msqe) <- DataFrame(featureID =  sample(c("geneA", "geneB", "geneC"),
                                                 nQTL, replace=TRUE),
                             snpID = paste0("snp", sample(seq(1e3:1e5), nQTL)))

  rownames(msqe) <- paste(rowData(msqe)$featureID, rowData(msqe)$snpID, sep="|")
  
  colData(msqe) <- DataFrame(state=paste0("state", seq(1, nStates)),
                             sample_size=sample(seq(60,120), ncol(msqe)))
  colnames(msqe) <- msqe$state
  metadata(msqe) <- list(study="mock-example")

  msqe
}

#' Create mock beta matrix
#' 
#' @importFrom stats rnorm
#' @export
mockBeta <- function(nStates = 10, nQTL = 100, seed=NULL){
  matrix(rnorm(nStates * nQTL), ncol=nStates)
}

#' Create mock beta-standard error matrix
#'
#' @importFrom stats rnorm
#' @export
mockSE <- function(nStates = 10, nQTL = 100, seed=NULL){
  matrix(abs(rnorm(nStates * nQTL)), ncol=nStates)
}


#' Create mock pvalue matrix
#'
#' @importFrom stats runif
#' @export
mockPval <- function(nStates = 10, nQTL = 100, seed=NULL){
  values <- runif(n=(nStates-1) * nQTL, min=1e-12, max=1)
  values <- c(values, runif(n=nQTL, min=1e-12, max=0.1))
  values <- sample(values)
  matrix(values, ncol=nStates)
}
