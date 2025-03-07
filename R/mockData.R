#' @title Mock data for the QTLExperiment object
#'
#' @description
#' Functions to create fake input data for QTLExperiments.
#' Note that this data is generated simply, and does not have consistency between 
#' betas, errors and p-values. 
#' It is helpful to populate the slots of a QTLExperiment object and has expected 
#' properties of the betas, errors and p-values assays.
#' Namely, betas are symmetric (specifically normally distributed), errors are 
#' non-negative, and p-values consist of a null and significant distribution. 
#' The significant effects make up 10% of the p-values and are randomly distributed 
#' across states and tests. 
#' 
#' Feature IDs are simulated by randomly selecting a feature from the list 
#' c("geneA", "geneB", "geneC") with replacement. Variant IDs are created by 
#' concatenating the string "snp" with a random number in the range 1000:100000.
#' Row names combine the feature and variant IDs using a vertical line as the 
#' separator. 
#'
#' @param nStates Number of states
#' @param nQTL Number of QTL associations
#' @param names Logical to include column and row names. 
#'
#' @docType methods
#' @rdname mock-data
#'
#' @examples
#' nStates <- 6
#' nQTL <- 40
#'
#' # Mock QTLExperiment data
#'
#' qtle <- mockQTLE(nStates, nQTL)
#' dim(qtle)
#'
#' @returns an object containing simulated data.
#'
#' @author Christina B Azodi, Amelia Dunstone
#' @export
#'
#' @importFrom stats prcomp

mockQTLE <- function(nStates=10, nQTL=100, names=TRUE){

    sumstats <- mockSummaryStats(nStates=nStates, nQTL=nQTL, names=names)

    object <- QTLExperiment(
        assays=list(betas=sumstats$betas,
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
#'
#' @examples
#' mock_summary_stats <- mockSummaryStats(nStates=nStates, nQTL=nQTL)
#' mock_summary_stats$betas
#' mock_summary_stats$errors
#' mock_summary_stats$pvalues
#'
#' @export

mockSummaryStats <- function(nStates=10, nQTL=100, names=TRUE){

    betas <- matrix(rnorm(nStates * nQTL), ncol=nStates)
    errors <- matrix(abs(rnorm(nStates * nQTL)), ncol=nStates)
    pvalues <- runif(n=(nStates-1) * nQTL, min=1e-12, max=1) # Null distribution
    pvalues <- c(pvalues, runif(n=nQTL, min=1e-12, max=0.1)) # Significant effects
    pvalues <- sample(pvalues) # Shuffle significant effects throughout data 
    pvalues <- matrix(pvalues, ncol=nStates) # Format p-values as a matrix

    if(names){
        test_ids <- paste(
            sample(c("geneA", "geneB", "geneC"), nQTL, replace=TRUE),
            paste0("snp", sample(seq(1e3,1e5), nQTL)),
            sep="|")
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
#'
#' @examples
#' # Mock MASHR data
#'
#' mockr_sim <- mockMASHR(nStates=nStates, nQTL=nQTL)
#' mockr_sim$B
#' mockr_sim$Bhat
#' mockr_sim$Shat
#'
#' @export
#'

mockMASHR <- function(nStates=10, nQTL=100){

    sumstats <- mockSummaryStats(nStates=nStates, nQTL=nQTL)

    list(B=sumstats$betas, Bhat=sumstats$betas, Shat=sumstats$errors)
}

#' Mock mashr data after fitting
#'
#' @param nStates Number of states
#' @param nQTL Number of QTL associations
#'
#' @rdname mock-data
#' @export
#'

mockMASHR_FIT <- function(nStates=10, nQTL=100){

    sumstats <- mockSummaryStats(nStates=nStates, nQTL=nQTL)
    result <- list(PosteriorMean=sumstats$betas,
        PosteriorSD=sumstats$betas,
        lfsr=sumstats$pvalues)
    list(result=result)
}
