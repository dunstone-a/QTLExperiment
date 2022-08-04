#' @title Coercing mash data objects into MSQE objects
#'
#' @param data A `MSQE` object or named list containing "betas" and "error"
#'             matricies.
#' @param thresh Threshold for calling tests as significant (pval or lfsr).
#' @param verbose Logical.
#'
#' @examples
#'
#' @rdname mash_2_msqe
#'
#' @importFrom stats rnorm
#' @export
#'
mash_2_msqe <- function(data, sep=NULL, rowData=NULL, verbose=FALSE) {

  if(all(is.null(sep), is.null(rowData))){
    stop("Must specify sep or rowData.")
  }

  if("Bhat" %in% names(data)){
    assay_list <- .mashData_2_msqe(data)
  } else if("result" %in% names(data)){
    assay_list <- .mashFit_2_msqe(data)
  }

  if(!is.null(sep)){
    rowData <- as.data.frame(list(id=as.character(rownames(assay_list[[1]])))) %>%
      tidyr::separate(id, into=c("feature_id", "variant_id"), sep=sep)

    if(verbose){message("# unique features: ",
                        length(unique(rowData$feature_id)))}
  }

  msqe <- multiStateQTLExperiment(assay=assay_list, rowData=rowData)

  return(msqe)
}


#' @rdname mash_2_msqe
#' @importFrom SummarizedExperiment assay assay<-
#' @importFrom tidyr separate
#' @export
#'
.mashData_2_msqe <- function(data) {

  betas <- as.matrix(data$Bhat)
  error <- as.matrix(data$Shat)

  assay_list <- list(betas = betas,
                     error = error)

  if("pval" %in% names(data)){
    assay_list[["pval"]] <- as.matrix(data$pval)
  }

  if("lfsr" %in% names(data)){
    assay_list[["lfsr"]] <- as.matrix(data$lfsr)
  }

  return(assay_list)
}

#' @rdname mash_2_msqe
#' @importFrom ashr get_pm get_psd get_lfsr
#'
.mashFit_2_msqe <- function(data){

    beta <- as.matrix(get_pm(data))
    error <- as.matrix(get_psd(data))
    lfsr <- as.matrix(get_lfsr(data))

    assay_list <- list(betas = beta, error = error, lfsr = lfsr)

    return(assay_list)
}
