#' Return an array of the top QTL for each feature across all states
#'
#' @param input Named array or data.frame with state name and the file to the
#'              QTL summary statistics for that state. If data.frame is
#'              provided, additional columns will be stored in the colData
#'              annotation.
#' @param sig The name/index of the column with the significance score.
#' @param verbose logical. Whether to print progress messages.
#'
#' @importFrom dplyr group_by slice
#' @importFrom data.table fread
#'
subsetTopHits <- function(msqe, mode=c("global", "state"),
                          assay = "pval", .FUN = min(),
                          verbose = FALSE){

  if(verbose) { message("Selecting top hits per feature from...") }
  keep <- c()

  if(mode == "global"){
    keep <- as.data.frame(list(feature_id=rowData(msqe)[,"feature_id"],
                              id=rownames(msqe),
                              min_p = rowMin(assay(msqe, assay)))) %>%
      group_by(feature_id) %>% slice(which.min(min_p)) %>% fselect(id)
  }else if (mode == "state"){
    keep <- as.data.frame(assay(msqe, assay)) %>%
      fmutate(id=rownames(msqe),
             feature_id=rowData(msqe)$feature_id) %>%
      pivot_longer(-c(feature_id, id)) %>%
      group_by(feature_id, name) %>% slice(which.min(value)) %>% fselect(id)
  }

  if(verbose) { message("Total number of top associations: ", length(keep)) }

  return(msqe[unique(keep$id), ])
}


#' Return an array of the QTL significant in at least one state
#'
#' @param msqe `MSQE` object
#' @param assay The name of the assay containing significance information.
#' @param thresh Significance threshold.
#' @param verbose logical. Whether to print progress messages.
#'
#' @importFrom dplyr %>%
#' @importFrom collapse fmin
#'
#' @export
subsetSignificant <- function(msqe,
                              assay = "pval",
                              thresh = thresh,
                              verbose = FALSE){

  keep <- t(assay(msqe, assay)) %>% fmin() <= thresh
  msqe <- msqe[keep, ]

  if(verbose) { message("Number of significant associations: ", sum(keep)) }

  return(msqe)
}


#' Remove QTL tests with N missing values
#'
#' @param msqe `MSQE` object
#' @param n Number (or percent if n <= 1) of states requiring non-null values
#' @param verbose logical. Whether to print progress messages.
#'
#' @importFrom dplyr %>%
#' @importFrom collapse fmin
#'
#' @export
#'
subsetComplete <- function(msqe, n = 1, verbose=FALSE){

  if(n <= 1){n <- ncol(msqe) * n}

  if(verbose) { message("Removing QTL with data for < ", n, " states")}

  keep <- rowSums(!is.na(betas(msqe))) >= n
  msqe <- msqe[keep, ]

  if(verbose) { message("Number of remaining QTL: ", nrow(msqe)) }

  return(msqe)
}


#' Return MSQE object with NAs filled in
#'
#' @param object `MSQE` object
#' @param verbose logical. Whether to print progress messages.
#'
#' @importFrom SummarizedExperiment assays assays<-
#'
#' @export
#'
naReplace <- function(object, verbose=FALSE){

  for(a in names(assays(object))){
    if(grepl("betas|beta|Bhat|effect", a)){
      if (verbose) {message("Replacing NAs in ", a, " with 0...")}
      assays(object)[[a]][is.na(assays(object)[[a]])] <- 0

    } else if(grepl("sig|pval|p.val|pvalue|lfsr", a)){
      if (verbose) {message("Replacing NAs in ", a, " with 1...")}
      assays(object)[[a]][is.na(assays(object)[[a]])] <- 1

    } else if(grepl("error|se|Shat|SE", a)){
      if (verbose) {message("Replacing NAs in ", a, " with the row mean...")}
      tmp <- assays(object)[[a]]
      k <- which(is.na(tmp), arr.ind=TRUE)
      assays(object)[[a]][k] <- rowMeans(tmp, na.rm=TRUE)[k[, 1]] # association mean SE
    } else{
      stop("Did not know how to relace NAs in assay: ", a)
    }
  }

  return(object)
}
