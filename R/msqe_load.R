#' @title Generate multiStateQTLExperiment object from QTL mapping results
#'
#' @description
#' A suite of methods to extract QTL mapping summary statistics from common
#' QTL workflow output files.
#'
#' @section Modes:
#'
#' \describe{
#' \item{\code{mode=all}:}{Loads all QTL in input data.}
#' \item{\code{mode=significant}:}{Loads all QTL under the significance
#' threshold in at least one state.}
#' \item{\code{mode=top}:}{Loads all QTL that are the top QTL for a feature in
#' at least one state.}
#' \item{\code{mode=random}:}{Loads a random subset of QTL (n=`nRandom`).}
#' }
#'
#'
#' @param input Named array or data.frame with state name and the file to the
#'              QTL summary statistics for that state. If data.frame is
#'              provided, additional columns will be stored in the colData
#'              annotation.
#' @param mode How to extract summary statistics.
#' @param na.rm logical. If QTL should be dropped if missing for any state.
#' @param thresh Significance threshold if `mode=significant`.
#' @param nRandom Number of QTL to sample if `mode=random`.
#' @param beta The name/index of the column with the effect size/beta value.
#' @param se The name/index of the column with the effect size/beta standard
#'           error value.
#' @param sig The name/index of the column with the significance score.
#' @param verbose logical. Whether to print progress messages.
#'
#' @author
#' Christina B Azodi
#'
#' @export
#' @importFrom rlang .data
#' @importFrom data.table fread
#' @importFrom dplyr filter mutate %>%
#' @importFrom SummarizedExperiment rowData rowData<- colData assay
#'
loadSummaryStatistics <- function(input,
                                  mode = "significant",
                                  na.rm = FALSE,
                                  thresh = 0.05,
                                  nRandom = 1000,
                                  beta = "beta",
                                  se = "beta_se",
                                  sig = "empirical_feature_p_value",
                                  verbose = TRUE){

  id <- NULL

  if(class(input)=="list"){
    input <- data.frame(list(state=names(input), file=unlist(unname(input))))
  }

  if(!"state" %in% colnames(input) | !"file" %in% colnames(input)){
    error("input a named array or a df with columns `state` and `file`")
  }

  input <- .absent_file_action(input, onAbsence="warn")


  if(tolower(mode) == "random"){
    keep <- getRandomQTL(input, nRandom=nRandom, verbose=verbose)
  } else if (tolower(mode) == "top"){
    keep <- getTopQTL(input, sig=sig, verbose=verbose)
  } else if (tolower(mode) %in% c("sig", "significant")){
    keep <- getSignificantQTL(input, sig=sig, thresh=thresh, verbose=verbose)
  } else if (tolower(mode) == "all"){
    keep <- NULL
  } else { warning("Did not recognize mode...")}


  if(verbose) {message("Loading summary statistics for: ")}
  listBetas <- list()
  listError <- list()
  listPval <- list()

  for(row in seq(1, nrow(input))){
    if(verbose) { message(input$state[row]) }

    tmp <- data.table::fread(input$file[row], showProgress = verbose) %>%
      dplyr::mutate(id = paste(.data$feature_id, .data$variant_id, sep="|"))

    if(!is.null(keep)) { tmp <- tmp %>% dplyr::filter(.data$id %in% keep)}

    listBetas[[row]] <- setNames(as.list(tmp[, beta]), tmp[, id])
    listError[[row]] <- setNames(as.list(tmp[, se]), tmp[, id])
    listPval[[row]] <- setNames(as.list(tmp[, sig]), tmp[, id])

  }

  dfBeta <- t(data.table::setDF(data.table::rbindlist(listBetas, fill=TRUE)))
  dfSE <- t(data.table::setDF(data.table::rbindlist(listError, fill=TRUE)))
  dfSig <- t(data.table::setDF(data.table::rbindlist(listPval, fill=TRUE)))

  msqe <- multiStateQTLExperiment(list(beta = dfBeta, se = dfSE, sig = dfSig))

  colData(msqe) <- DataFrame(input)
  colnames(msqe) <- input$state
  row.names(msqe) <- row.names(dfBeta)
  rowData(msqe)$feature_id <- gsub("\\|.*", "", row.names(msqe))
  rowData(msqe)$variant_id <- gsub(".*\\|", "", row.names(msqe))

  if(any(any(is.na(assays(msqe))))){
    if(na.rm) {msqe <- removeNAs(msqe, verbose)}
    else{msqe <- fillNAs(msqe, verbose)}
  }

  return(msqe)

}


#' Convert a mash_set_data object to a MSQE
#'
#' @param m mash_set_data input
#' @importFrom SummarizedExperiment assay assay<-
#'
mash_set_data2MSQE <- function(m) {

  msqe <- multiStateQTLExperiment(list(beta = m$Bhat,
                                         se = m$Shat))

  if("pval" %in% names(m)){
    assay(m)$pval <- m$pval
  }

  return(msqe)
}

#' @importFrom ashr get_pm get_psd get_lfsr
#' @importFrom SummarizedExperiment rowData rowData<-
#'
mash_out2MSQE <- function(m){

  beta <- DataFrame(get_pm(m))
  sd <- DataFrame(get_psd(m))
  lfsr <- DataFrame(get_lfsr(m))

  msqe <- multiStateQTLExperiment(list(beta = beta, se = sd, lfsr = lfsr))

  rowData(msqe)$feature_id <- gsub("\\|.*", "", row.names(msqe))
  rowData(msqe)$variant_id <- gsub(".*\\|", "", row.names(msqe))
  msqe$state <- colnames(msqe)

  return(msqe)
}

#' Return an array of N random QTL
#' @param input Named array or data.frame with state name and the file to the
#'              QTL summary statistics for that state. If data.frame is
#'              provided, additional columns will be stored in the colData
#'              annotation.
#' @param nRandom Number of QTL to sample if `mode=random`.
#' @param verbose logical. Whether to print progress messages.
#'
#' @importFrom data.table fread
#'
getRandomQTL <- function(input,
                         nRandom = nRandom,
                         verbose = verbose){

  if(verbose) {message("Selecting ", nRandom, " random tests from: ",
                       input$state[1])}
  tmp <- data.table::fread(input$file[1], showProgress = verbose)
  tmp$QTL <- paste(tmp[, "feature_id"], tmp[, "variant_id"], sep="|")

  return(sample(tmp$QTL, nRandom))

}

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
getTopQTL <- function(input,
                      sig = sig,
                      verbose = verbose){

  if(verbose) { message("Selecting top hits per feature from...") }
  keep <- c()

  for(row in seq(1, nrow(input))){
    if(verbose) { message(input$state[row]) }

    tmp <- data.table::fread(input$file[row], showProgress = verbose) %>%
      dplyr::group_by(get(.data$feature_id)) %>%
      dplyr::slice(which.min(get(sig))) %>%
      dplyr::mutate(id = paste(get(.data$feature_id), .data$variant_id, sep="|"))

    keep <- union(keep, tmp$id)
  }

  if(verbose) { message("Total number of top associations: ", length(keep)) }

  return(keep)
}

#' Return an array of the QTL significant in at least one state
#'
#' @param input Named array or data.frame with state name and the file to the
#'              QTL summary statistics for that state. If data.frame is
#'              provided, additional columns will be stored in the colData
#'              annotation.
#' @param sig The name/index of the column with the significance score.
#' @param thresh Significance threshold if `mode=significant`.
#' @param verbose logical. Whether to print progress messages.
#'
#' @importFrom dplyr group_by slice
#' @importFrom data.table fread
#'
getSignificantQTL <- function(input,
                      sig = sig,
                      thresh = thresh,
                      verbose = verbose){

  if(verbose) { message("Selecting significant hits from...") }
  keep <- c()

  for(row in seq(1, nrow(input))){
    if(verbose) { message(input$state[row]) }

    tmp <- data.table::fread(input$file[row], showProgress = verbose) %>%
      dplyr::filter(get(sig) <= thresh) %>%
      dplyr::mutate(id = paste(.data$feature_id, .data$variant_id, sep="|"))

    keep <- union(keep, tmp$id)
  }

  if(verbose) { message("Total number of significant associations: ",
                        length(keep)) }

  return(keep)
}

#' Return MSQE object with NAs filled in
#'
#' @param object Named array or data.frame with state name and the file to the
#'              QTL summary statistics for that state. If data.frame is
#'              provided, additional columns will be stored in the colData
#'              annotation.
#' @param verbose logical. Whether to print progress messages.
#'
#' @importFrom dplyr group_by slice
#' @importFrom SummarizedExperiment assays assays<-
#' @importFrom data.table fread
#'
fillNAs <- function(object, verbose){

  for(a in names(assays(object))){
    if(grepl("beta|effect", a)){
      if (verbose) {message("Replacing NAs in ", a, " with 0...")}
      assays(object)[[a]][is.na(assays(object)[[a]])] <- 0

    } else if(grepl("sig|pval|p.val", a)){
      if (verbose) {message("Replacing NAs in ", a, " with 1...")}
      assays(object)[[a]][is.na(assays(object)[[a]])] <- 1

    } else if(grepl("se|standard", a)){
      if (verbose) {message("Replacing NAs in ", a, " with the row mean...")}
      tmp <- assays(object)[[a]]
      k <- which(is.na(tmp), arr.ind=TRUE)
      assays(object)[[a]][k] <- rowMeans(tmp, na.rm=TRUE)[k[, 1]] # association mean SE
    }
  }

  return(object)
}

#' Return MSQE object with rows with NAs in any assay removed
#'
#' @param object Named array or data.frame with state name and the file to the
#'              QTL summary statistics for that state. If data.frame is
#'              provided, additional columns will be stored in the colData
#'              annotation.
#' @param verbose logical. Whether to print progress messages.
#'
#' @importFrom dplyr group_by slice
#' @importFrom data.table fread
#' @importFrom SummarizedExperiment assay
#'
removeNAs <- function(object, verbose){

  keep <- rep(TRUE, nrow(object))
  for(a in names(assays(object))){
    keep[is.na(assays(object)[[a]])] <- FALSE
  }

  if(verbose) {message("Dropping ", table(keep)["FALSE"], " rows with NAs...")}
  object <- object[keep, ]

  return(object)
}
