#' Get the threshold for each state
#' 
#' @param object A \linkS4class{multiStateQTLExperiment} object.
#' @param mode Method to determine significance threshold per state
#' @param thresh Significance threshold
#' @param p.adjust.method Method of multiple-test correction if mode != simple
#'        
#' @importFrom SummarizedExperiment assay
#' 
.get_thresh_per_state <- function(object, mode, thresh, p.adjust.method){
  if (match.arg(mode) == "simple") {
    state_thresh <- rep(thresh, ncol(object))
    names(state_thresh) <- colnames(object)
    
  } else if (match.arg(mode) == "global-FDR"){
    state_thresh <- apply(assay(object, significance), 2, function(x)
      .get_corrected_thresh(x, thresh = thresh,
                            p.adjust.method = match.arg(p.adjust.method)))
    
  } else if (match.arg(mode) == "feature-wise-FDR") {
    top <- getTopPerFeature(object)
    state_thresh <- apply(assay(top, significance), 2, function(x)
      .get_corrected_thresh(x, thresh = thresh,
                            p.adjust.method = match.arg(p.adjust.method)))
  } else {
    warning("Mode not available for getSignificant...")
  }
  
  state_thresh <- t(as.data.frame(state_thresh))
  state_thresh <- state_thresh[rep(seq_len(nrow(state_thresh)), 
                                   each = nrow(object)), ]
  return(state_thresh)
}

#' Get corrected significance threshold for state
#' @param x One column from a \linkS4class{multiStateQTLExperiment} object.
#' @param thresh Significance threshold
#' @param p.adjust.method Method of multiple-test correction if mode != simple
#' 
#' @importFrom stats p.adjust
.get_corrected_thresh <- function(x, thresh, p.adjust.method){
  
  sig <- data.frame(list(raw = unname(x), 
                         adj = unname(p.adjust(x, method = p.adjust.method))))
  sig <- sig[sig$adj <= thresh, ]
  
  if (nrow(sig)==0){ emp_sig_thresh <- 0
  } else{ emp_sig_thresh <- max(sig$raw, na.rm = TRUE)}
  
  emp_sig_thresh
  
}


##################################################

#' @export
#' @rdname getSignificant
setGeneric("getSignificant", function(object, ...) standardGeneric("getSignificant"))

#' Add assay with T/F significance calls
#' 
#' @param object A \linkS4class{multiStateQTLExperiment} object.
#' @param thresh Significance threshold
#' @param second.thresh Significance threshold for associations with significance in one state.
#' @param feature rowData column name with feature identifier
#' @param significance assay name with significance score.
#' @param mode Method to determine significance threshold per state
#' @param p.adjust.method Method of multiple-test correction if mode != simple
#' 
#' @export
#' @rdname getSignificant
#' @importFrom SummarizedExperiment assay
#'
setMethod("getSignificant", "multiStateQTLExperiment",
          function(object, 
                   thresh = 0.05,
                   second.thresh = thresh,
                   feature = "feature_id",
                   significance = "pval",
                   mode = c("feature-wise-FDR", "simple", "global-FDR"),
                   p.adjust.method=c("fdr", "holm", "hochberg", "hommel", 
                                     "bonferroni", "BH", "BY", "none")){
            
            state_thresh <- .get_thresh_per_state(object, mode, 
                                                  thresh, p.adjust.method)
            object$significance_threshold <- state_thresh[1, ]
            significant <- as.matrix(assay(object, 
                                           significance)) <= state_thresh
            
            # If using less strict threshold for tests with at least one sig hit
            if(!is.null(second.thresh) & second.thresh != thresh){
              
              state_2thresh <- .get_thresh_per_state(object, mode, 
                                                     second.thresh, 
                                                     p.adjust.method)
              object$significance_threshold2 <- state_2thresh[1, ]
              significant.second <- as.matrix(assay(object, 
                                                    significance)) <= state_2thresh
              
              first.sig <- which(rowSums(significant) > 0)
              significant[first.sig, ] <- significant.second[first.sig, ]
            }
          
          assay(object, "significant") <- significant
          object$nSignificant <- colSums(significant)
          
          return(object)
})

perTestMetrics <- function(object, ...) {
  
  if( ! "significant" %in% names(assays(object)) ) {
    object <- getSignificant(object, ...)
  }
 
  nSig <- rowSums(as.matrix(assay(object, "significant")))
  rowData(object)$nSignificant <- nSig
  rowData(object)$type <- ifelse(nSig == ncol(object), "global",
                           ifelse(nSig == 0, "not_sig",
                            ifelse(nSig == 1, "unique", "multi-state")))
  return(object)
}