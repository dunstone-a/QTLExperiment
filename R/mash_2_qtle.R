#' @title Coercing mash data objects into QTLe objects
#'
#' @description
#' Function to coerce a mashr object (class list or mashr) into a QTLe object.
#'
#' @param data A mashr object output from mash_set_data() or mash() from mashr.
#' @param sep String separating the feature_id from the variant_id in the row.names of the mashr object
#' @param rowData if feature_id and variant_id are not in the row.names, a rowData matrix can be provided with this information.
#' @param verbose Logical.
#'
#' @docType methods
#' @name QTLe-coerce
#' @rdname mash2qtle
#'
#' @returns A \linkS4class{QTLExperiment} object.
#'
#' @examples
#' nStates <- 6
#' nQTL <- 40
#' mashr_sim <- mockMASHR(nStates, nQTL)
#'
#' qtle2 <- mash2qtle(
#'     mashr_sim,
#'     rowData=DataFrame(feature_id=row.names(mashr_sim$Bhat),
#'                       variant_id=sample(seq_len(nQTL))))
#' dim(qtle2)
#'
#'
#' @author Christina B Azodi, Amelia Dunstone
#'
#' @importFrom stats rnorm
#' @importFrom tidyr separate
#' @importFrom rlang .data
#' @export
#'
mash2qtle <- function(data, sep=NULL, rowData=NULL, verbose=FALSE) {

    if(all(is.null(sep), is.null(rowData))){
        stop("Must specify sep or rowData.")
    }

    if("Bhat" %in% names(data)){
        assay_list <- .mashData_2_qtle(data)
    } else if("result" %in% names(data)){
        assay_list <- .mashFit_2_qtle(data)
    }

    if(!is.null(sep)){
        rowData <- as.data.frame(list(id=as.character(rownames(assay_list[[1]])))) %>%
            separate(.data$id, into=c("feature_id", "variant_id"), sep=sep)

        if(verbose){message("# unique features: ",
                            length(unique(rowData$feature_id)))}
    }

    object <- QTLExperiment(assays=assay_list,
                            feature_id = rowData$feature_id,
                            variant_id = rowData$variant_id)

    return(object)
}


#' @rdname mash2qtle
#' @importFrom SummarizedExperiment assay assay<-
#' @importFrom tidyr separate
#' @export
#'
.mashData_2_qtle <- function(data) {

    betas <- as.matrix(data$Bhat)
    errors <- as.matrix(data$Shat)

    assay_list <- list(betas = betas,
                       errors = errors)

    if("pvalues" %in% names(data)){
        assay_list[["pvalue"]] <- as.matrix(data$pval)
    }

    if("lfsrs" %in% names(data)){
        assay_list[["lfsrs"]] <- as.matrix(data$lfsr)
    }

    return(assay_list)
}

#' @rdname mash2qtle
#' @importFrom ashr get_pm get_psd get_lfsr
#'
.mashFit_2_qtle <- function(data){

    betas <- as.matrix(get_pm(data))
    errors <- as.matrix(get_psd(data))
    lfsrs <- as.matrix(get_lfsr(data))

    assay_list <- list(betas = betas, errors = errors, lfsrs = lfsrs)

    return(assay_list)
}
