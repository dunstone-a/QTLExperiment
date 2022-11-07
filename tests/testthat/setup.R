# Setting up the options for a mock MultiStateQTLExperiment.

set.seed(42)
nQTL <- 100
nStates <- 10

sumstats <- mockSummaryStats(nStates=nStates, nQTL=nQTL, names=TRUE)
qtle <- QTLExperiment(assay=list(betas=sumstats$betas,
                                 errors=sumstats$errors,
                                 pvalues=sumstats$pvalues,
                                 lfsrs=sumstats$pvalues))


sumstats_noNames <- mockSummaryStats(nStates=nStates, nQTL=nQTL, names=FALSE)
state_ids <- colnames(sumstats$betas)
feature_ids <- gsub("\\|.*", "", row.names(sumstats$betas))
variant_ids <- gsub(".*\\|", "", row.names(sumstats$betas))

mock <- mockQTLE(nStates = nStates, nQTL = nQTL)
