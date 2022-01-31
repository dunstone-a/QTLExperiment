# Setting up the options for a fully mock SingleCellExperiment.

set.seed(42)
nTests <- 100
nStates <- 10
mock <- mockMSQE()


#########################################
# Mocking up some reduced dimensions.

d1 <- matrix(rnorm(nStates*4), ncol=4)
d2 <- matrix(rnorm(nStates*2), ncol=2)



#########################################
# Mocking up some alternative Experiments.

# se1 <- SummarizedExperiment(
#   list(counts=matrix(rpois(1000, 5), ncol=nTests),
#        logcounts=matrix(rnorm(1000, 5), ncol=nTests)
#   )
# )
# rowData(se1)$stuff <- sample(LETTERS, nrow(se1), replace=TRUE)
# rownames(se1) <- sprintf("SPIKE_%i", seq_len(nrow(se1)))

# se2 <- SummarizedExperiment(
#   list(counts=matrix(rpois(500, 5), ncol=nTests),
#        logcounts=matrix(rnorm(500), ncol=nTests)
#   )
# )
# rowData(se2)$blah <- sample(letters, nrow(se2), replace=TRUE)
# rownames(se2) <- sprintf("TAG_%i", seq_len(nrow(se2)))

# altExp(mock, "Spike") <- se1
# altExp(mock, "Protein") <- se2

#########################################
# Adding some pairings.

# rhits <- SelfHits(
#   sample(nrow(mock), 100),
#   sample(nrow(mock), 100),
#   nnode=nrow(mock)
# )
# mcols(rhits)$value <- runif(100)
# 
# chits <- SelfHits(
#   sample(ncol(mock), 20),
#   sample(ncol(mock), 20),
#   nnode=ncol(mock)
# )
# mcols(chits)$value <- runif(20)
# 
# rowPair(mock) <- rhits
# colPair(mock) <- chits

#########################################
# Other load-ups. 

# sizeFactors(mock) <- 2^rnorm(nTests)
