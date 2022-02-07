# Setting up the options for a mock MultiStateQTLExperiment.

set.seed(42)
nTests <- 100
nStates <- 10
mock <- mockMSQE()

#########################################
# Mock reduced dimensions.

d1 <- matrix(rnorm(nStates*4), ncol=4)
rownames(d1) <- colnames(mock)
d2 <- matrix(rnorm(nStates*2), ncol=2)
rownames(d2) <- colnames(mock)
