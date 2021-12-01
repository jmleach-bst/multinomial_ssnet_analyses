# simulate data
# b/c scientific notation is horrendous for these scenarios
options(scipen = 999)
# always, and for so many things
library(tidyverse)
# elastic net
library(glmnet)
# simulate spatially correlated data
library(sim2Dpredictr)
# fit ssen (iar) models
library(ssnet)

# reproduce
set.seed(893828)

# how many simulations?
M <- 5000

# how many predictors?
p <- 1000

# sample size
n.train <- 300
n.test <- 100

# function to build correlation matrix
block_correlation_builder <- function(
  size_block = 20,
  num_block = 50,
  corr_block = 0.90
) {
  cm <- list()
  for (i in 1:num_block) {
    cmi <- matrix(corr_block, nrow = size_block, ncol = size_block)
    diag(cmi) <- 1
    cm[[i]] <- cmi
  }
  bm <- Matrix::bdiag(cm)
  return(bm)
}

# correlation matrix
# 50 blocks of 20 highly correlated parameters
R <- block_correlation_builder(
  size_block = 20,
  num_block = 50,
  corr_block = 0.90
)

# if you want non-unit covariance ... probably not, but just in case
sigma <- 1
if (sigma == 1) {
  S <- R
} else {
  D <- diag(sigma, nrow = p, ncol = p)
  S <- D %*% R %*% D
}

# spam is better
S <- spam::as.spam(as.matrix(S))
Rc <- spam::chol.spam(S)
L <- spam::t(Rc)

# What is/are the effect size(s)?
# Here we will make it so that each "block" has at most 
# a single non-zero parameter.
# In this simulation only the 1st 6 blocks have non-zero parameters.
# We are trying to in some way emulate a simulation scenario from
# Rockova & George (2018), but the coefficients won't be the same 
# because these are multinomial, not continuous, outcomes.

BB <- rep(0, p)

# non-zero locations
nzl <- c(1, 51, 101, 151, 201, 251)

# "reference" group 1 (all 0's)
B1 <- c(0, BB)
# group 2
B2 <- BB
B02 <- -0.15
B2[c(1, 51, 101, 151, 201, 251)] <- 0.075
B2 <- c(B02, B2)
# group 3
B3 <- BB
B03 <- -0.5
B3[c(1, 51, 101, 151, 201, 251)] <- 0.15
B3 <- c(B03, B3)

B.all <- list(B1 = B1,
              B2 = B2,
              B3 = B3)

dat.train <- list()
y.train <- NULL
for (m in 1:M) {
  dat.train[[m]] <- sim2Dpredictr::sim_Y_MVN_X(
    N = n.train, B = B.all, L = L, S = S,
    dist = "multinomial", V = 3, incl.subjectID = FALSE
  )
  y.train <- rbind(
    y.train,
    table(dat.train[[m]]$Y)
  )
}

dat.test <- list()
y.test <- NULL
for (m in 1:M) {
  dat.test[[m]] <- sim2Dpredictr::sim_Y_MVN_X(
    N = n.test, B = B.all, L = L, S = S,
    dist = "multinomial", V = 3, incl.subjectID = FALSE
  )
  y.test <- rbind(
    y.test,
    table(dat.test[[m]]$Y)
  )
}

apply(y.train, 2, mean)
apply(y.train, 2, sd)
apply(y.test, 2, mean)
apply(y.test, 2, sd)


saveRDS(dat.train, 
        file = 
          "/data/user/jleach/sim_mn_2021/simdata/sim_data_M5000_X1k_B6_BV0075015_NTR300.RDS")

saveRDS(dat.test, 
        file = 
          "/data/user/jleach/sim_mn_2021/simdata/sim_data_M5000_X1k_B6_BV0075015_NTE100.RDS")












