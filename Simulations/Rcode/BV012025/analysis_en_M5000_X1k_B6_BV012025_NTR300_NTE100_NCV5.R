## Traditional Elastic Net (alpha = 0.5)

library(tidyverse)
library(sim2Dpredictr)

# package to fit model
library(BhGLM)
library(rstan)
rstan_options(auto_write = TRUE)
library(ssnet)

# load data -> use array jobs
runID <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# load data
# dat.train <- readRDS(
#   file = paste0(
#     "/data/user/jleach/sim_mn_2021/simdata/train_tr",
#     runID, ".RDS"
#   )
# )
# 
# dat.test <- readRDS(
#   file = paste0(
#     "/data/user/jleach/sim_mn_2021/simdata/test_tr",
#     runID, ".RDS"
#   )
# )

if (runID == 1){
  sim.ids <- 1:1000
} else {
  sim.low <- (runID - 1)*1000 + 1
  sim.high <- 1000 + (runID - 1)*1000
  sim.ids <- sim.low:sim.high
}

dat.train <- readRDS(
  file = "/data/user/jleach/sim_mn_2021/simdata/sim_data_M5000_X1k_B6_BV012025_NTR300.RDS"
)[sim.ids]
dat.test <- readRDS(
  file = "/data/user/jleach/sim_mn_2021/simdata/sim_data_M5000_X1k_B6_BV012025_NTE100.RDS"
)[sim.ids]

# to store errors
file.create(paste0("/data/user/jleach/sim_mn_2021/errors/errors_en_M5000_X1k_B6_BV012025_NTE100_",
                   runID, ".txt"))
# file.create("/data/user/jleach/sim_mn_2021/errors/errors_tr_en.txt")

# Get M from length of list of data frames
M <- length(dat.train)

# simulation parameters
model <- "glmnet"
alpha <- 0.5
nfolds <- 5


model_fitness <- NULL
param_est <- NULL

set.seed(263512)
t1 <- proc.time()
for (m in 1:M) {
  tryCatch(
    expr = {
      # cross validation in training set (en)
      fit.m <- cv.glmnet(
        x = as.matrix(dat.train[[m]] %>% select(-Y)),
        y = dat.train[[m]]$Y,
        alpha = alpha, nfolds = nfolds, 
        family = "multinomial",
        type.multinomial = "grouped")
      
      # validate in test set (en)
      val.m <- validate_ssnet(
        model = model, alpha = alpha, s0 = fit.m$lambda.min,
        family = "multinomial", type.multinomial = "grouped",
        x.train = as.matrix(dat.train[[m]] %>% select(-Y)),
        x.test = as.matrix(dat.test[[m]] %>% select(-Y)),
        y.train = dat.train[[m]]$Y, y.test = dat.test[[m]]$Y,
        output_param_est = TRUE
      )
      
      model_fitness <- rbind(
        model_fitness,
        cbind(sim.num = m,
              s0 = fit.m$lambda.min,
              s1 = fit.m$lambda.min,
              val.m$model_fitness)
      )
      param_est <- rbind(
        param_est,
        cbind(sim.num = m,
              s0 = fit.m$lambda.min,
              s1 = fit.m$lambda.min,
              val.m$param_est)
      )
    },
    error = function(e) {
      message("* Caught an error on iteration ", m)
      print(e)
      write_lines(paste("Iteration #", m , ":", as.character(e)),
                  path = paste0("/data/user/jleach/sim_mn_2021/errors/errors_en_M5000_X1k_B6_BV012025_NTE100_",
                                runID, ".txt"),
                  append = TRUE)
      # write_lines(paste("Iteration #", m , ":", as.character(e)),
      #             path = "/data/user/jleach/sim_mn_2021/errors/errors_tr_en.txt",
      #             append = TRUE)
    },
    finally = {
      message(cat("Model fitting for simulated dataset ", m, " has completed."))
    }
  )
}
fit.time <- proc.time() - t1
fit.time
out <- list(
  fit_time = fit.time,
  model_fitness = model_fitness,
  param_est = param_est
)

saveRDS(out,
        file = paste0("/data/user/jleach/sim_mn_2021/results/results_en_M5000_X1k_B6_BV012025_NTR300_NTE100_NCV5_",
                      runID, ".RDS"))
# saveRDS(out,
#         file = "/data/user/jleach/sim_mn_2021/results/results_en_M5000_X1k_B6_BV012025_NTR300_NTE100_NCV10.RDS")

