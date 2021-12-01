## Traditional Spike-and-Slab Lasso (alpha = 1)

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

# if (runID == 1){
#   sim.ids <- 1:1000
# } else {
#   sim.low <- (runID - 1)*1000 + 1
#   sim.high <- 1000 + (runID - 1)*1000
#   sim.ids <- sim.low:sim.high
# }

if (runID == 1){
  sim.ids <- 1:200
} else {
  sim.low <- (runID - 1)*200 + 1
  sim.high <- 200 + (runID - 1)*200
  sim.ids <- sim.low:sim.high
}

dat.train <- readRDS(
  file = "/data/user/jleach/sim_mn_2021/simdata/sim_data_M5000_X1k_B6_BV0075015_NTR300.RDS"
)[sim.ids]
dat.test <- readRDS(
  file = "/data/user/jleach/sim_mn_2021/simdata/sim_data_M5000_X1k_B6_BV0075015_NTE100.RDS"
)[sim.ids]

# to store errors
file.create(paste0("/data/user/jleach/sim_mn_2021/errors/errors_ssl_M5000_X1k_B6_BV0075015_NTE100_",
                   runID, ".txt"))
# file.create("/data/user/jleach/sim_mn_2021/errors/errors_tr_ssl.txt")

# Get M from length of list of data frames
M <- length(dat.train)

# simulation parameters
model <- "ss"
alpha <- 1
nfolds <- 5
s0 = seq(0.01, 0.1, 0.01)
s1 = seq(1, 4, 1)
mult.opt = "min"

model_fitness <- NULL
param_est <- NULL

set.seed(263512)
t1 <- proc.time()
for (m in 1:M) {
  tryCatch(
    expr = {
      # cross validation in training set
      fit.m <- cv_ssnet(
        family = "multinomial", type.multinomial = "grouped",
        model = model, alpha = alpha, nfolds = nfolds,
        s0 = s0, s1 = s1,
        x = as.matrix(dat.train[[m]] %>% select(-Y)),
        y = dat.train[[m]]$Y,
        fold.seed = 6048497
      )
      
      # obtain mean deviance for each combination of s0, s1
      fit.m.g <- fit.m %>% 
        select(s0, s1, deviance) %>%
        group_by(s0, s1) %>%
        nest() %>%
        mutate(
          mean_deviance = map(.x = data, .f = function(x) mean(x$deviance))
        ) %>%
        select(-data) %>%
        unnest(cols = mean_deviance)
      
      # select optimal parameters
      md.m <- min(fit.m.g$mean_deviance)
      opt.m <- fit.m.g %>%
        filter(mean_deviance == md.m)
      
      # handle situations with multiple "optimal" parameters
      if (length(opt.m$s0) == 1) {
        s0.m <- opt.m$s0
      } else {
        if (mult.opt == "max") {
          warning("multiple optimal s0 - selecting maximum value.")
          s0.m <- max(opt.m$s0)
        }
        if (mult.opt == "min") {
          warning("multiple optimal s0 - selecting minimum value.")
          s0.m <- min(opt.m$s0)
        }
      }
      
      if (length(opt.m$s1) == 1) {
        s1.m <- opt.m$s1
      } else {
        if (mult.opt == "max") {
          warning("multiple optimal s1 - selecting maximum value.")
          s1.m <- max(opt.m$s1)
        }
        if (mult.opt == "min") {
          warning("multiple optimal s1 - selecting minimum value.")
          s1.m <- min(opt.m$s1)
        }
      }
      
      # validate in test set 
      val.m <- validate_ssnet(
        model = model, alpha = alpha, 
        s0 = s0.m, s1 = s1.m,
        family = "multinomial", type.multinomial = "grouped",
        x.train = as.matrix(dat.train[[m]] %>% select(-Y)),
        x.test = as.matrix(dat.test[[m]] %>% select(-Y)),
        y.train = dat.train[[m]]$Y, y.test = dat.test[[m]]$Y,
        output_param_est = TRUE
      )
      
      model_fitness <- rbind(
        model_fitness,
        cbind(sim.num = m, 
              s0 = s0.m, s1 = s1.m,
              val.m$model_fitness)
      )
      param_est <- rbind(
        param_est,
        cbind(sim.num = m,
              s0 = s0.m, s1 = s1.m,
              val.m$param_est)
      )
    },
    error = function(e) {
      message("* Caught an error on iteration ", m)
      print(e)
      write_lines(paste("Iteration #", m , ":", as.character(e)),
                  file = paste0("/data/user/jleach/sim_mn_2021/errors/errors_ssl_M5000_X1k_B6_BV0075015_NTE100_",
                                runID, ".txt"),
                  append = TRUE)
      # write_lines(paste("Iteration #", m , ":", as.character(e)),
      #             path = "/data/user/jleach/sim_mn_2021/errors/errors_tr_ssl.txt",
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
        file = paste0("/data/user/jleach/sim_mn_2021/results/results_ssl_M5000_X1k_B6_BV0075015_NTR300_NTE100_NCV5_",
                      runID, ".RDS"))
# saveRDS(out,
#         file = "/data/user/jleach/sim_mn_2021/results/results_tr_ssl.RDS")

