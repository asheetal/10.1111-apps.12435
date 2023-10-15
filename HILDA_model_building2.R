#!/usr/bin/Rscript 
# Copyright (C) Abhishek Sheetal
# This file is part of HILDA methods project
#
# HILDA methods is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# HILDA methods is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with HILDA methods If not, see <http://www.gnu.org/licenses/>.
#
# This file is the main file for model building. It searches for optimal parameters
# and finally builds a best possible model
# 
#
suppressPackageStartupMessages({
  library(caret)
  library(naniar)
  library(AppliedPredictiveModeling)
  set.seed(12345)
  library(caret)
  library(xgboost)
  library(dplyr)
  library(mlrMBO)
  library(missRanger)
  library(filenamer)
  library(testit)
  library(rstanarm)
  library(bayestestR)
  library(ggthemes)
  library(mice)
  library(rstan)
  library(rstanarm)
  library(brms)
  library(parallel)
  library(bnstruct)
  library(glmnet)
  library(purrr)
})

MODEL_VAR <- Sys.getenv("MODEL")
MODEL_VAR <- ifelse(MODEL_VAR == "", "neurotic", MODEL_VAR)

COMPLETE <- Sys.getenv("COMPLETE")
COMPLETE <- ifelse(COMPLETE == "", FALSE, as.logical(as.integer(COMPLETE)))

print(paste("Building model in ", MODEL_VAR, COMPLETE))

BASE_PATH <- "/research/dataset/HILDA/HILDA_methods" #Give proper full path name where all outpuut files will be saved

GPU_ID <- 0 #If there are multiple graphics cards in the system, use this to control which GPU will be used to run
OBJECTIVE <- "reg:squarederror"
EVAL_METRIC <- list("rmse") #for regression use rmse, Check xgboost manual for other eval_metric
TRAIN_DATA <- paste(BASE_PATH, "/all_data_2022-03-14.rds", sep="")
PREPROCESS <- c("center", "scale")

METHOD <- "xgbTree" #Do not change this unless prepared to do a major surgery of this code
TREE_METHOD <- "gpu_hist" #Options are hist for CPU based processing or gpu_hist for GPU based processing. can try others from the xgboost manual
THREADS <- 4 #change this only if you wish to explore how deep the rabbit hole goes
DEBUG_FILE <- "/research/dataset/HILDA/HILDA_methods/bayes_debug.log" #sometimes the program will crash, you can try to debug
TRIALS <- 200 #You could watch your hair get gray, so increase with caution
#this N should control all corners of bounds. Increase or decrease this N if all corners are not addressed in seeding. (10 worked for xgbTree)
big5 <- c("extrav", "agree", "consc", "neurotic", "open")
big5_subitem_wild <- paste0(big5, "*")

options(filenamer.timestamp=1)
SAVE_MODEL <- filename(paste0("xgb_bayes_completed_model_", MODEL_VAR, "_", COMPLETE),
                       path=BASE_PATH,
                       tag=NULL,
                       ext="RData",
                       subdir=FALSE) %>%
  as.character() %>%
  print()

df.pre <- readRDS(TRAIN_DATA) %>%
  pluck("df.train.imputed") %>%
  as.data.frame() %>%
  filter(!is.na(get(MODEL_VAR)))

all_big5 <- grep(paste(big5_subitem_wild, collapse="|"), names(df.pre), value = TRUE)
INDEPENDANT_VARS <- names(df.pre) %>%
  setdiff(all_big5)

if (COMPLETE) {
  df <- df.pre %>%
    na.omit() %>%
    select(c(INDEPENDANT_VARS, MODEL_VAR))
} else {
  df <- df.pre %>%
    select(c(INDEPENDANT_VARS, MODEL_VAR))
}

#drop 1 column percentage
col_sel <- (ncol(df) - 1) / ncol(df)

#determine the weights
weight_table <- table(floor(df[,MODEL_VAR] + 0.5)) %>%
  as.data.frame() %>%
  mutate(weight = min(Freq)/Freq)
df_weights <- weight_table$weight[floor(df[,MODEL_VAR]+0.5)]

#bounds: Refer to xgboost manual on explanations. 
#this n value must be adjusted to make sure seedgrid below covers all corners of the bounds above
SEED_N <- 10
par.set = makeParamSet(
  makeIntegerParam("max_depth", lower = 3L, upper = 23L),
  makeNumericParam("eta", lower = 0.000001, upper = .999999),
  makeNumericParam("min_child_weight", lower= 0L, upper = 2L),
  #makeNumericParam("subsample", lower = 1.0, upper = 1.0),
  #makeNumericParam("colsample_bytree", lower = 1.0, upper = 1.0),
  makeNumericParam("gamma", lower = 0L, upper = 20L),
  makeIntegerParam("nrounds", lower = 500L, upper = 2000L))

#This is the trial model to check seed bounds. If the trial fails, then there is some issue with bounds
form <- paste(MODEL_VAR, "~", paste(INDEPENDANT_VARS, collapse="+")) %>% 
  as.formula()
set.seed(101)

#Should not need any more changes after this comment
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  allowParallel = TRUE
)

fn <- function(x){   
  set.seed(101)
  train_model = caret::train(form,
                             data = df,
                             na.action = na.pass,
                             weights = df_weights,
                             method = METHOD,
                             objective = OBJECTIVE,
                             eval_metric = EVAL_METRIC,
                             trControl = fitControl,
                             tuneGrid = expand.grid(max_depth = x$max_depth, 
                                                    eta = x$eta,
                                                    min_child_weight = x$min_child_weight,
                                                    subsample = 1.0, #x$subsample, 
                                                    colsample_bytree = col_sel, #x$colsample_bytree,
                                                    gamma = x$gamma, 
                                                    nrounds = x$nrounds),
                             #preProcess = PREPROCESS,
                             tree_method=TREE_METHOD, #only for computers where GPU is enabled
                             gpu_id = GPU_ID, 
                             nthread=THREADS)
  getTrainPerf(train_model)[, "TrainRMSE"]
}

obj.fun  <- smoof::makeSingleObjectiveFunction(
  name = "xgb_cv_bayes",
  has.simple.signature = FALSE,
  fn =  fn,
  par.set = par.set,
  minimize = TRUE
)

control <- makeMBOControl()
control <- setMBOControlTermination(control, iters = TRIALS)

des <- generateDesign(n = SEED_N,
                      par.set = getParamSet(obj.fun), 
                      fun = lhs::randomLHS)
print("Fitting model with bayesian optimized hyperparameters..\n")
run <- mbo(fun = obj.fun, 
           control = control, 
           show.info = TRUE,
           design = des)

################3
print("Completed Hyperpameter..Now building final model\n")

set.seed(101)
xgb.tuned.bayes <- caret::train(form,
                         df,
                         na.action = na.pass,                  
                         weights = df_weights,
                         method=METHOD,
                         objective = OBJECTIVE,
                         eval_metric = EVAL_METRIC,
                         tuneGrid = data.frame(max_depth = run$x["max_depth"],
                                               eta = run$x["eta"],
                                               min_child_weight = run$x["min_child_weight"],
                                               subsample = 1.0, #run$x["subsample"],
                                               colsample_bytree = col_sel, #run$x["colsample_bytree"],
                                               gamma = run$x["gamma"],
                                               nrounds = run$x["nrounds"]),
                         trControl = fitControl,
                         #preProcess = PREPROCESS,
                         tree_method=TREE_METHOD,
                         gpu_id = GPU_ID,
                         nthread=THREADS)

save.image(SAVE_MODEL)
print("XGBOOST Model building is completed")
prior <- get_prior(formula = form, 
                   data = df, 
                   family = gaussian())
model_bayes <- brm(formula = form, 
                   data = df, 
                   prior = prior,
                   family = gaussian(), 
                   warmup = 1000, 
                   iter = 3000,
                   chains = 6, 
                   cores = 6,
                   seed = 123)
save.image(SAVE_MODEL)
print("MCMC Model building is completed")
