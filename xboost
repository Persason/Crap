
paste0("


dir.create("R", showWarnings = FALSE)

download.file(
  paste0(
    "https://raw.githubusercontent.com/tagteam/rtmle/",
    "7bfb3a8d732694a8f7d090586f7306e43c4b637a/",
    "R/learn_xgboost.R"
  ),
  destfile = "R/learn_xgboost_legacy.R",
  mode = "wb"
)




source("R/learn_xgboost_legacy.R")



tar_target(
  learners_comb,
  list(
    xgboost = list(
      learner_fun = "learn_xgboost",
      xgboost.objective = "reg:logistic",
      xgboost.nrounds = 100L,
      xgboost.max_depth = 2L,
      xgboost.eta = 0.05,
      xgboost.nthread = 1L
    ),
    glmnet = list(
      learner_fun = "learn_glmnet"
    )
  )
),




if (length(learner) > 1L) {
  x <- run_rtmle(
    x,
    learner = learner,
    time_horizon = times,
    folds = 2
  )
} else {
  x <- run_rtmle(
    x,
    learner = learner,
    time_horizon = times
  )
}


")

