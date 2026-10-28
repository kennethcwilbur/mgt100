# MGT 100 model-fit functions, as defined in the week-3 class script. Load with:
#   source("https://raw.githubusercontent.com/kennethcwilbur/mgt100/main/data/mnl_functions.R")
hit_rate <- function(model) {
  probs <- fitted(model, type = "probabilities")
  predicted <- colnames(probs)[apply(probs, 1, which.max)]
  actual <- idx(model$model)$phone_id[model$model$choice]
  mean(predicted == actual)
}
ll_ratio <- function(model) {
  probs <- fitted(model, type = "probabilities")
  ll_equal <- nrow(probs) * log(1 / ncol(probs))
  1 - as.numeric(logLik(model)) / ll_equal
}
