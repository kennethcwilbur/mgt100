# MGT 100 model-fit functions, with the arguments the week-3 class script gives them. Load with:
#   source("https://raw.githubusercontent.com/kennethcwilbur/mgt100/main/data/mnl_functions.R")
# data is the dfidx table a model was fit on; model is a fitted mlogit model.
product_hit_rate <- function(data, model) {
  probs <- predict(model, newdata = data)                    # one row per customer, one column per phone
  predicted <- colnames(probs)[apply(probs, 1, which.max)]   # each customer's most likely phone
  actual <- idx(data)$phone_id[data$choice]                  # the phone each customer bought
  mean(predicted == actual)
}
brand_hit_rate <- function(data, model) {
  probs <- predict(model, newdata = data)
  predicted <- colnames(probs)[apply(probs, 1, which.max)]
  actual <- idx(data)$phone_id[data$choice]
  mean(substr(predicted, 1, 1) == substr(actual, 1, 1))      # the first letter of a phone id is its brand
}
ll_ratio <- function(data, model) {
  probs <- predict(model, newdata = data)
  ll_equal <- nrow(probs) * log(1 / ncol(probs))             # log-likelihood when every phone gets the same chance
  1 - as.numeric(logLik(model)) / ll_equal
}
