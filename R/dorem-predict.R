#' Predict from a `dorem`
#'
#' @param object A `dorem` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"numeric"` for numeric predictions.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @examples
#' require(tidyverse)
#'
#' data("bike_score")
#'
#' banister_model <- dorem(
#'   Test_5min_Power ~ BikeScore,
#'   bike_score,
#'   method = "banister"
#' )
#'
#' bike_score$pred <- predict(banister_model, bike_score)$.pred
#'
#' ggplot(bike_score, aes(x = Day, y = pred)) +
#'   theme_bw() +
#'   geom_line() +
#'   geom_point(aes(y = Test_5min_Power), color = "red") +
#'   ylab("Test 5min Power")
#' @export
predict.dorem <- function(object, new_data, type = "numeric", ...) {
  forged <- hardhat::forge(new_data, object$blueprint)
  rlang::arg_match(type, valid_predict_types())
  predict_dorem_bridge(type, object, forged$predictors)
}

valid_predict_types <- function() {
  c("numeric")
}

# ------------------------------------------------------------------------------
# Bridge

predict_dorem_bridge <- function(type, model, predictors) {
  predict_function <- get_predict_function(type)
  predictions <- predict_function(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

get_predict_function <- function(type) {
  switch(
    type,
    numeric = predict_dorem_numeric
  )
}

# ------------------------------------------------------------------------------
# Implementation

predict_dorem_numeric <- function(model, predictors) {
  # Select appropriate prediction function based on the method employed
  dorem_predict_func <- switch(
    model$method,
    banister = banister_predict
  )

  predictions <- dorem_predict_func(model, predictors)
  hardhat::spruce_numeric(predictions)
}
