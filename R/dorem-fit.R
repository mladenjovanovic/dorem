#' Fit a `dorem`
#'
#' `dorem()` fits a model.
#'
#' @param x Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `x` is a __data frame__ or __matrix__, `y` is the outcome
#' specified as:
#'
#'   * A __data frame__ with 1 numeric column.
#'   * A __matrix__ with 1 numeric column.
#'   * A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return
#'
#' A `dorem` object.
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
#'
#' @export
dorem <- function(x, ...) {
  UseMethod("dorem")
}

#' @export
#' @rdname dorem
dorem.default <- function(x, ...) {
  stop("`dorem()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname dorem
dorem.data.frame <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  dorem_bridge(processed, ...)
}

# XY method - matrix

#' @export
#' @rdname dorem
dorem.matrix <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  dorem_bridge(processed, ...)
}

# Formula method

#' @export
#' @rdname dorem
dorem.formula <- function(formula, data, ...) {
  processed <- hardhat::mold(formula, data)
  dorem_bridge(processed, ...)
}

# Recipe method

#' @export
#' @rdname dorem
dorem.recipe <- function(x, data, ...) {
  processed <- hardhat::mold(x, data)
  dorem_bridge(processed, ...)
}

# ------------------------------------------------------------------------------
# Bridge

dorem_bridge <- function(processed, ...) {
  predictors <- processed$predictors
  outcome <- processed$outcomes

  # Validate
  hardhat::validate_outcomes_are_univariate(outcome)
  outcome <- outcome[[1]]

  fit <- dorem_impl(predictors, outcome, ...)

  new_dorem(
    method = fit$method,
    data = fit$data,
    coefs = fit$coefs,
    loss_func_value = fit$loss_func_value,
    performance = fit$performance,
    cross_validation = fit$cross_validation,
    control = fit$control,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation
dorem_impl <- function(predictors, outcome, method = "banister", control = dorem_control()) {
  # Check if method is correct
  rlang::arg_match(method, valid_dorem_methods())

  # Select appropriate train function based on the method employed
  dorem_train_func <- switch(
    method,
    banister = banister_train
  )

  # Set-up seed for reproducibility
  set.seed(control$seed)

  # Set up weights
  if(is.null(control$weights)) {
    control$weights <- rep(1, length(outcome))
  }

  # Perform model
  train_results <- dorem_train_func(predictors, outcome, control)

  cross_validation <- 0

  # Return object
  list(
    method = method,
    data = list(
      predictors = predictors,
      outcome = outcome,
      predicted =  train_results$predicted),
    coefs = train_results$coef,
    loss_func_value = train_results$loss_func_value,
    performance = train_results$performance,
    cross_validation = cross_validation,
    control = control)
}


# ------------------------------------------------------------------------------
# All valid dorem methods
valid_dorem_methods <- function() {
  c("banister")
}
