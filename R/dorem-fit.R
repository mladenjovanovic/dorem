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
#' predictors <- mtcars[, -1]
#' outcome <- mtcars[, 1]
#'
#' # XY interface
#' mod <- dorem(predictors, outcome)
#'
#' # Formula interface
#' mod2 <- dorem(mpg ~ ., mtcars)
#'
#' # Recipes interface
#' library(recipes)
#' rec <- recipe(mpg ~ ., mtcars)
#' rec <- step_log(rec, disp)
#' mod3 <- dorem(rec, mtcars)
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
    coefs = fit$coefs,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation

dorem_impl <- function(predictors, outcome) {
  list(coefs = 1)
}
