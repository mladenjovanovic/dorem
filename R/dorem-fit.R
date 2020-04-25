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
    shuffle = fit$shuffle,
    control = fit$control,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation
dorem_impl <- function(predictors, outcome, method = "banister", control = dorem_control()) {
  # Pull out iter control
  iter <- control$iter

  # Check if method is correct
  rlang::arg_match(method, valid_dorem_methods())

  # Select appropriate train function based on the method employed
  dorem_train_func <- switch(
    method,
    banister = banister_train
  )

  if (iter) {
    message(paste("Performing", method, "method using", control$optim_method, "optimization"))
  }

  # Set-up seed for reproducibility
  set.seed(control$seed)

  # Set up weights
  if (is.null(control$weights)) {
    control$weights <- rep(1, length(outcome))
  }

  # ===================================
  # Train model
  if (iter) {
    message("Training the model...")
  }
  train_results <- dorem_train_func(predictors, outcome, control)

  # ===================================
  # Cross validation
  cross_validation <- NA

  if(!is.null(control$cv_folds)) {
    # If there is no repeats defined then assume 1
    if(is.null(control$cv_repeats)) {
      control$cv_repeats <- 1
      train_results$control <- 1
    }

    if (iter) {
      message(paste("Cross-validating the model using", control$cv_repeats,
                    "repeats of", control$cv_folds, "folds"))
    }

    cv_outcome_index <- seq(1, length(outcome))

    # Remove the missing rows/indexes
    if (control$na.rm == TRUE) {
      cv_outcome_index <- cv_outcome_index[!is.na(outcome)]
    }

    # Create CV folds
    cv_folds <- caret::createMultiFolds(
      y = cv_outcome_index,
      k = control$cv_folds,
      times = control$cv_repeats
    )

    # Loop through CV folds
    cv_results <- purrr::map2(cv_folds, names(cv_folds), function(cv_folds, fold_name) {
      if (iter) {
        message(paste(fold_name, "...", sep = ""))
      }

      cv_train_index <- cv_outcome_index[cv_folds]
      cv_test_index <- cv_outcome_index[-cv_folds]

      # Create test and train outcome partitions
      cv_train_outcome <- outcome
      cv_train_outcome[-cv_train_index] <- NA

      cv_test_outcome <- outcome
      cv_test_outcome[-cv_test_index] <- NA

      # Train
      cv_train_results <- dorem_train_func(predictors, cv_train_outcome, control)

      # Test
      test_performance <- control$perf_func(
        obs = cv_test_outcome,
        pred = cv_train_results$predicted,
        na.rm = control$na.rm
      )

      return(list(
        data = list(
          train_predictors = predictors,
          train_outcome = cv_train_outcome,
          train_predicted = cv_train_results$predicted,
          test_predictors = predictors,
          test_outcome = cv_test_outcome,
          test_predicted = cv_train_results$predicted
        ),
        coefs = cv_train_results$coef,
        loss_func_value = cv_train_results$loss_func_value,
        performance = test_performance
      ))
    })

    cross_validation <- cv_results
  } # End of Cross-Validation

  # ===================================
  # Shuffle
  shuffle <- NA
  if (control$shuffle == TRUE) {
    if (iter) {
      message("Training the model using shuffled predictors...")
    }

    # Shuffle the predictors
    rows <- sample(nrow(predictors))
    predictors <- predictors[rows, ]

    shuffle_results <- dorem_train_func(predictors, outcome, control)

    shuffle <- list(
      data = list(
        predictors = predictors,
        outcome = outcome,
        predicted = shuffle_results$predicted
      ),
      coefs = shuffle_results$coef,
      loss_func_value = shuffle_results$loss_func_value,
      performance = shuffle_results$performance
    )
  } # End of Shuffle

  if (iter) {
    message("Done!")
  }

  # Return object
  list(
    method = method,
    data = list(
      predictors = predictors,
      outcome = outcome,
      predicted = train_results$predicted
    ),
    coefs = train_results$coef,
    loss_func_value = train_results$loss_func_value,
    performance = train_results$performance,
    cross_validation = cross_validation,
    shuffle = shuffle,
    control = train_results$control
  )
}


# ------------------------------------------------------------------------------
# All valid dorem methods
valid_dorem_methods <- function() {
  c("banister")
}
