# ------------------------------------------------------------------------------
# Training function
EWMA_train <- function(predictors, outcome, control = dorem_control()) {

  # Function that converts par to coef and returns predictions
  predict_func <- function(par, predictors) {

    # create coefs from predictors (to get the right format)
    coefs <- EWMA_make_coefs(predictors)

    # Convert par to coefs again
    coefs <- EWMA_vector_to_coefs(values = par, coefs = coefs)

    # Predict
    model <- list(coefs = coefs, control = control)
    predicted <- EWMA_predict(model, predictors)

    return(predicted)
  }

  # -------------------------------------
  # Get coefs
  coefs_start <- EWMA_coefs_start(predictors, outcome, control$na.rm)
  coefs_lower <- EWMA_coefs_lower(predictors, outcome, control$na.rm)
  coefs_upper <- EWMA_coefs_upper(predictors, outcome, control$na.rm)

  # Check if user provided starting coefs
  if (!is.null(control$coefs_start)) {
    coefs_start <- EWMA_vector_to_coefs(control$coefs_start, coefs_start)
  }

  # Check if user provided coefs lower bound
  if (!is.null(control$coefs_lower)) {
    coefs_lower <- EWMA_vector_to_coefs(control$coefs_lower, coefs_lower)
  }

  # Check if user provided coefs upper bound
  if (!is.null(control$coefs_upper)) {
    coefs_upper <- EWMA_vector_to_coefs(control$coefs_upper, coefs_upper)
  }

  # Convert coefs to par
  control$coefs_start <- EWMA_coefs_to_vector(coefs_start)
  control$coefs_lower <- EWMA_coefs_to_vector(coefs_lower)
  control$coefs_upper <- EWMA_coefs_to_vector(coefs_upper)

  # Call to optim function
  opt_res <- dorem_optim(
    par = control$coefs_start,
    predict_func = predict_func,
    predictors = predictors,
    outcome = outcome,
    control = control
  )

  # Extract coefs
  coefs_result <- EWMA_vector_to_coefs(opt_res$par, coefs_start)

  # List to be returned
  list(
    coefs = coefs_result,
    loss_func_value = opt_res$loss_func_value,
    predicted = opt_res$predicted,
    performance = opt_res$performance,
    control = opt_res$control,
    optim_model = opt_res$optim_model
  )
}

# ------------------------------------------------------------------------------
# Function to predict outcome from coefs and predictors
EWMA_predict <- function(model, predictors) {
  coefs <- model$coefs

  # Get and remove intercept
  intercept <- coefs[[1]]
  coefs[[1]] <- NULL

  predictors <- as.list(predictors)
  # Function to go over predictors and coefs
  training_responses <- purrr::map2(
    coefs,
    predictors,
    .f = function(.x, .y) {
      # Function to calculate EWMA
      PTE <- .x$PTE_gain * TTR::EMA(x = .y, n = 1, ratio = 2/(.x$PTE_tau + 1))
      NTE <- .x$NTE_gain * TTR::EMA(x = .y, n = 1, ratio = 2/(.x$NTE_tau + 1))
      return(PTE - NTE)
    }
  )
  # Combine training responses
  total_response <- intercept + purrr::pmap_dbl(training_responses, sum)

  # =======================
  # Apply the link function
  link_func <- model$control$link_func
  total_response <- link_func(total_response)

  return(total_response)
}

# ============================================================================================
# The following function deal with coefs and their conversion to par
# --------------------------------------------------------------------------------------------
EWMA_make_coefs <- function(predictors) {
  coefs <- purrr::map(predictors, function(...) {
    list(
      PTE_gain = NA, PTE_tau = NA,
      NTE_gain = NA, NTE_tau = NA
    )
  })

  c(intercept = NA, coefs)
}

EWMA_coefs_start <- function(predictors, outcome, na.rm = TRUE) {
  coefs <- purrr::map(predictors, function(...) {
    list(
      PTE_gain = 1, PTE_tau = 21,
      NTE_gain = 3, NTE_tau = 7
    )
  })

  c(intercept = min(outcome, na.rm = na.rm), coefs)
}

# --------------------------------------------------------------------------------------------
EWMA_coefs_lower <- function(predictors, outcome, na.rm = TRUE) {
  coefs <- purrr::map(predictors, function(...) {
    list(
      PTE_gain = 0, PTE_tau = 0,
      NTE_gain = 0, NTE_tau = 0
    )
  })

  c(intercept = 0, coefs)
}

# --------------------------------------------------------------------------------------------
EWMA_coefs_upper <- function(predictors, outcome, na.rm = TRUE) {
  coefs <- purrr::map(predictors, function(...) {
    list(
      PTE_gain = 10000, PTE_tau = 300,
      NTE_gain = 10000, NTE_tau = 300
    )
  })

  c(intercept = max(outcome, na.rm = na.rm), coefs)
}

# ---------------------------------------------------------------------
EWMA_coefs_to_vector <- function(coefs) {
  unlist(coefs)
}

# ---------------------------------------------------------------------
EWMA_vector_to_coefs <- function(values, coefs) {
  coefs_new <- coefs

  # Assign intercept
  coefs_new[[1]] <- values[[1]]

  n_predictors <- length(coefs)

  for (i in seq(2, n_predictors)) {
    coefs_new[[i]]$PTE_gain <- values[[1 + (i - 2) * 4 + 1]]
    coefs_new[[i]]$PTE_tau <- values[[1 + (i - 2) * 4 + 2]]
    coefs_new[[i]]$NTE_gain <- values[[1 + (i - 2) * 4 + 3]]
    coefs_new[[i]]$NTE_tau <- values[[1 + (i - 2) * 4 + 4]]
  }

  return(coefs_new)
}
