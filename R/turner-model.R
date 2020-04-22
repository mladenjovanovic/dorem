# ------------------------------------------------------------------------------
# Training function
turner_train <- function(predictors, outcome, control = dorem_control()) {

  # Function that converts par to coef and returns predictions
  predict_func <- function(par, predictors) {

    # create coefs from predictors (to get the right format)
    coefs <- turner_make_coefs(predictors)

    # Convert par to coefs again
    coefs <- turner_vector_to_coefs(values = par, coefs = coefs)

    # Predict
    model <- list(coefs = coefs, control = control)
    predicted <- turner_predict(model, predictors)

    return(predicted)
  }

  # -------------------------------------
  # Get coefs
  coefs_start <- turner_coefs_start(predictors, outcome, control$na.rm)
  coefs_lower <- turner_coefs_lower(predictors, outcome, control$na.rm)
  coefs_upper <- turner_coefs_upper(predictors, outcome, control$na.rm)

  # Check if user provided starting coefs
  if (!is.null(control$coefs_start)) {
    coefs_start <- turner_vector_to_coefs(control$coefs_start, coefs_start)
  }

  # Check if user provided coefs lower bound
  if (!is.null(control$coefs_lower)) {
    coefs_lower <- turner_vector_to_coefs(control$coefs_lower, coefs_lower)
  }

  # Check if user provided coefs upper bound
  if (!is.null(control$coefs_upper)) {
    coefs_upper <- turner_vector_to_coefs(control$coefs_upper, coefs_upper)
  }

  # Convert coefs to par
  control$coefs_start <- turner_coefs_to_vector(coefs_start)
  control$coefs_lower <- turner_coefs_to_vector(coefs_lower)
  control$coefs_upper <- turner_coefs_to_vector(coefs_upper)

  # Call to optim function
  opt_res <- dorem_optim(
    par = control$coefs_start,
    predict_func = predict_func,
    predictors = predictors,
    outcome = outcome,
    control = control
  )

  # Extract coefs
  coefs_result <- turner_vector_to_coefs(opt_res$par, coefs_start)

  # List to be returned
  list(
    coefs = coefs_result,
    loss_func_value = opt_res$loss_func_value,
    predicted = opt_res$predicted,
    performance = opt_res$performance,
    control = control
  )
}


# ------------------------------------------------------------------------------
# Function to predict outcome from coefs and predictors
turner_predict <- function(model, predictors) {
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

      # Function to calculate rolling training effects
      effect_func <- function(prev, current, tau, alpha) {
        (-1/tau * sign(prev)*(abs(prev)^alpha)) + current
      }
      PTE <- .x$PTE_gain * purrr::accumulate(.y, effect_func, tau = .x$PTE_tau, alpha = .x$PTE_alpha)
      NTE <- .x$NTE_gain * purrr::accumulate(.y, effect_func, tau = .x$NTE_tau, alpha = .x$NTE_alpha)

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
turner_make_coefs <- function(predictors) {
  coefs <- purrr::map(predictors, function(...) {
    list(
      PTE_gain = NA, PTE_tau = NA, PTE_alpha = NA,
      NTE_gain = NA, NTE_tau = NA, NTE_alpha = NA
    )
  })

  c(intercept = NA, coefs)
}

turner_coefs_start <- function(predictors, outcome, na.rm = TRUE) {
  coefs <- purrr::map(predictors, function(...) {
    list(
      PTE_gain = 1, PTE_tau = 21, PTE_alpha = 1,
      NTE_gain = 3, NTE_tau = 7, NTE_alpha = 1
    )
  })

  c(intercept = min(outcome, na.rm = na.rm), coefs)
}

# --------------------------------------------------------------------------------------------
turner_coefs_lower <- function(predictors, outcome, na.rm = TRUE) {
  coefs <- purrr::map(predictors, function(...) {
    list(
      PTE_gain = 0, PTE_tau = 0, PTE_alpha = 0,
      NTE_gain = 0, NTE_tau = 0, NTE_alpha = 0
    )
  })

  c(intercept = 0, coefs)
}

# --------------------------------------------------------------------------------------------
turner_coefs_upper <- function(predictors, outcome, na.rm = TRUE) {
  coefs <- purrr::map(predictors, function(...) {
    list(
      PTE_gain = 10000, PTE_tau = 300, PTE_alpha = 10,
      NTE_gain = 10000, NTE_tau = 300, NTE_alpha = 10
    )
  })

  c(intercept = max(outcome, na.rm = na.rm), coefs)
}

# ---------------------------------------------------------------------
turner_coefs_to_vector <- function(coefs) {
  unlist(coefs)
}

# ---------------------------------------------------------------------
turner_vector_to_coefs <- function(values, coefs) {
  coefs_new <- coefs

  # Assign intercept
  coefs_new[[1]] <- values[[1]]

  n_predictors <- length(coefs)

  for (i in seq(2, n_predictors)) {
    coefs_new[[i]]$PTE_gain <- values[[1 + (i - 2) * 6 + 1]]
    coefs_new[[i]]$PTE_tau <- values[[1 + (i - 2) * 6 + 2]]
    coefs_new[[i]]$PTE_alpha <- values[[1 + (i - 2) * 6 + 3]]
    coefs_new[[i]]$NTE_gain <- values[[1 + (i - 2) * 6 + 4]]
    coefs_new[[i]]$NTE_tau <- values[[1 + (i - 2) * 6 + 5]]
    coefs_new[[i]]$NTE_alpha <- values[[1 + (i - 2) * 6 + 6]]
  }

  return(coefs_new)
}

