# ------------------------------------------------------------------------------
# Training function
banister_train <- function(predictors, outcome, control = banister_control()) {

  # Function that converts par to coef and returns predictions
  predict_func <- function(par, predictors) {

    # create coefs from predictors (to get the right format)
    coefs <- banister_make_coefs(predictors)

    # Convert par to coefs again
    new_coefs <- banister_vector_to_coefs(values = par, coefs = coefs)

    # Convert optim coef back to true coefs
    new_coefs  <- banister_par_to_coefs(new_coefs)

    model <- list(coefs = new_coefs, control = control)
    predicted <- banister_predict(model, predictors)

    return(predicted)
  }

  # -------------------------------------
  # Get coefs format
  coefs <- banister_make_coefs(predictors)

  # Prepare coefs to be pars (so they can be bounded)
  coefs <- banister_coefs_to_par(coefs)
  # Convert list to named vector
  par <- banister_coefs_to_vector(coefs)

  # Create the lower bound (which are zeros)
  #lower_bounds <- rep(Inf, length(par))
  #upper_bounds <- rep(-Inf, length(par))

 # Call to optim function
  opt_res <- dorem_optim(
    par = par,
    predict_func = predict_func,
    predictors = predictors,
    outcome = outcome,
    control = control)

  # Extract coefs
  coefs <- banister_vector_to_coefs(opt_res$par, coefs)
  # Convert par back to true cofs
  coefs <- banister_par_to_coefs(coefs)

  # List to be returned
  list(
    coefs = coefs,
    loss_func_value = opt_res$loss_func_value,
    predicted = opt_res$predicted,
    performance = opt_res$performance)
}

# ------------------------------------------------------------------------------
# Function to predict outcome from coefs and predictors
banister_predict <- function(model, predictors) {
  coefs <- model$coefs

  # Get and remove intercept
  intercept <- coefs[[1]]
  coefs[[1]] <- NULL

  # Function to go over predictors and coefs
  training_responses <- purrr::map(
    coefs,
    predictors,
    .f = function(.x, .y) {
    # Function to calculate rolling training effects
    # See Clarke DC, Skiba PF. 2013. <DOI: 10.1152/advan.00078.2011> for more info

    effect_func <- function(prev, current, tau) {
      (prev * exp(-1/tau) + current)
    }

    PTE <- .x$PTE_gain * purrr::accumulate(.y[[1]], effect_func, tau = .x$PTE_tau)
    NTE <- .x$NTE_gain * purrr::accumulate(.y[[1]], effect_func, tau = .x$NTE_tau)

    return(PTE - NTE)
    })

  # Combine training responses
  total_response <- intercept + purrr::pmap_dbl(training_responses, sum)

  # Apply the link function
  link_func <- model$control$link_func
  total_response <- link_func(total_response)
return(total_response)
}

# --------------------------------------------------------------------------------------------
banister_make_coefs <- function(predictors) {
  coefs <- purrr::map(predictors, function(...){list(
    PTE_gain = 1, PTE_tau = 14,
    NTE_gain = 3, NTE_tau = 7
  )})

  c(intercept = 260, coefs)
}

# ---------------------------------------------------------------------
banister_coefs_to_vector <- function(coefs) {
  unlist(coefs)
}

# ---------------------------------------------------------------------
banister_vector_to_coefs <- function(values, coefs) {
  coefs_new <- coefs

  # Assign intercept
  coefs_new[[1]] <- values[[1]]

  n_predictors <- length(coefs)

  for (i in seq(2, n_predictors)) {
    coefs_new[[i]]$PTE_gain = values[[1 + (i-2) * 4 + 1]]
    coefs_new[[i]]$PTE_tau = values[[1 + (i-2) * 4 + 2]]
    coefs_new[[i]]$NTE_gain = values[[1 + (i-2) * 4 + 3]]
    coefs_new[[i]]$NTE_tau = values[[1 + (i-2) * 4 + 4]]
  }

  return(coefs_new)
}

# ---------------------------------------------------------------------
# Adjust the coefs so upper and lower bounds can be set
banister_coefs_to_par <- function(coefs) {
  # Get and remove intercept
  intercept <- coefs[[1]]
  coefs[[1]] <- NULL

  coefs <- purrr::map(coefs, function(.x){list(
    PTE_gain = .x$PTE_gain, PTE_tau = .x$PTE_tau - .x$NTE_tau,
    NTE_gain = .x$NTE_gain - .x$PTE_gain, NTE_tau = .x$NTE_tau
  )})

  c(intercept = intercept, coefs)
}

# ---------------------------------------------------------------------
# Adjust the coefs back from the par (optim function)
banister_par_to_coefs <- function(coefs) {
  # Get and remove intercept
  intercept <- coefs[[1]]
  coefs[[1]] <- NULL

  coefs <- purrr::map(coefs, function(.x){list(
    PTE_gain = .x$PTE_gain, PTE_tau = .x$PTE_tau + .x$NTE_tau,
    NTE_gain = .x$NTE_gain + .x$PTE_gain, NTE_tau = .x$NTE_tau
  )})

  c(intercept = intercept, coefs)
}
