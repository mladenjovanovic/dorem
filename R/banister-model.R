# ------------------------------------------------------------------------------
# Training function
banister_train <- function(predictors, outcome, weights = NULL, control = banister_control()) {

  coefs <- banister_make_coefs(predictors)

  objective_func <- function(par, predictors, outcome, weights, coefs, loss_func, link_func) {

    # Convert par to coefs again
    new_coefs <- banister_vector_to_coefs(values = par, coefs = coefs)
    # Convert par back to coefs
    new_coefs  <- banister_par_to_coefs(new_coefs )

    model <- list(coefs = new_coefs, control = control)
    predicted <- banister_predict(model, predictors)

    # Loss/Objective function to be returned to optimizer
    loss_value <- loss_func(
      obs = outcome,
      pred = predicted,
      weights = weights)

    return(loss_value)
  }

  # Prepare coefs to be pars (so they can be bounded)
  par <- banister_coefs_to_par(coefs)
  # Convert list to named vector
  par <- banister_coefs_to_vector(par)

  # Create the lower bound (which are zeros)
  lower_bounds <- rep(0, length(par))
  upper_bounds <- rep(300, length(par))

  # Optim function
  model <- optimx::optimx(
    par = par,
    fn = objective_func,
    method = control$optim_method,
    #lower = NULL,
    #upper = upper_bounds,
    hessian = TRUE,
    control = list(
      trace = control$optim_trace,
      maxit = control$optim_maxit),

    # ---------------------
    # ... parameters (extra)
    # to be forwarded to objective_func
    predictors = predictors,
    outcome = outcome,
    weights = weights,
    coefs = coefs,
    loss_func = control$loss_func,
    link_func = control$link_func
    )

  model_coefs <- stats::coef(model)
  loss_func_value <- model$value

  # Extract coefs
  coefs <- banister_vector_to_coefs(model_coefs, coefs)
  # Convert par back to true cofs
  coefs <- banister_par_to_coefs(coefs)

  # calculate performance
  model <- list(coefs = coefs, control = control)
  predicted <- banister_predict(model, predictors)

  performance = control$perf_func(
    obs = outcome,
    pred = predicted
  )

  # List to be returned
  list(
    coefs = coefs,
    loss_func_value = loss_func_value,
    predicted = predicted,
    performance = performance)
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
    PTE_gain = 1, PTE_tau = 1,
    NTE_gain = 1, NTE_tau = 1
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
