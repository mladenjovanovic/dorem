# Function that is called for optimization
dorem_optim <- function(par, predict_func, predictors, outcome, control) {

  # Objective function
  objective_func <- function(par, predict_func, predictors, outcome, weights, loss_func, na.rm) {
    # Get the model predictions
    pred <- predict_func(par, predictors)

    # Calculate the loss
    loss <- loss_func(
      obs = outcome,
      pred = pred,
      weights = weights,
      na.rm = na.rm
    )

    return(loss)
  }

  # =========================================
  # L-BFGS-B method
  if (control$optim_method == "L-BFGS-B") {
    model <- optimx::optimx(
      par = par,
      fn = objective_func,
      method = "L-BFGS-B",
      lower = control$coefs_lower,
      upper = control$coefs_upper,
      control = list(
        trace = control$optim_trace,
        maxit = control$optim_maxit
        #abstol = control$optim_VTR
      ),

      # ---------------------
      # ... parameters (extra)
      # to be forwarded to objective_func
      predict_func = predict_func,
      predictors = predictors,
      outcome = outcome,
      weights = control$weights,
      loss_func = control$loss_func,
      na.rm = control$na.rm
    )

    par <- stats::coef(model)
    loss_func_value <- model$value
  }

  # CMA-ES
  if (control$optim_method == "CMA-ES") {
    model <- cmaes::cma_es(
      par = par,
      fn = objective_func,
      lower = control$coefs_lower,
      upper = control$coefs_upper,
      control = list(
        trace = control$optim_trace,
        maxit = control$optim_maxit,
        stopfitness = control$optim_VTR
      ),

      # ---------------------
      # ... parameters (extra)
      # to be forwarded to objective_func
      predict_func = predict_func,
      predictors = predictors,
      outcome = outcome,
      weights = control$weights,
      loss_func = control$loss_func,
      na.rm = control$na.rm
    )

    par <- model$par
    loss_func_value <- model$value
  }

  # DE
  if (control$optim_method == "DE") {
    # Send warning if user provided starting values
    if (!is.null(control$coefs_start)) {
      warning("DE method doesn't use starting values for coefficients. Disregarding them.", call. = FALSE)

      # Remove starting coefs, since DE doesn't
      # use it
      control$coefs_start <- control$coefs_start * NA
    }

    model <- DEoptim::DEoptim(
      # par = par, # Not needed
      fn = objective_func,
      lower = control$coefs_lower,
      upper = control$coefs_upper,
      control = DEoptim::DEoptim.control(
        trace = control$optim_trace,
        itermax = control$optim_maxit,
        VTR = control$optim_VTR,
        #strategy = 2,
        #NP = 300,
        #CR = 0.9, F = 0.9
      ),

      # ---------------------
      # ... parameters (extra)
      # to be forwarded to objective_func
      predict_func = predict_func,
      predictors = predictors,
      outcome = outcome,
      weights = control$weights,
      loss_func = control$loss_func,
      na.rm = control$na.rm
    )

    par <- model$optim$bestmem
    loss_func_value <- model$optim$bestval
  }

  # --------------------------------------
  predicted <- predict_func(par, predictors)

  # Generate performance metrics
  performance <- control$perf_func(
    obs = outcome,
    pred = predicted,
    na.rm = control$na.rm
  )

  # List to be returned
  list(
    par = par,
    loss_func_value = loss_func_value,
    predicted = predicted,
    performance = performance,
    control = control
  )
}
