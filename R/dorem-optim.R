# Function that is called for optimization
dorem_optim <- function(par, predict_func, predictors, outcome, control) {
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

  # L-BFGS-B method
  if (control$optim_method == "L-BFGS-B") {
    model <- optimx::optimx(
      par = par,
      fn = objective_func,
      method = control$optim_method,
      lower = control$coefs_lower,
      upper = control$coefs_upper,
      control = list(
        trace = control$optim_trace,
        maxit = control$optim_maxit
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
    performance = performance
  )
}
