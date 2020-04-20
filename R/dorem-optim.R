# Function that is called for optimization
dorem_optim <- function(par, predict_func, predictors, outcome, control) {

  objective_func <- function(par, predict_func, predictors, outcome, weights, na.rm) {

    pred <- predict_func(par, predictors)

      MSE <- mean((weights * (pred - outcome))^2, na.rm = na.rm)

      return(MSE)
  }

  # L-BFGS-B method
  model <- optimx::optimx(
    par = par,
    fn = objective_func,
    method = control$optim_method,
    #lower = lower_bounds,
    #upper = upper_bounds,
    hessian = TRUE,
    control = list(
      trace = control$optim_trace,
      maxit = control$optim_maxit),

    # ---------------------
    # ... parameters (extra)
    # to be forwarded to objective_func
    predict_func = predict_func,
    predictors = predictors,
    outcome = outcome,
    weights = control$weights,
    na.rm = control$na.rm
  )

  par <- stats::coef(model)
  loss_func_value <- model$value
  predicted <- predict_func(par, predictors)

  # Generate performance metrics
  performance = control$perf_func(
    obs = outcome,
    pred = predicted
  )

  # List to be returned
  list(
    par = par,
    loss_func_value = loss_func_value,
    predicted = predicted,
    performance = performance)
}
