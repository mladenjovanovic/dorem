# Function that is called for optimization
dorem_optim <- function(optim_method, objective_func, control, ...) {

  obj_func_ <- function(par, objective_func, ...) {
      resid <- objective_func(par, ...)
  }
}
