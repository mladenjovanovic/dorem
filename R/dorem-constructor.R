new_dorem <- function(method, coefs, performance, control, blueprint) {
  hardhat::new_model(
    method = method,
    coefs = coefs,
    performance = performance,
    control = control,
    blueprint = blueprint,
    class = "dorem")
}
