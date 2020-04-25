#' S3 method for extracting model coefficients
#' @param object Object of class \code{dorem}
#' @export
#' @examples
#' data("bike_score")
#'
#' banister_model <- dorem(
#'   Test_5min_Power ~ BikeScore,
#'   bike_score,
#'   method = "banister"
#' )
#' coef(banister_model)
coef.dorem <- function(object) {
  unlist(object$coefs)
}
