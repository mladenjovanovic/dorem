#' Cycling BikeScore and 5min Test Power
#'
#' Example data provided in \href{https://journals.physiology.org/doi/full/10.1152/advan.00078.2011m}{supplementary material}
#'     of Clarke & Skiba, 2013 paper, freely available on the publisher website. Data set contains cycling training load (i.e. dose)
#'     measured using the BikeScore metric (in AU) over 165 days, with occasional training response measured using 5-min Power Test (in Watts).
#'     Using Banister model implemented in Microsoft Excel, Clarke & Skiba, 2013 estimate the following parameters:
#'     \itemize{
#'       \item Intercept of 262 W
#'       \item Gain of Positive Training Effect of 0.18 W/AU
#'       \item Gain of Negative Training Effect of 0.23 W/AU
#'       \item Time Constant of Positive Training Effect of 36 days
#'       \item Time Constant of Negative Training Effect of 21 days
#'       \item Sums of Square Error equal to 20.25 W
#'       \item R Squared (Variance explained) of 0.979
#'     }
#' @format
#'   Data frame with 3 variables and 165 observations:
#'   \describe{
#'    \item{Day}{Integer vector}
#'    \item{BikeScore}{BikeScore training load metric in arbitrary units}
#'    \item{Test_5min_Power}{Results of occasional 5min Power Test in Watts}
#' }
#' @usage data(bike_score)
#' @source
#' Clarke DC, Skiba PF. 2013. Rationale and resources for teaching the mathematical modeling of athletic
#'     training and performance. Advances in Physiology Education 37:134–152. DOI: 10.1152/advan.00078.2011.
"bike_score"
