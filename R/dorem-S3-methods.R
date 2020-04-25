#' S3 method for extracting model coefficients
#' @param object Object of class \code{dorem}
#' @param ... Extra arguments
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
coef.dorem <- function(object, ...) {
  unlist(object$coefs)
}

#' S3 method for plotting model
#' @param x Object of class \code{dorem}
#' @param type Type of plot. Options are "pred", "coef", "perf", which "pred" being default
#' @param ... Extra arguments
#' @export
#' @examples
#' data("bike_score")
#'
#' banister_model <- dorem(
#'   Test_5min_Power ~ BikeScore,
#'   bike_score,
#'   method = "banister",
#'   control = dorem_control(
#'         cv_folds = 3,
#'         cv_repeats = 5,
#'         shuffle = TRUE
#'   )
#' )
#' plot(banister_model)
plot.dorem <- function(x, type = "pred", ...) {
  rlang::arg_match(type, c("pred", "coef", "perf"))

  gg <- list(NULL)

  if (type == "pred") {
    plot_df <- data.frame(
      x = seq(1, nrow(x$data$predictors)),
      outcome = x$data$outcome,
      predicted = x$data$predicted,
      fold = NA
    )

    gg <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = x)
    ) +
      ggplot2::geom_point(
        ggplot2::aes(y = outcome),
        size = 1,
        alpha = 0.8,
        color = "red",
        show.legend = TRUE
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = predicted),
        alpha = 0.8,
        show.legend = TRUE
      ) +
      ggplot2::xlab(NULL)

    # If there are cross-validation performed add
    # predictions on the graph
    if (!is.null(x$cross_validation)) {
      cv_plot_df <- x$cross_validation$data$testing
      cv_plot_df <- cv_plot_df %>%
        dplyr::group_by(fold) %>%
        dplyr::mutate(x = seq(1, dplyr::n())) %>%
        dplyr::group_by(x) %>%
        dplyr::summarise(
          y_min = min(predicted),
          y_max = max(predicted)
        )
      gg <- gg +
        ggplot2::geom_ribbon(
          data = cv_plot_df,
          ggplot2::aes(
            x = x,
            ymin = y_min,
            ymax = y_max
          ),
          alpha = 0.3,
          show.legend = TRUE
        )
    }

    # If there are shuffle performed
    if (!is.null(x$shuffle)) {
      shuffle_plot_df <- plot_df
      shuffle_plot_df$predicted <- x$shuffle$data$predicted

      gg <- gg +
        ggplot2::geom_line(
          data = shuffle_plot_df,
          ggplot2::aes(
            y = predicted
          ),
          alpha = 0.8,
          linetype = "dotted",
          show.legend = TRUE
        )
    }
  }

  if (type == "coef") {
    if (is.null(x$cross_validation)) {
      stop("Coef plot can only be created for cross-validated model", call. = FALSE)
    }

    gg <- ggplot2::ggplot(
      x$cross_validation$coefs,
      ggplot2::aes(y = value, x = factor(0))
    ) +
      ggplot2::geom_boxplot() +
      ggplot2::facet_wrap(~coefs, scales = "free_y") +
      ggplot2::theme(
        axis.line.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      ) +
      ggplot2::scale_x_discrete(breaks = NULL) +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL)
  }

  if (type == "perf") {
    if (is.null(x$cross_validation)) {
      stop("Perf plot can only be created for cross-validated model", call. = FALSE)
    }

    perf_plot_df <- rbind(
      data.frame(
        type = "Training",
        x$cross_validation$performance$folds$training
      ),
      data.frame(
        type = "Testing",
        x$cross_validation$performance$folds$testing
      )
    )

    perf_plot_df$type <- factor(perf_plot_df$type, levels = c("Training", "Testing"))
    gg <- ggplot2::ggplot(
      perf_plot_df,
      ggplot2::aes(y = value, x = type)
    ) +
      ggplot2::geom_boxplot() +
      ggplot2::facet_wrap(~metric, scales = "free_y") +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL)
  }

  gg
}
