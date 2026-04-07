#' Plot performance cone
#'
#' Visualizes a cone of expected performance paths with actual fund returns
#' overlaid. Expects the enriched data frame from calculate_performance_cone
#' containing cone bands, cumulative_return, and assumption attributes.
#'
#' @param cone_data Data frame from calculate_performance_cone
#' @param target_ticker Ticker symbol for plot title
#'
#' @return A ggplot object
#' @export
plot_performance_cone <- function(cone_data, target_ticker = NULL) {
  sigma_cols <- grep("^upper_", names(cone_data), value = TRUE)
  n_sigma <- length(sigma_cols)
  has_actual <- "cumulative_return" %in% names(cone_data)

  # Read assumptions from attributes
  sr <- attr(cone_data, "sharpe_ratio")
  vol <- attr(cone_data, "volatility")
  mu_g <- attr(cone_data, "mu_geometric")

  band_color <- "#08519c"
  band_alphas <- seq(0.35, 0.12, length.out = n_sigma)

  p <- ggplot2::ggplot(cone_data, ggplot2::aes(x = date))

  # Ribbons from widest to narrowest so narrower bands paint on top
  for (k in rev(seq_len(n_sigma))) {
    upper_sym <- rlang::sym(paste0("upper_", k))
    lower_sym <- rlang::sym(paste0("lower_", k))
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = !!lower_sym, ymax = !!upper_sym),
      fill = band_color,
      alpha = band_alphas[k]
    )
  }

  # Center line (geometric growth rate)
  p <- p + ggplot2::geom_line(
    ggplot2::aes(y = center),
    color = band_color,
    linewidth = 0.8,
    linetype = "dashed"
  )

  # Overlay actual returns if present
  if (has_actual) {
    last_row <- cone_data %>% dplyr::slice_tail(n = 1)
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = cumulative_return),
        color = "black",
        linewidth = 0.7
      ) +
      ggplot2::geom_point(
        data = last_row,
        ggplot2::aes(y = cumulative_return),
        color = "black",
        size = 2.5
      ) +
      ggplot2::geom_text(
        data = last_row,
        ggplot2::aes(
          y = cumulative_return,
          label = scales::percent(cumulative_return, accuracy = 0.1)
        ),
        color = "black",
        size = 3.5,
        hjust = -0.15
      )
  }

  # Build title and subtitle from attributes
  title <- "Performance Cone"
  if (!is.null(target_ticker)) title <- paste0(title, ": ", target_ticker)

  subtitle_parts <- character(0)
  if (!is.null(sr)) {
    subtitle_parts <- c(subtitle_parts, paste0("SR = ", sr))
  }
  if (!is.null(vol)) {
    subtitle_parts <- c(
      subtitle_parts,
      paste0("Vol = ", scales::percent(vol, accuracy = 0.1))
    )
  }
  if (!is.null(mu_g)) {
    subtitle_parts <- c(
      subtitle_parts,
      paste0("Geom. growth = ", scales::percent(mu_g, accuracy = 0.01))
    )
  }
  subtitle <- paste(subtitle_parts, collapse = " | ")

  p +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = ggplot2::expansion(mult = c(0.02, 0.10))
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "",
      y = "Cumulative Return",
      caption = "Bands: 1/2/3 std. dev. | Center: geometric growth rate | Data: Alpha Vantage"
    ) +
    factor_analysis_theme()
}
