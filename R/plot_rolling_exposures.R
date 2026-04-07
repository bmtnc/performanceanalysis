#' Plot rolling factor exposures
#'
#' Stacked bar chart showing time-varying factor exposures from a rolling
#' regression. Positive and negative exposures stack above and below zero.
#'
#' @param exposure_data Data frame with a date column and one column per factor
#'   containing rolling beta estimates. Typically the output of
#'   roll_constrained_lm with dates attached.
#' @param factor_cols Character vector of factor column names in exposure_data
#' @param target_ticker Ticker symbol for plot title
#' @param roll_window Rolling window length for subtitle annotation
#'
#' @return A ggplot object
#' @export
plot_rolling_exposures <- function(exposure_data, factor_cols, target_ticker,
                                   roll_window) {
  meta <- factor_metadata(factor_cols)

  viz <- exposure_data %>%
    dplyr::select(date, dplyr::all_of(factor_cols)) %>%
    tidyr::pivot_longer(-date, names_to = "factor", values_to = "exposure") %>%
    dplyr::mutate(
      factor_label = factor(meta$labels[factor], levels = rev(meta$order))
    )

  viz %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = exposure, fill = factor_label)) +
    ggplot2::geom_col(
      ggplot2::aes(color = factor_label),
      width = 30,
      alpha = 0.8,
      linewidth = 0.25
    ) +
    ggplot2::scale_color_manual(values = meta$palette, guide = "none") +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = c(0.02, 0)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::number_format(accuracy = 0.1)
    ) +
    ggplot2::scale_fill_manual(
      values = meta$palette,
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::labs(
      title = paste0("Rolling Factor Exposures: ", target_ticker),
      subtitle = paste0(
        "Unconstrained regression -- ",
        length(factor_cols),
        "-factor model, ",
        roll_window,
        "-month window"
      ),
      x = "",
      y = "Factor Exposure (Beta)",
      caption = "Data: FRED, AQR, Alpha Vantage"
    ) +
    factor_analysis_theme(legend_bottom = TRUE)
}
