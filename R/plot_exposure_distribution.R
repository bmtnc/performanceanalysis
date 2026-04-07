#' Plot factor exposure distribution
#'
#' Box plot of rolling factor betas with the latest observation highlighted
#' as a gold diamond.
#'
#' @param exposure_data Data frame with a date column and one column per factor
#'   containing rolling beta estimates
#' @param factor_cols Character vector of factor column names in exposure_data
#' @param target_ticker Ticker symbol for plot title
#' @param roll_window Rolling window length for subtitle annotation
#'
#' @return A ggplot object
#' @export
plot_exposure_distribution <- function(exposure_data, factor_cols,
                                       target_ticker, roll_window) {
  meta <- factor_metadata(factor_cols)

  viz <- exposure_data %>%
    dplyr::select(date, dplyr::all_of(factor_cols)) %>%
    tidyr::pivot_longer(-date, names_to = "factor", values_to = "beta") %>%
    dplyr::mutate(
      factor_label = factor(meta$labels[factor], levels = meta$order)
    )

  latest_betas <- viz %>%
    dplyr::group_by(factor_label) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup()

  latest_date <- format(max(exposure_data$date), "%b %Y")

  viz %>%
    ggplot2::ggplot(ggplot2::aes(x = factor_label, y = beta)) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    ggplot2::geom_boxplot(
      ggplot2::aes(fill = factor_label),
      alpha = 0.6,
      outlier.size = 0.8,
      outlier.alpha = 0.4,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = latest_betas,
      color = "#d4a017",
      fill = "#d4a017",
      shape = 23,
      size = 3.5,
      stroke = 0.8
    ) +
    ggplot2::scale_fill_manual(values = meta$palette) +
    ggplot2::labs(
      title = paste0("Factor Exposure Distribution: ", target_ticker),
      subtitle = paste0(
        "Rolling ",
        roll_window,
        "-month betas | ",
        "Gold diamond = latest (",
        latest_date,
        ")"
      ),
      x = "",
      y = "Factor Exposure (Beta)",
      caption = "Data: FRED, AQR, Alpha Vantage"
    ) +
    factor_analysis_theme(rotate_x = TRUE) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 11))
}
