#' Plot rolling alpha
#'
#' Bar chart of annualized rolling intercept (alpha) from factor regression.
#' Bars are colored blue (positive) or red (negative), with the latest value
#' labeled.
#'
#' @param exposure_data Data frame with date and alpha columns. Alpha is the
#'   monthly intercept from the factor regression.
#' @param target_ticker Ticker symbol for plot title
#' @param roll_window Rolling window length for subtitle annotation
#'
#' @return A ggplot object
#' @export
plot_rolling_alpha <- function(exposure_data, target_ticker, roll_window) {
  alpha_data <- exposure_data %>%
    dplyr::select(date, alpha) %>%
    dplyr::mutate(alpha_ann = alpha * 12)

  alpha_data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = alpha_ann)) +
    ggplot2::geom_col(
      ggplot2::aes(fill = alpha_ann > 0),
      width = 25,
      alpha = 0.8,
      show.legend = FALSE
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    ggplot2::geom_point(
      data = alpha_data %>% dplyr::slice_tail(n = 1),
      color = "black",
      size = 2
    ) +
    ggrepel::geom_text_repel(
      data = alpha_data %>% dplyr::slice_tail(n = 1),
      ggplot2::aes(label = scales::percent(alpha_ann, accuracy = 0.1)),
      nudge_x = 30,
      direction = "y",
      segment.color = NA,
      size = 3.5
    ) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#2171b5", "FALSE" = "#cb181d")
    ) +
    ggplot2::labs(
      title = paste0("Rolling Alpha: ", target_ticker),
      subtitle = paste0(
        "Annualized intercept from ",
        roll_window,
        "-month unconstrained regression"
      ),
      x = "",
      y = "Alpha (annualized)",
      caption = "Data: FRED, AQR, Alpha Vantage"
    ) +
    factor_analysis_theme()
}
