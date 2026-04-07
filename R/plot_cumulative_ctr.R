#' Plot cumulative contribution to return
#'
#' Stacked bar chart of cumulative factor contributions with a total return
#' line overlay. Alpha and residual are combined into an "Idiosyncratic"
#' component.
#'
#' @param cumulative_ctr Data frame from calculate_cumulative_ctr containing
#'   cumulative_fund_return, cumulative_alpha_ctr, cumulative_residual, and
#'   cumulative_{factor}_ctr columns
#' @param factor_cols Character vector of factor column names (without _ctr
#'   suffix)
#' @param target_ticker Ticker symbol for plot title
#' @param roll_window Rolling window length for subtitle annotation
#'
#' @return A ggplot object
#' @export
plot_cumulative_ctr <- function(cumulative_ctr, factor_cols, target_ticker,
                                roll_window) {
  meta <- factor_metadata(factor_cols)
  ctr_col_names <- paste0(factor_cols, "_ctr")

  factor_ctr_labels <- setNames(unname(meta$labels), ctr_col_names)
  ctr_labels <- c(idiosyncratic = "Idiosyncratic", factor_ctr_labels)
  ctr_palette <- c("Idiosyncratic" = "Black", meta$palette)
  ctr_order <- unname(ctr_labels)

  viz <- cumulative_ctr %>%
    dplyr::mutate(
      cumulative_idiosyncratic = cumulative_alpha_ctr + cumulative_residual
    ) %>%
    dplyr::select(
      date,
      cumulative_idiosyncratic,
      dplyr::all_of(paste0("cumulative_", ctr_col_names))
    ) %>%
    tidyr::pivot_longer(
      -date, names_to = "component", values_to = "value"
    ) %>%
    dplyr::mutate(
      component_name = gsub("^cumulative_", "", component),
      label = factor(ctr_labels[component_name], levels = rev(ctr_order))
    )

  viz %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = value, fill = label)) +
    ggplot2::geom_col(
      ggplot2::aes(color = label),
      width = 30,
      alpha = 0.8,
      linewidth = 0.25
    ) +
    ggplot2::scale_color_manual(values = ctr_palette, guide = "none") +
    ggplot2::geom_line(
      data = cumulative_ctr,
      ggplot2::aes(x = date, y = cumulative_fund_return, fill = NULL),
      color = "black",
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = cumulative_ctr %>% dplyr::slice_tail(n = 1),
      ggplot2::aes(x = date, y = cumulative_fund_return, fill = NULL),
      color = "black",
      size = 2.5,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = cumulative_ctr %>% dplyr::slice_tail(n = 1),
      ggplot2::aes(
        x = date,
        y = cumulative_fund_return,
        fill = NULL,
        label = scales::percent(cumulative_fund_return, accuracy = 0.1)
      ),
      color = "black",
      size = 3.5,
      hjust = -0.15,
      show.legend = FALSE
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = ggplot2::expansion(mult = c(0.02, 0.07))
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(
      values = ctr_palette,
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::labs(
      title = paste0("Cumulative Contribution to Return: ", target_ticker),
      subtitle = paste0(
        "Out-of-sample decomposition (lagged betas) -- ",
        length(factor_cols),
        "-factor model, ",
        roll_window,
        "-month window"
      ),
      x = "",
      y = "Cumulative Return",
      caption = paste0(
        "Black line = Total cumulative return | Data: FRED, AQR, Alpha Vantage"
      )
    ) +
    factor_analysis_theme(legend_bottom = TRUE)
}
