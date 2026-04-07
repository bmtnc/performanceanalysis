#' Factor analysis ggplot2 theme
#'
#' Minimal theme for cross-asset factor analysis charts. Provides consistent
#' styling for titles, grid lines, and captions across all factor analysis
#' visualizations.
#'
#' @param base_size Base font size passed to ggplot2::theme_minimal
#' @param legend_bottom If TRUE, places legend at the bottom with no title
#' @param rotate_x If TRUE, rotates x-axis labels 45 degrees
#'
#' @return A ggplot2 theme object
#' @export
factor_analysis_theme <- function(base_size = 12, legend_bottom = FALSE,
                                  rotate_x = FALSE) {
  t <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey80"),
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 12),
      plot.caption = ggplot2::element_text(size = 8, color = "grey40")
    )
  if (legend_bottom) {
    t <- t + ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    )
  }
  if (rotate_x) {
    t <- t + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9)
    )
  }
  t
}
