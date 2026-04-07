#' Factor display metadata
#'
#' Returns display labels, color palette, and ordering for cross-asset factor
#' columns used in hedge fund factor analysis.
#'
#' @param factor_cols Character vector of factor column names (e.g. "fi_carry",
#'   "eq_hml"). Only columns present in the internal lookup are included.
#'
#' @return A list with components:
#'   - labels: Named character vector mapping factor_cols to display names
#'   - palette: Named character vector mapping display names to hex colors
#'   - order: Character vector of display names in canonical display order
#' @export
factor_metadata <- function(factor_cols) {
  all_labels <- c(
    fi_carry = "FI Carry",
    fi_value = "FI Value",
    fi_mom = "FI Momentum",
    fi_def = "FI Defensive",
    fx_carry = "FX Carry",
    fx_mom = "FX Momentum",
    fx_value = "FX Value",
    eq_hml = "EQ Value",
    eq_bab = "EQ Low Beta",
    eq_qmj = "EQ Quality",
    eq_mkt = "EQ Market",
    eq_smb = "EQ Size",
    eq_mom = "EQ Momentum"
  )

  all_palette <- c(
    "FI Carry" = "#08519c",
    "FI Value" = "#2171b5",
    "FI Momentum" = "#4292c6",
    "FI Defensive" = "#6baed6",
    "FX Carry" = "#006d2c",
    "FX Momentum" = "#238b45",
    "FX Value" = "#41ab5d",
    "EQ Value" = "#a50f15",
    "EQ Low Beta" = "#cb181d",
    "EQ Quality" = "#ef3b2c",
    "EQ Market" = "#d4a017",
    "EQ Size" = "#fb6a4a",
    "EQ Momentum" = "#fc9272"
  )

  labels <- all_labels[intersect(factor_cols, names(all_labels))]
  order <- unname(labels)
  palette <- all_palette[order]

  list(labels = labels, palette = palette, order = order)
}
