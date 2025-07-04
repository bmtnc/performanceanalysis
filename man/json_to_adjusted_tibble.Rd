% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/json_to_adjusted_tibble.R
\name{json_to_adjusted_tibble}
\alias{json_to_adjusted_tibble}
\title{Convert Alpha Vantage JSON to Tidy Tibble (Adjusted Close Only)}
\usage{
json_to_adjusted_tibble(json_data, symbol)
}
\arguments{
\item{json_data}{Parsed JSON list from \code{fetch_symbol_json()}.}

\item{symbol}{Character scalar ticker.}
}
\value{
Tibble with columns \code{date}, \code{ticker}, \code{adjusted_close}, or \code{NULL}.
}
\description{
Convert Alpha Vantage JSON to Tidy Tibble (Adjusted Close Only)
}
