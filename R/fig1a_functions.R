#' Subset data
#'
#' \code{subset_data} drops observations according to their genera.
#'
#' @param df A data frame, data frame extension (e.g. a tibble), or a lazy
#'   data frame (e.g. from dbplyr or dtplyr). Requires column `Genus`.
#' @param genera A character vector that corresponds to the genera to be
#'   dropped.
#' @return An object of the same type as `df`. Properties of the output:
#'   - Retained rows appear in the same order as in the input
#'   - Columns and data frame attributes are preserved
#' @examples
#' subset_data(data, c("Elephantulus", "Orcinus"))
#'
#' #' \dontrun{
#' subset_data("a")
#' }
subset_data <- function(df, genera) {
  sub_df <- df %>%
    filter(!Genus %in% genera)
  sub_df
}
