subset_data <- function(df, genera) {
  sub_df <- df %>%
    filter(!Genus %in% genera)
  sub_df
}
