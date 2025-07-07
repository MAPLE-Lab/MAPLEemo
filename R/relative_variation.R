# ============================================================================ #
#' Calculate Relative Variation
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Calculates the relative variation of subgroups (versions) within a larger
#' grouping by calculating the ratio of their variability to the total
#' variability of the group. This is useful for assessing the consistency of
#' variation within different subgroups compared to the
#' overall group variation.
#'
#' @param df Dataframe containing the data to be analyzed
#' @param grouping_col Column name in `df` that defines the groups for which
#' variation is calculated. Each group consists of multiple versions (subgroups)
#' @param value_col Column name in `df` that contains the numerical values
#' whose variability is being measured
#' @param variability_metric Function to compute variability. Defaults to `sd`
#' (standard deviation), but other functions (e.g., `var`) can be used to
#' customize the measure of variation
#'
#' @return Dataframe containing the computed relative variation ratio for
#' each subgroup (`version`) within specified grouping. The output includes:
#' - `grouping_col`: The grouping identifier
#' - `var_version`: The variability (e.g., standard deviation) of the subgroup
#' - `var_total`: The total variability of the entire group
#' - `ratio`: The computed ratio of subgroup variation to total variation
#'
#' @examples
#' # Typical usage.
#' relative_variation(
#'   df = mtcars,
#'   grouping_col = cyl,
#'   value_col = mpg
#' )
#'
#' # Use with dplyr:group_by().
#' library(dplyr)
#' mtcars |>
#' group_by(gear) |>
#' relative_variation(
#'   grouping_col = cyl,
#'   value_col = mpg
#' )
#'
#' @export
# TODO: Is grouping call a good name?
relative_variation <- function(
    df,
    grouping_col,
    value_col,
    variability_metric = sd
) {
  # Check for existing groups.
  existing_groups <- dplyr::group_vars(df)

  # Calculate version-wise variation
  df_version <- df |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(existing_groups)),
      {{ grouping_col }}
    ) |>
    dplyr::summarize(
      var_version = variability_metric(
        {{ value_col }},
        na.rm = TRUE
      ),
      .groups = "drop_last"  # Drop only `grouping_col`, keep outer groups
    )

  # Compute total variation within each group
  df_total <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(existing_groups))) |>
    dplyr::summarize(
      var_total = variability_metric(
        {{ value_col }},
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  # Merge back and calculate variation ratio.
  if (length(existing_groups) > 0) {
    df_return <- df_version |>
      dplyr::left_join(
        df_total,
        by = existing_groups
      )
  } else {
    df_return <- df_version |>
      dplyr::cross_join(df_total)  # Explicit cross join when ungrouped
  }

  df_return <- df_return |>
    dplyr::mutate(ratio = var_version / var_total) |>
    dplyr::ungroup()

  return(df_return)
}

# ============================================================================ #
