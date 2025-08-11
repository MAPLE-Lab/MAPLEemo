# ============================================================================ #
#' Generate Grouped Histogram Dataframe
#'
#' Add columns to a dataframe that allows building a discrete histogram. Each
#' row will recieve a bin, bin center value, and y position. While the bin
#' center values directly correspond to a value in the "val" unit, the y axis
#' represents a stacking of all rows in that bin. This is designed for creating
#' discrete histograms where each observation is a point, rather than part of
#' a distribution.
#'
#' @param data A data.frame, data.table, or tibble.
#' @param val_col Column name containing the value to be histogrammed, as a string.
#' @param group_cols Column names containing any groups if faceting, as a string.
#' @param breaks Equation or type of break to be passed to hist().
#' @param y_margin y axis margin for space between points.
#' @param y_margin x axis margin for space between points.
#'
#' @return
#'
#' @examples
#' library(ggplot2)
#' mtcars |> discrete_histogram(
#'   val_col = "mpg"
#' ) |>
#'   ggplot(
#'     aes(
#'       x = x_mid,
#'       y = y,
#'       label = rownames(mtcars),
#'       xmin = xmin,
#'       xmax = xmax,
#'       ymin = ymin,
#'       ymax = ymax,
#'       fill = cyl
#'     )
#'   ) +
#'   geom_vline(xintercept = 21) +
#'  geom_rect() +
#'   geom_text(
#'     size   = 3.5,
#'     colour = "white"
#'   )
#'
#' @export
discrete_histogram <- function(
  data,
  val_col,
  group_cols = NULL,
  breaks = "fd",
  y_margin = 2,
  x_margin = 2
) {
  df_in <- data

  # Single‑point edge case.
  if (nrow(df_in) == 1) {
    v <- df_in[[val_col]]
    breaks <- c(v - 1, v, v + 1)
  }

  # Compute histogram (y counts + x mid points).
  h <- hist(
    df_in[[val_col]],
    plot = FALSE,
    breaks = breaks
  )
  if (length(h$counts) == 0) {
    h$counts <- 1
    h$mids   <- df_in[[val_col]]
    h$breaks <- c(
      df_in[[val_col]] - .5,
      df_in[[val_col]] + .5
    )
  }

  width <- diff(h$breaks)[1]

  # Assign bins, stack within group+bin.
  df_hist <- df_in |>
    dplyr::mutate(
      x_val = .data[[val_col]],
      x_bin = cut(
        x_val,
        breaks = h$breaks,
        labels = FALSE,
        include.lowest = TRUE
      ),
      x_mid = h$mids[x_bin]
    ) |>
    dplyr::arrange(x_bin) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "x_bin")))) |>
    dplyr::mutate(y = dplyr::row_number()) |>
    dplyr::ungroup()

  # Compute rectangle limits.
  df_hist |>
    dplyr::mutate(
      xmin = x_mid - (width - width * x_margin / 100) / 2,
      xmax = x_mid + (width - width * x_margin / 100) / 2,
      ymin = y - (1 - 1 * y_margin/  100) / 2,
      ymax = y + (1 - 1 * y_margin / 100) / 2
    )
}

# ============================================================================ #
