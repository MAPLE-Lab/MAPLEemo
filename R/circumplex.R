# ============================================================================ #
#' Make circumplex-style scatter plot
#'
#' @description
#' `r lifecycle::badge("stable")`
#' Simple function to prepare a circumplex model based on user input.
#'
#' @param data Tibble or data.frame object
#' @param x Character. Column name for x variable. Should be passed as a string.
#' @param y Character. Column name for y variable. Should be passed as a string.
#' @param label Character. Labels to add to plot. Should be passed as a string.
#' @param group Character. Column of group to colour-code observations by.
#'   Should be passed as a string.
#' @param yintercept Numeric. y intercept defining placement of horizontal line,
#'   defaulted to y = 50 as the midpoint for arousal ratings.
#' @param xintercept Numeric. x intercept defining placement of vertical line,
#'   defaulted to x = 4 as the midpoint for valence ratings.
#' @param add_points Boolean. Option to add points marking observation; FALSE by
#'   default.
#' @param add_text Boolean. Option to add text based on label input; TRUE by
#'   default.
#' @param ... Additional arguments passed to geom_text.
#'
#' @return NULL
#'
#' @examples
#' # Typical usage.
#' df <- subset(df_emosample, expID == 101)
#'
#' df_circumplex <- df |>
#'   dplyr::group_by(pieceID) |>
#'   dplyr::mutate(
#'      valence_mean = mean(valence),
#'      arousal_mean = mean(arousal)
#'   ) |>
#'   dplyr::select(
#'     setCode,
#'     albumID,
#'     pieceID,
#'     mode,
#'     valence_mean,
#'     arousal_mean
#'   ) |>
#'   dplyr::distinct()
#'
#' plot_circumplex(
#'   df_circumplex,
#'   "valence_mean",
#'   "arousal_mean",
#'   "pieceID",
#'   "mode"
#' )
#'
#' # Points only, no text.
#' plot_circumplex(
#'   df_circumplex,
#'   "valence_mean",
#'   "arousal_mean",
#'   "pieceID",
#'   "mode",
#'   add_points = TRUE,
#'   add_text = FALSE
#' )
#'
#' # Points with text.
#' plot_circumplex(
#'   df_circumplex,
#'   "valence_mean",
#'   "arousal_mean",
#'   "pieceID",
#'   "mode",
#'   add_points = TRUE,
#'   color = "grey", # arg passed to geom_text
#'   position = ggplot2::position_jitter(width = 0.2)
#' )
#'
#' @export
plot_circumplex <- function(
  data,
  x,
  y,
  label,
  group,
  yintercept = 50,
  xintercept = 4,
  add_points = FALSE,
  add_text = TRUE,
  ...
) {
  # Create ggplot object.
  p <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data[[x]],
      y = .data[[y]],
      group = .data[[group]],
      label = .data[[label]],
      color = .data[[group]]
    )
  ) +

    # Add lines for circumplex boundaries.
    ggplot2::geom_hline(yintercept = yintercept) +
    ggplot2::geom_vline(xintercept = xintercept) +

    ggplot2::theme_classic() +

    # Remove Axis elements.
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  # Plotting.
  if (add_points == TRUE && add_text == FALSE) {
    p <- p + ggplot2::geom_point()
  } else if (add_points == FALSE && add_text == TRUE) {
    p <- p + ggplot2::geom_text(...)
  } else if (add_points == FALSE && add_text == FALSE) {
    p <- p
  } else {
    p <- p +
      ggplot2::geom_point() +
      ggplot2::geom_text(...)
  }

  # Return ggplot object.
  return(p)
}

# ============================================================================ #
#' Make paired circumplex-style scatter plot
#'
#' @description
#' `r lifecycle::badge("stable")`
#' Simple function to visualize paired circumplex data by drawing lines between
#' two matched observations. Useful for showing movement or differences across
#' two conditions (e.g., same pieces by different groups).
#'
#' @param data Tibble or data.frame object containing paired observations.
#' @param x Character. Column name for x variable. Should be passed as a string.
#' @param y Character. Column name for y variable. Should be passed as a string.
#' @param group Character. Column name denoting grouping variable with at least
#'   two levels. Should be passed as a string.
#' @param group_levels Character vector of length 2. Names of the two levels
#'   within `group` to compare.
#' @param label Character. Column name of labels for each matched observation
#'   (e.g., "pieceID").
#' @param id_by_shape Boolean. Option to distinguish group levels by shape in
#' the plot; FALSE by default.
#' @param id_by_text Boolean. Option to add text labels for both groups; TRUE by
#'   default.
#'
#' @return NULL.
#'
#' @examples
#' # Set up data.
#' dfs <- subset(
#'   df_empsample,
#'   expID %in% c(101, 135)
#' )
#' dfs <- dfs |>
#'   dplyr::group_by(expID, pieceID) |>
#'   dplyr::mutate(
#'     valence = mean(valence, na.rm = TRUE),
#'     arousal = mean(arousal, na.rm = TRUE)
#'   ) |>
#'   dplyr::select(expID, pieceID, valence, arousal) |>
#'   dplyr::distinct()
#'
#' #
#' plot_paired_scatterplot(
#'   data = dfs,
#'   x = "valence",
#'   y = "arousal",
#'   group = "expID",
#'   group_levels = c(101, 135),
#'   label = "pieceID"
#' )
#'
#' #
#' plot_paired_scatterplot(
#'   data = dfs,
#'   x = "valence",
#'   y = "arousal",
#'   group = "expID",
#'   group_levels = c(101, 135),
#'   label = "pieceID",
#'   id_by_text = TRUE
#' )
#'
#' #
#' plot_paired_scatterplot(
#'   data = dfs,
#'   x = "valence",
#'   y = "arousal",
#'   group = "expID",
#'   group_levels = c(101, 135),
#'   label = "pieceID",
#'   id_by_shape = TRUE
#' )
#'
#' @export
plot_paired_scatterplot <- function(
  data,
  x,
  y,
  group,
  group_levels,
  label,
  id_by_shape = FALSE,
  id_by_text = FALSE
) {
  # Format data.
  group_1 <- data[data[[group]] == group_levels[1], ]
  group_2 <- data[data[[group]] == group_levels[2], ]
  data <- rbind(group_1, group_2)

  # Create ggplot object.
  p <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data[[x]],
      y = .data[[y]],
      group = .data[[group]],
      label = .data[[label]],
      color = .data[[group]],
      shape = .data[[group]]
    )
  ) +

    # Draw line between grouped observations.
    ggplot2::geom_line(
      color = "grey",
      ggplot2::aes(
        group = .data[[label]]
      )
    )

  # Plotting
  if (
    id_by_shape == TRUE
  ) {
    p <- p +
      ggplot2::geom_point()
  } else if (
    id_by_text == TRUE
  ) {
    p <- p +
      ggplot2::geom_text()
  } else {
    p <- p +
      ggplot2::geom_text(data = group_1) +
      ggplot2::geom_point(data = group_2)
  }

  # Apply theme.
  p <- p +
    ggplot2::theme_classic()

  # Return ggplot object.
  return(p)
}
# ============================================================================ #
