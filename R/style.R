# ============================================================================ #
#' Colour Code Nominal Mode
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Colours used in the Emotional Piano Project to distinguish Major and Minor
#' modes. Includes three different conventions for mode labels for flexibility.
#'
#' @name colours_mode
#' @docType data
#' @export
colours_mode <- c(
  "Major" = "#a00202",
  "major" = "#a00202",
  "M" = "#a00202",
  "minor" = "#799afd",
  "Minor" = "#799afd",
  "m" = "#799afd",
  "NA_grey" = "#6F766F"
)

# ============================================================================ #
#' Colour Code Cues Used in Commonality Analysis
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Colours used for cues in Commonality Analysis for Valence/Arousal. Chosen for
#' accessible contrast.
#'
#' @name colours_cues
#' @docType data
#' @export
colours_cues <- c(
  "Mode" = "#cde7ca",
  "mode" = "#cde7ca",
  "Attack Rate" = "#5f9dcc",
  "arPerf" = "#5f9dcc",
  "arScore" = "#5f9dcc",
  "Pitch Height" = "#3c2cab",
  "pitchHeight" = "#3c2cab",
  "Amplitude" = "#cea7b1",
  "rms" = "#cea7b1"
)

# ============================================================================ #
#' Colour Code Music Content Analysis Tools
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Colours used for popular tools analyzed by the MAPLE Lab. Based on logos, but
#' modified for accessible contrast.
#'
#' @name colours_tool
#' @docType data
#' @export
colours_tool <- c(
  "Essentia" = "#90253F",
  "Librosa" = "#FBBC86",
  "MIRtoolbox" = "#268234"
)

# ============================================================================ #
# ggplot Theme for MAPLE Lab
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Makes modifications to ggplot themeing specific to MAPLE Lab plots.
#' Specified through discussion among grad students and Dr. Schutz to insure
#' that plots are uniform across all facets of the Emotional Piano Project.
#' Provides both a shortcut to generating clean plots, but also reduces the back
#' and forth on makign publication ready plots. Note that ggplot can be modified
#' further by adding theme() after theme_maple().
#'
#' @examples
#' # Typical usage.
#' tibble(
#'   x = c(1, 2, 3),
#'   y = c(1, 2, 3)
#' ) %>%
#'   ggplot(
#'     aes(
#'       x = x,
#'       y = y
#'     )
#'   ) +
#'   geom_point() +
#'   theme_maple()
#' # Override some aspects.
#' library(ggplot2)
#' tibble(
#'   x = c(1, 2, 3),
#'   y = c(1, 2, 3)
#' ) %>%
#'   ggplot(
#'     aes(
#'       x = x,
#'       y = y
#'     )
#'   ) +
#'   geom_point() +
#'   theme_maple() +
#'   theme(panel.background = element_rect(fill = "pink"))
#' @export
theme_maple <- function() {
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    panel.border = ggplot2::element_rect(
      fill = NA,
      color = "black",
      size = 1
    ),
    strip.background = ggplot2::element_rect(
      fill = "white",
      color = "black",
      size = 1
    ),
    strip.text = ggplot2::element_text(
      color = "black"
    )
  )
}

# ============================================================================ #
# Superceded variables.
#' @export
mode_cols <- colours_mode
#' @export
tool_cols <- colours_tool
#' @export
cue_cols <- colours_cues

# ============================================================================ #