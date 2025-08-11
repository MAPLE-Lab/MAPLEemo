# =========================================================================== #
#' @name df_emosample
#'
#' @title A Three-Experiment Sample of MAPLE Lab's Emotional Piano Project Data
#'
#' @description Sample of valence and arousal ratings from three experiments
#' (three sets of 24 piece preludes) corresponding to published papers:
#' Bach Well-Tempered Clavier, BWV 846-69, Book 1, Preludes (Battcock & Schutz,
#' 2019; 2021; Anderson & Schutz, 2021; Delle Grazie et al., in press)
#' Chopin Preludes Op. 28 (Anderson & Schutz, 2021; Delle Grazie et al., under
#' review)
#' Shostkovich Preludes Op. 34 (Delle Grazie et al., in press)
#' Also includes piece-level metadata described below.
#'
#' @format A data frame with 2280 rows and 14 variables:
#' \describe{
#'   \item{setCode}{(character) Unique identifier for the specific set of 24
#'         piece preludes; begins with composer last name in lowercase.}
#'   \item{albumID}{(character) Unique identifier for the specific album of the
#'         performance recording.}
#'   \item{pieceID}{(character) Unique identifier for the specific piece.
#'         Typically notated as M for major or m for minor followed by a number
#'         corresponding to the chroma (the numerical representation of the key;
#'         C = 0, C# = 1, etc).}
#'   \item{mode}{(character) The notated modality of the piece; Major or minor.}
#'   \item{expID}{(character) The nominal ID of the experiment. Either 6, 101,
#'         or 135 in this sample dataframe.}
#'   \item{participant}{(character) Participant identifier.}
#'   \item{valence}{(numeric) Participant’s valence rating along a 7-point
#'         Likert scale.}
#'   \item{arousal}{(numeric) Participant’s arousal rating along a 100-point
#'         scale.}
#'   \item{attacks}{(numeric) Total number of attacks (note onsets).}
#'   \item{arScore}{(numeric) Attack rate calculated based on score notation.}
#'   \item{arPerf}{(numeric) Average performance attack rate of the excerpt in
#'         attacks per second.}
#'   \item{pitchHeight}{(numeric) The average pitch height of the excerpt.}
#'   \item{duration}{(numeric) Total duration of the excerpt in seconds.}
#'   \item{rms}{(numeric) Average amplitude (measure of loudness) of the piece.}
#' }
#' @source
"df_emosample"

# =========================================================================== #
