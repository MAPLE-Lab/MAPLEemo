#' Translate filenames into Emotional Piano Project UID
#'
#' Translates a vector of filenames into an Emotional Piano Project UID
#' containing a setCode, pieceID, and albumID. The function parses the provided
#' filenames, which is expected to follow a specific naming convention, and
#' extracts the unique identifiers.
#'
#' @param filename The filename to be translated. Expected format uses at least
#' 5 underscores as delimiter, and must include the Composer, Set Number,
#' Performer, Chroma, Mode, and albumID. Non-conventional files can include
#' 'Book' and Piece Number in the place of Chroma and Mode respectively. Can
#' include a track number (from full track files) at the beggining, and any
#' number of additional delimiters at the end (for instance, from manipulated
#' files). Can include the file extension. Note that Chroma may sometimes
#' include letters in case of multiple same pieceID in same set.
#'
#' @return A tibble with 3 columns:
#' \describe{
#'   \item{setCode}{(character) Unique identifier for the 'set' or work
#'   (e.g., Chopin Op.28)}
#'   \item{pieceID}{(character) Unique identifier for the specific piece. May
#'   include an alternate letter.}
#'   \item{albumID}{(character) Unique identifier for the album.}
#' }
#'
#' @examples
#' # Typical usage with single filename: returns a tibble.
#' translate_filename("Bach_1_Newman_11_minor_bachNewman2001piano.wav")
#' translate_filename("Debussy_1_Arrau_1_4_debussyArrau1991.wav")
#' translate_filename("Alkan_1_Martin_0a_Major_alkanMartin2006.wav")
#'
#' # Less common usage: extract specific elements of UID indepedendantly.
#' translate_filename("Alkan_1_Martin_0a_Major_alkanMartin2006.wav")$setCode
#' translate_filename("Alkan_1_Martin_0a_Major_alkanMartin2006.wav")$pieceID
#' translate_filename("Alkan_1_Martin_0a_Major_alkanMartin2006.wav")$albumID
#'
#' # Mutating the UIDs from filenames in a tibble.
#' tibble::tibble(
#'   filename = c(
#'     "Bach_1_Newman_11_minor_bachNewman2001piano.wav",
#'     "Debussy_1_Arrau_1_4_debussyArrau1991.wav",
#'     "Alkan_1_Martin_0a_Major_alkanMartin2006.wav"
#'   )
#' ) |>
#'   dplyr::mutate(translate_filename(filename))
#'
#' @export
translate_filename <- function(filenames) {
  # Get just the filename with no extension.
  filename_no_ext <- tools::file_path_sans_ext(basename(filenames))
  # Split into components using underscore.
  parts_list <- strsplit(filename_no_ext, "_")
  
  rows <- lapply(parts_list, function(delims) {
    # Only works if there's enough underscores.
    if (length(delims) < 5) {
      return(tibble(setCode = NA_character_, pieceID = NA_character_, albumID = NA_character_))
    }
    
    # Remove track number if included
    if (nchar(delims[1]) == 2 && grepl("^\\d+$", delims[1])) {
      delims <- delims[-1]
    }
    
    # Double check number of parts after removing track number.
    if (length(delims) < 5) {
      return(tibble(setCode = NA_character_, pieceID = NA_character_, albumID = NA_character_))
    }

    # Translate parts into UIDs.
    composer <- tolower(delims[1])
    set_num <- delims[2]
    key <- delims[4]
    mode <- tolower(delims[5])
    albumID <- if (length(delims) >= 6) delims[6] else NA_character_
    
    setCode <- paste0(composer, "-", set_num)
    pieceID <- if (mode == "major") {
      paste0("M", key)
    } else if (mode == "minor") {
      paste0("m", key)
    } else {
      paste0(key, "-", mode)
    }
    
    tibble::tibble(setCode = setCode, pieceID = pieceID, albumID = albumID)
  })
  
  dplyr::bind_rows(rows)
}

# =========================================================================== #
