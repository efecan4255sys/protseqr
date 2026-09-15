#' Sliding Window Hydrophobicity Profile
#'
#' Calculates the Kyte & Doolittle hydrophobicity profile of a protein sequence using a sliding window.
#'
#' @param sequence A character string of amino acid single-letter codes.
#' @param window_size An odd positive integer specifying the window length (default is 7).
#'
#' @return A numeric vector representing the mean hydrophobicity score for each window position.
#' @export
#'
#' @examples
#' calc_hydro_profile("MKVLWAACDEFGHIKLMNPQRSTVWY", window_size = 5)
calc_hydro_profile <- function(sequence, window_size = 7) {
  if (!is.character(sequence) || length(sequence) != 1) {
    stop("Input 'sequence' must be a single string.")
  }
  if (!is.numeric(window_size) || window_size <= 0 || window_size %% 2 == 0) {
    stop("'window_size' must be a positive odd integer.")
  }

  seq_clean <- toupper(sequence)
  aa_chars <- strsplit(seq_clean, "")[[1]]
  n <- length(aa_chars)

  if (n < window_size) {
    stop("Sequence length must be greater than or equal to window_size.")
  }

  kd_scale <- c(
    "A" = 1.8,  "R" = -4.5, "N" = -3.5, "D" = -3.5, "C" = 2.5,
    "Q" = -3.5, "E" = -3.5, "G" = -0.4, "H" = -3.2, "I" = 4.5,
    "L" = 3.8,  "K" = -3.9, "M" = 1.9,  "F" = 2.8,  "P" = -1.6,
    "S" = -0.8, "T" = -0.7, "W" = -0.9, "Y" = -1.3, "V" = 4.2
  )

  if (!all(aa_chars %in% names(kd_scale))) {
    stop("Sequence contains invalid amino acid characters.")
  }

  values <- kd_scale[aa_chars]
  num_windows <- n - window_size + 1
  scores <- numeric(num_windows)

  for (i in seq_len(num_windows)) {
    scores[i] <- mean(values[i:(i + window_size - 1)])
  }

  return(round(scores, 3))
}
