#' Calculate Molecular Weight of a Protein Sequence
#'
#' This function calculates the molecular weight (in Daltons) of a given
#' amino acid sequence based on average isotopic weights, taking into account
#' the loss of water molecules during peptide bond formation.
#'
#' @param sequence A character string representing the protein sequence (single-letter codes).
#' @return A numeric value representing the molecular weight in Daltons (Da).
#' @export
#' @examples
#' calc_mw("ACDEFGHIKLMNPQRSTVWY")
calc_mw <- function(sequence) {
  if (!is.character(sequence) || length(sequence) != 1) {
    stop("Input must be a single character string.")
  }

  seq_clean <- toupper(trimws(sequence))

  if (nchar(seq_clean) == 0) {
    stop("Sequence cannot be empty.")
  }

  mw_weights <- c(
    A = 71.08,  R = 156.20, N = 114.11, D = 115.09, C = 103.14,
    E = 129.12, Q = 128.14, G = 57.05,  H = 137.14, I = 113.17,
    L = 113.17, K = 128.18, M = 131.21, F = 147.18, P = 97.12,
    S = 87.08,  T = 101.11, W = 186.21, Y = 163.18, V = 99.13
  )

  chars <- unlist(strsplit(seq_clean, split = ""))

  invalid_chars <- setdiff(chars, names(mw_weights))
  if (length(invalid_chars) > 0) {
    stop(paste("Invalid amino acid characters found:", paste(invalid_chars, collapse = ", ")))
  }

  total_mw <- sum(mw_weights[chars]) + 18.015
  return(round(total_mw, 2))
}
