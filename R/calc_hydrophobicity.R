#' Calculate Gravy (Hydrophobicity) Index of a Protein Sequence
#'
#' Bu fonksiyon verilen bir amino asit dizisinin Kyte-Doolittle hidrofobiklik
#' indeksine göre ortalama hidrofobikliğini (GRAVY skoru) hesaplar.
#'
#' @param sequence Karakter vektörü. Tek harfli amino asit dizisi (örn: "MKVLWA").
#'
#' @return Sayısal bir değer (GRAVY skoru). Pozitif değerler hidrofobik,
#'   negatif değerler hidrofilik karakteri gösterir.
#' @export
#'
#' @examples
#' calc_hydrophobicity("MKVLWA")
calc_hydrophobicity <- function(sequence) {
  # 1. Girdi Kontrolleri (Sanity Checks)
  if (!is.character(sequence) || length(sequence) != 1) {
    stop("Hata: Girdi tek bir karakter dizisi (string) olmalıdır.")
  }

  # Diziyi büyük harfe çevir ve harflerine ayır
  clean_seq <- toupper(trimws(sequence))
  aa_vector <- unlist(strsplit(clean_seq, ""))

  if (length(aa_vector) == 0) {
    stop("Hata: Dizi boş olamaz.")
  }

  # 2. Kyte-Doolittle Skalası
  kd_scale <- c(
    A =  1.8, R = -4.5, N = -3.5, D = -3.5, C =  2.5,
    Q = -3.5, E = -3.5, G = -0.4, H = -3.2, I =  4.5,
    L =  3.8, K = -3.9, M =  1.9, F =  2.8, P = -1.6,
    S = -0.8, T = -0.7, W = -0.9, Y = -1.3, V =  4.2
  )

  # Geçersiz amino asit kontrolü
  invalid_aa <- setdiff(aa_vector, names(kd_scale))
  if (length(invalid_aa) > 0) {
    stop(paste("Geçersiz amino asit kodu tespit edildi:", paste(invalid_aa, collapse = ", ")))
  }

  # 3. Hesaplama: Ortalama Hidrofobiklik (GRAVY)
  scores <- kd_scale[aa_vector]
  gravy_score <- mean(scores)

  return(round(gravy_score, 3))
}
