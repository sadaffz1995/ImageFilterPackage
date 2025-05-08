#' Convert hex string to raw byte
#' @param hex A hex string, e.g., "FF"
#' @return A raw byte
#' @keywords internal
hex_to_raw <- function(hex) {
  as.raw(as.integer(strtoi(hex, base = 16)))
}
