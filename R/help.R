#' Convert a hex string to a raw byte
#' @param hex A 2-character hex string, e.g. "FF"
#' @return A raw value
#' @keywords internal
hex_to_raw <- function(hex) {
  as.raw(as.integer(strtoi(hex, base = 16)))
}
