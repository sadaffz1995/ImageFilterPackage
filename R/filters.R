#' Grayscale filter with a positive message
#' @param r Red hex value
#' @param g Green hex value
#' @param b Blue hex value
#' @return A raw RGB triplet
#' @export
grayscale_filter <- function(r, g, b) {
  # ✨ Fun surprise message - only show once per session
  if (!exists(".grayscale_message_shown", envir = .GlobalEnv)) {
    assign(".grayscale_message_shown", TRUE, envir = .GlobalEnv)

    cat(crayon::cyan$bold("╔════════════════════════════════════════════╗\n"))
    cat(crayon::magenta$bold("║  Even though your filter is grayscale...   ║\n"))
    cat(crayon::green$bold(  "║         life still has colors 🌈         ║\n"))
    cat(crayon::cyan$bold(   "╚════════════════════════════════════════════╝\n"))
  }

  r_val <- as.integer(r)
  g_val <- as.integer(g)
  b_val <- as.integer(b)
  grey_val <- round(0.299 * r_val + 0.587 * g_val + 0.114 * b_val)
  rep(as.raw(grey_val), 3)
}


#' Red filter
#' @export
red_filter <- function(r, g, b) {
  c(hex_to_raw(r), as.raw(0), as.raw(0))
}

#' Green filter
#' @export
green_filter <- function(r, g, b) {
  c(as.raw(0), hex_to_raw(g), as.raw(0))
}

#' Blue filter
#' @export
blue_filter <- function(r, g, b) {
  c(as.raw(0), as.raw(0), hex_to_raw(b))
}

#' Magenta binary filter
#' @param r,g,b RGB hex values
#' @param cutoff Brightness cutoff
#' @export
magenta_filter <- function(r, g, b, cutoff = 127) {
  grey <- round(0.299 * strtoi(r, 16) + 0.587 * strtoi(g, 16) + 0.114 * strtoi(b, 16))
  if (grey > cutoff) c(as.raw(255), as.raw(0), as.raw(255)) else rep(as.raw(0), 3)
}

#' Sunset Glow filter
#' @export
sunset_glow_filter <- function(rgb_vec) {
  r <- as.integer(rgb_vec[1])
  g <- as.integer(rgb_vec[2])
  b <- as.integer(rgb_vec[3])
  c(as.raw(min(255, r + 40)), as.raw(round(g * 0.85)), as.raw(round(b * 0.8)))
}
