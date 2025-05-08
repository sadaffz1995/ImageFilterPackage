grayscale_filter <- function(r, g, b) {
  # ✨ Fun surprise message - only show once per session
  if (!exists(".grayscale_message_shown", envir = .GlobalEnv)) {
    assign(".grayscale_message_shown", TRUE, envir = .GlobalEnv)

    cat(crayon::cyan$bold("╔════════════════════════════════════════════╗\n"))
    cat(crayon::magenta$bold("║  Even though your filter is grayscale,   ║\n"))
    cat(crayon::green$bold(  "║ I wish your life will be colorful 🌈      ║\n"))
    cat(crayon::cyan$bold(   "╚════════════════════════════════════════════╝\n"))
  }

  r_val <- as.integer(r)
  g_val <- as.integer(g)
  b_val <- as.integer(b)
  grey_val <- round(0.299 * r_val + 0.587 * g_val + 0.114 * b_val)
  rep(as.raw(grey_val), 3)
}


# Red filter
red_filter <- function(r, g, b) {
  raw_r <- hex_to_raw(r)
  c(raw_r, as.raw(0), as.raw(0))
}

# Green filter
green_filter <- function(r, g, b) {
  raw_g <- hex_to_raw(g)
  c(as.raw(0), raw_g, as.raw(0))
}

# Blue filter
blue_filter <- function(r, g, b) {
  raw_b <- hex_to_raw(b)
  c(as.raw(0), as.raw(0), raw_b)
}

# Binary black/white filter
binary_filter <- function(r, g, b, cutoff = 127) {
  r_val <- as.integer(strtoi(r, 16))
  g_val <- as.integer(strtoi(g, 16))
  b_val <- as.integer(strtoi(b, 16))
  grey_val <- round(0.299 * r_val + 0.587 * g_val + 0.114 * b_val)
  if (grey_val > cutoff) rep(as.raw(255), 3) else rep(as.raw(0), 3)
}

# Magenta binary filter
magenta_filter <- function(r, g, b, cutoff = 127) {
  r_val <- as.integer(strtoi(r, 16))
  g_val <- as.integer(strtoi(g, 16))
  b_val <- as.integer(strtoi(b, 16))
  grey_val <- round(0.299 * r_val + 0.587 * g_val + 0.114 * b_val)
  if (grey_val > cutoff) c(as.raw(255), as.raw(0), as.raw(255)) else rep(as.raw(0), 3)
}


