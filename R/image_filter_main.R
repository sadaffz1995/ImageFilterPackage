

# Main function to apply pixel-level filter and return modified image
image_filter <- function(image_path, filter_function) {

  img <- image_read(image_path)
  img_data <- image_data(img)


  bitmap <- img[[1]]
  dims <- dim(bitmap) # Extract bitmap

  # Apply filter to each pixel
  for (x in 1:dims[2]) {
    for (y in 1:dims[3]) {
      r <- img_data[1, x, y]
      g <- img_data[2, x, y]
      b <- img_data[3, x, y]
      bitmap[, x, y] <- filter_function(r, g, b)
    }
  }

  image_read(bitmap)
}


