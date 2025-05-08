#' Apply a pixel filter to an image
#'
#' @param image_path Path to the image file
#' @param filter_function A function that returns a raw RGB triplet
#' @return A magick image object
#' @examples
#' \dontrun{
#' image_filter("path/to/image.jpg", grayscale_filter)
#' }
#' @import magick
#' @importFrom crayon green blue magenta cyan bold
#' @export
image_filter <- function(image_path, filter_function) {
  img <- image_read(image_path)
  img_data <- image_data(img)
  bitmap <- img[[1]]
  dims <- dim(bitmap)

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
