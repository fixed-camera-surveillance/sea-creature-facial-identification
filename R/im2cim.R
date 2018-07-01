#' An Image Format Conversion Function
#' 
#' This function converts image data obtained via OpenImageR::readImage() into C-image format.
#' @param im The image to convert
#' @export
im2cim <- function( im ){
  d <- dim( im )
  N <- length( d )
  data <- im
  if (( N == 3 ) & ( d[ 3 ] == 3 )) data <- array( im, dim = c( d[ 1:2 ], 1, d[ 3 ] ) )
  c_image <- imager::as.cimg( data )
  c_image %>%
  imager::imrotate( 90 ) %>%
  imager::mirror( 'x' )
}
