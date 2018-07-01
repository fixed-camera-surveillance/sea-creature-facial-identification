#' An Image Format Conversion Function
#' 
#' This function converts image data obtained via OpenImageR::readImage() into C-image format.
#' @param im The image
#' @param sigma Parameter used by isoblur. Default 3
#' @param threshold Highlight pixels having values greater than this. Default 0.5
#' @export
highlight_pixelset <- function( im, sigma=3, threshold=0.5 ){
  show( an_image )
  px <- ( isoblur( an_image, sigma = sigma )  > threshold )
  highlight( px )
}
