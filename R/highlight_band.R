#' A Pixel Highlighting Function
#' 
#' This function highlights pixels having values between two thresholds or outside of two thresholds, depending on whether the first threshold is above or below the second.
#' @param im The image
#' @param lo One threshold
#' @param hi Another threshold
#' @export
highlight_band <- function( im, lo, hi){
  if( lo < 0 ) lo <- 0
  if( hi > 1 ) hi <- 1
  if( hi < lo ){
    px <- ( im < hi | im > lo )
  } else {
    px <- ( im > lo & im < hi )
  }
  px_cimg <- as.cimg( px )
  show( px_cimg )
  #show( im )
}
