#' A Light De-Trending Function
#' 
#' This function adjusts the light in an image by removing a linear trend.
#' @param im The image to adjust
#' @param outfile Where to put the result
#' @param channel Grey or Color. Defaults to Color
#' @param k Scale factor. Defaults to 1.0


# ----------------------------------
#  png( file = outfile ) # *** NB: this makhttps://en.gravatar.com/mertnuhoglues an image with FOUR color channels:  num [1:480, 1:480, 1:4] 1 1 1 1 1 1 1 1 1 1 
# ----------------------------------

#' @export
detrend_image <- function( im, outfile='', channel = 'Color', k=1.0 ){
  cat( sprintf( "\nscaling with k = %s\n", k))
  produce_detrended_images( im ) -> images
  output <- imager::imresize( images[[ channel ]], k )
  if( nchar( outfile ) > 0 ){
    cat( sprintf( "\nsaving\n"))
    imager::save.image(
        imager::imresize( output, k )
      , outfile
    )
  }
  output
}

  produce_detrended_images <- function( im, k=k ){
    p1 <- detrend( im, 'grey' )
    p2 <- detrend( im, 'red' )
    p3 <- detrend( im, 'green' )
    p4 <- detrend( im, 'blue' )

    # Make a C-image
    results <- im
    # Insert de-trended Red
    results[ , , 1, 1 ] <- p2
    # Insert de-trended Green
    results[ , , 1, 2 ] <- p3
    # Insert de-trended Blue
    results[ , , 1, 3 ] <- p4
    list( 'Grey' = p1, 'Color' = results )
  }

    detrend <- function( im, channel='grey', k=k ){
      stopifnot( channel %in% c( 'grey', 'red', 'green', 'blue' ))
      data <- switch( channel
        , grey  = imager::grayscale( im )
        , red   = imager::R( im )
        , green = imager::G( im )
        , blue  = imager::B( im )
      )
      dframe <- as.data.frame( data )
      linear_trend <- lm( value ~ x + y, data = dframe )
      data - fitted( linear_trend )
    }
