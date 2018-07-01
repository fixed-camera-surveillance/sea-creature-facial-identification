#' A Collection of ggplot Shortcuts
#' 
#' These functions put commonly used ggplot commands into plain language
#' @param im An image
#' @export
make_a_plot <- function( df ) ggplot( df, aes( x, y ))

#' @export
show_the_image <- function( f ) geom_raster( aes( fill = value ))

#' @export
remove_grey_margin <- function(f) scale_x_continuous(expand=c( 0, 0 ))

#' @export
flip_vertically <- function( f ) scale_y_continuous(
      expand=c( 0, 0 )
    , trans = scales::reverse_trans()
  )

#' A C-Image Plotting Function.
#' 
#' This function plots an image (in C-image format) using a variety of color settings
#' @param im An image
#' @param palette A collection of colors
#' @export
plot_cimg <- function( im, palette=c( 'black', 'white') ){

  if( !( 'cimg' %in% class( im )) ){
    return( 'Input must be of class *cimg*' )
  }

  N <- length( palette )

  # Use the number of colors in the palette to select which
  # scale_file_...function to use: [ gradient, gradient2, gradientn ]
  if( N == 1 ){
    colors <- c( low = 'black', high = palette )
    fill_function <- scale_fill_gradient( colors )
    #                               ---^---
  }

  if( N == 2 ){
    colors <- c( low = palette[ 1 ], high = palette[ 2 ] )
    fill_function <- scale_fill_gradient( colors )
    #                               ---^---
  }

  if( N == 3 ){
    colors <- c(
        low = palette[ 1 ]
      , mid = palette[ 2 ]
      , high = palette[ 3 ]
    )
    fill_function <- scale_fill_gradient2( colors )
    #                               ----^---
  }

  if( N > 3 ){
    fill_function <- scale_fill_gradientn( colors = palette )
    #                               ----^---
  }

  clean_theme <- theme(
    , axis.text.x = element_blank()
    , axis.text.y = element_blank()
    , axis.ticks  = element_blank()
    , axis.line   = element_line(
        color = 'darkblue', size = 1, linetype = 'blank'
      )
    , legend.position = 'none'
  )
 
  grayscale( im ) %>% as.data.frame -> df
  a_C_image <- make_a_plot( df ) +
    show_the_image() +
    remove_grey_margin() +
    flip_vertically() + 
    fill_function +
    xlab( '') +
    ylab( '') +
    clean_theme() +
    coord_fixed( ratio = 1 )
   # scale_fill_gradient2(low="black",mid="green",high='white')
  return( a_C_image )
}
# plot_cimg(im4,c('white'))
# plot_cimg(im4,c('blue','white'))
# plot_cimg(im4,c('white','blue'))
# plot_cimg(im4,c('blue','white','black'))
# plot_cimg(im4,c('blue','lightgrey','white','black'))
# plot_cimg(im4,c('blue','lightgrey','white','darkgrey','black'))
# plot_cimg(im4,c('blue','lightgrey','white','orange','darkgrey','black'))
# plot_cimg(im4,c('blue','lightgrey','green','white','orange','darkgrey','black'))
# plot_cimg(im4,c('blue','red','white','lightgrey','green','white','orange','darkgrey','black'))
# plot_cimg(im4,c('white','red','white','brown','white','blue','white','green','white','lightgrey','grey','darkgrey','black','black','black','black','black'))

# plot_cimg(im4,c('white','red','white','blue','yellow','brown','green','white','blue','white','white','white','white','white','white','white','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black'))