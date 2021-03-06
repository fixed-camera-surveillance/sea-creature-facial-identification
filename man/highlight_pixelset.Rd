% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/highlight_pixelset.R
\name{highlight_pixelset}
\alias{highlight_pixelset}
\title{An Image Format Conversion Function}
\usage{
highlight_pixelset(im, sigma = 3, threshold = 0.5)
}
\arguments{
\item{im}{The image}

\item{sigma}{Parameter used by isoblur. Default 3}

\item{threshold}{Highlight pixels having values greater than this. Default 0.5}
}
\description{
This function converts image data obtained via OpenImageR::readImage() into C-image format.
}
