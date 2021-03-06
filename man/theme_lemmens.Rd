% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot2.R
\docType{data}
\name{theme_lemmens}
\alias{theme_lemmens}
\title{Collection Of Functions And Settings For Ggplot2}
\format{An object of class \code{theme} (inherits from \code{gg}) of length 65.}
\usage{
theme_lemmens
}
\description{
Over the years, I developed a personalized style/theme \code{theme_lemmens}
for use in ggplot2. In
earlier versions, this theme, and some other personalizations were in my
.Rprofile that I synced across machines via Tresorit. \code{lemmens_geoms()}
is an additional function that sets default values for various geoms that made
more sense with my default theme.
}
\details{
To make things a little clearer and more consistent with other packages' styles,
almost
all objects have been renamed. \emph{This will most definately break older scripts.}

\code{lemmens_geoms(use = FALSE)} adapts default values for a series of geoms.
the \code{use} parameter defaults to FALSE, because I used bigger default
sizes in the days that I did not yet use Cairo. When the lemmens package is
loaded, \code{lemmens_geoms()} is automatically called.
}
\keyword{datasets}
