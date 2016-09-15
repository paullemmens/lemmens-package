#' @title Collection of functions and settings for ggplot2
#'
#' @description
#' Over the years, I developed a personalized style/theme \code{theme_lemmens}
#' for use in ggplot2. In
#' earlier versions, this theme, and some other personalizations were in my
#' .Rprofile that I synced across machines via Tresorit. \code{lemmens_geoms()}
#' is an additional function that sets default values for various geoms that made
#' more sense with my default theme.
#'
#' @details
#' To make things a little clearer and more consistent with other packages' styles,
#' almost
#' all objects have been renamed. \emph{This will most definately break older scripts.}
#'
#' \code{theme_lemmens} is my old \code{mytheme2} that is a cousin from
#' \code{mytheme} and
#' distinguishes itself by not using boxes and other decorating stuff around
#' ggplot2's facet headers/strips. \code{theme_l} and \code{theme_lem} are abbreviations.
#'
#' \code{lemmens_geoms(use = FALSE)} adapts default values for a series of geoms.
#' the \code{use} parameter defaults to FALSE, because I used bigger default
#' sizes in the days that I did not yet use Cairo. When the lemmens package is
#' loaded, \code{lemmens_geoms()} is automatically called.
#'
#' @aliases theme_lem theme_l
#'
#' @export
theme_lemmens <- ggplot2::theme_bw() +
	ggplot2::theme(text = ggplot2::element_text(size = 18), 
                 axis.title.x = ggplot2::element_text(vjust = -0.2), 
                 axis.title.y = ggplot2::element_text(angle = 90, vjust = 1),
                 panel.border = ggplot2::element_rect(colour = 'black', size = 1.2),
                 strip.background  =  ggplot2::element_rect(colour  =  NA, fill  =  NA),
                 #strip.text  =  ggplot2::element_text(size  =  rel(1.0)),
                 legend.position = 'bottom')

#' @export
theme_l <- theme_lemmens
#' @export
theme_lem <- theme_lemmens

## Different geom defaults that I predominantly used in my pre-Cairo days.
lemmens_geoms <- function(use=FALSE, reset=!use) {
	if (use) {
		ggplot2::update_geom_defaults('point', list(size=3.5))
		ggplot2::update_geom_defaults('errorbar', list(size=1.0, width=0.3))
		ggplot2::update_geom_defaults('line', list(size=1.5))
		ggplot2::update_geom_defaults('boxplot', list(size=0.8))
		ggplot2::update_geom_defaults('smooth', list(size=1.0))
	} else {
		ggplot2::update_geom_defaults('point', list(size=2))
		ggplot2::update_geom_defaults('errorbar', list(size=0.5, width=0.5))
		ggplot2::update_geom_defaults('line', list(size=0.5))
		ggplot2::update_geom_defaults('boxplot', list(size=0.5))
		ggplot2::update_geom_defaults('smooth', list(size=0.5))
	}
}

## Lastly theme adaptation that I regularly use.
## theme_lemmens.old <- theme_bw() +
## 	theme(text=element_text(size=18), 
##         axis.title.x=element_text(vjust=-0.2), 
## 	      axis.title.y=element_text(angle=90, vjust=1),
##         panel.border=element_rect(colour='black', size=1.2),
##         legend.position='bottom')

pd <- ggplot2::position_dodge(width=0.7)
