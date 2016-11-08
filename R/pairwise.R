#' @title Personalized pairwise correlation plots
#'
#' @description
#' \code{pairsplot()} creates a pairwase scatter/correlation plots for each
#' column of the matrix \code{mat} against the other ones. It uses two custom
#' panel functions that work best with non-categorical data. If you need to do
#' a smarter way to look at data, consider using ggpairs in the ggally package.
#'
#' @details
#' The default paneling functions that \code{pairsplot} uses are listed in the
#' help file for the \code{pairs()} function.
#'
#' @param mat A matrix of numeric (non-categorical) data
#'
#' @seealso
#' \link{https://ggobi.github.io/ggally/ggpairs.html} for a function that tackles
#' varying types of data in a smarter way.
#' \link{http://personality-project.org/r/Rfunc/pairs.panels.R}
#'
#' @return The function returns invisibly.
#'
#' @export 
#' 
#FIXME: make sure to make the dots permeate further in the function
pairsplot <- function(mat, ...) {
  pairs(mat, upper.panel = .panel.cor, diag.panel = .panel.hist)
  invisible()
}

.panel.cor <- function(x, y, digits = 2, prefix = "", ...)
{
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor.test(x, y, method = 'spearman', exact = FALSE)
  txt <- format(c(r$estimate, 0.123456789), digits = digits)[1]
  if (r$p.value < 0.0001) {
    postfix <- '***'
  } else if (r$p.value < 0.001) {
    postfix <- '**'
  } else if (r$p.value < 0.05) {
    postfix <- '*'
  } else {
    postfix <- ''
  }
  txt <- paste0(prefix, txt, postfix)
  cex.cor <- 0.8/strwidth(txt)    
  text(0.5, 0.5, txt, cex = cex.cor)
}

.panel.hist <- function(x, ...)
{
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "black", ...)
}

