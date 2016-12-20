#' @title Bayesian analysis: Bayes Factor T-test
#'
#' @description
#' The Bayes Factor t-test \code{bf.t.test} relies on code from the JASP Github repository to on the
#' one hand not have to rely on non-automated JASP usage for doing Bayesian analyses yet on the other hand
#' obtain the beautiful plots that JASP creates.
#'
#' @param x, y Vectors of (numeric) data.
#' @param paired Logical to specify whether the data in \code{x} and \code{y} are paired data or not (default: FALSE)
#' @param onesided String or logical \code{FALSE} to specify whether a two-sided should be performed (FALSE, default)
#' or one-sided alternatives (either \code{left} or \code{right})
#' @param ... Other parameters passed onto functions that \code{bf.t.test()} calls.
#'
#' @seealso \code{ttestBF} in the BayesFactor package for information on additional parameters that can be provided
#' to \code{bf.t.test()}.
#'
#' @export
## FIXME: add y, paired, and sidedness parameters.
## FIXME: need to test whether not specifying paired in call to .plotPosterior results in it being forwarded via ... argument.
## FIXME: implement only three values for oneSided like in t.test()
bf.t.test <- function(x = NULL, y = NULL, paired = FALSE, oneSided = FALSE, addInformation, ...) {

  ## Translate from JASP's oneSided parameter to BayesFactor::ttestBF nullInterval parameter.
  null.int <- NULL
  if (oneSided == 'left') {
    null.int <- c(-Inf, 0)
  } else if (oneSided == 'right') {
    null.int <- c(0, Inf)
  } else {
    stop('invalid specification of oneSided in bf.t.test().\n')
    }

  ## Package BayesFactor does all the number crunching.
  res.t.test <- BayesFactor::ttestBF(x, y, nullInterval = null.int, rscale = 'medium')
  BF <- BayesFactor::extractBF(res.t.test, onlybf = TRUE, ...)

  ## JASP's code to provide the pretty picture(s).
  .plotPosterior.ttest(x = x, y = y, rscale = "medium", BF = BF, oneSided = oneSided, ...) # FIXME: Add paired parameter?
}


.likelihoodShiftedT <- function(par, data) {
  -sum(log(dt((data - par[1])/par[2], par[3])/par[2]))
}

## function that returns the posterior density
.dposteriorShiftedT <- function(x, parameters, oneSided) {

  if (oneSided == FALSE) {
    dt((x - parameters[1])/parameters[2], parameters[3])/parameters[2]
  } else if (oneSided == "right") {
    ifelse(x >= 0, (dt((x - parameters[1])/parameters[2], parameters[3])/parameters[2])/pt((0 - parameters[1])/parameters[2], parameters[3], lower.tail = FALSE), 0)
  } else if (oneSided == "left") {
    ifelse(x <= 0, (dt((x - parameters[1])/parameters[2], parameters[3])/parameters[2])/pt((0 - parameters[1])/parameters[2], parameters[3], lower.tail = TRUE), 0)
  }
}

## function that returns the prior density
.dprior <- function(x, r, oneSided = oneSided) {

  if (oneSided == "right") {
    y <- ifelse(x < 0, 0, 2/(pi * r * (1 + (x/r)^2)))
    return(y)
  }
    
  if (oneSided == "left") {
    y <- ifelse(x > 0, 0, 2/(pi * r * (1 + (x/r)^2)))
    return(y)

  } else {
    return(1/(pi * r * (1 + (x/r)^2)))
  }
}

## Core function that builds the oh so important plot.
.plotPosterior.ttest <- function(x = NULL, y = NULL, paired = FALSE, oneSided = FALSE, 
                                 BF, BFH1H0 = TRUE, iterations = 10000, rscale = "medium", lwd = 2, 
                                 cexPoints = 1.5, cexAxis = 1.2, cexYlab = 1.5, cexXlab = 1.5, cexTextBF = 1.4, 
                                 cexCI = 1.1, cexLegend = 1.2, lwdAxis = 1.2, addInformation = TRUE, 
                                 dontPlotData = FALSE) {
    
  if (addInformation) {
        
    par(mar = c(5.6, 5, 7, 4) + 0.1, las = 1)
        
  } else {
        
    par(mar = c(5.6, 5, 4, 4) + 0.1, las = 1)
  }
    
  if (dontPlotData) {
        
    plot(1, type = "n", xlim = 0:1, ylim = 0:1, bty = "n", axes = FALSE, 
         xlab = "", ylab = "")
        
    axis(1, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, 
         xlab = "")
    axis(2, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, 
         ylab = "")
        
    mtext(text = "Density", side = 2, las = 0, cex = cexYlab, line = 3.25)
    mtext(expression(paste("Effect size", ~delta)), side = 1, cex = cexXlab, 
          line = 2.5)
        
    return()
  }
    
  if (rscale == "medium") {
    r <- sqrt(2)/2
  }
  if (rscale == "wide") {
    r <- 1
  }
  if (rscale == "ultrawide") {
    r <- sqrt(2)
  }
  if (mode(rscale) == "numeric") {
    r <- rscale
  }
    
  if (oneSided == FALSE) {
    nullInterval <- NULL
  }
  if (oneSided == "right") {
    nullInterval <- c(0, Inf)
  }
  if (oneSided == "left") {
    nullInterval <- c(-Inf, 0)
  }
    
  ## sample from delta posterior
  samples <- BayesFactor::ttestBF(x = x, y = y, paired = paired, posterior = TRUE, 
                                  iterations = iterations, rscale = r)
    
  delta <- samples[, "delta"]
    
  ## fit shifted t distribution
  if (is.null(y)) {
        
    deltaHat <- mean(x)/sd(x)
    N <- length(x)
    df <- N - 1
    sigmaStart <- 1/N
        
  } else if (paired) {
        
    deltaHat <- mean(x - y)/sd(x - y)
    N <- length(x)
    df <- N - 1
    sigmaStart <- 1/N
        
  } else if (!is.null(y) && !paired) {
        
    N1 <- length(x)
    N2 <- length(y)
    sdPooled <- sqrt(((N1 - 1) * var(x) + (N2 - 1) * var(y))/(N1 + N2))
    deltaHat <- (mean(x) - mean(y))/sdPooled
    df <- N1 + N2 - 2
    sigmaStart <- sqrt(N1 * N2/(N1 + N2))
  }
    
  if (sigmaStart < 0.01) 
    sigmaStart <- 0.01
    
  parameters <- try(silent = TRUE, expr = optim(par = c(deltaHat, sigmaStart, 
                                                        df), fn = .likelihoodShiftedT, data = delta, method = "BFGS")$par)
    
  if (class(parameters) == "try-error") {
        
    parameters <- try(silent = TRUE, expr = optim(par = c(deltaHat, 
                                                          sigmaStart, df), fn = .likelihoodShiftedT, data = delta, method = "Nelder-Mead")$par)
  }
    
  if (BFH1H0) {
        
    BF10 <- BF
    BF01 <- 1/BF10
        
  } else {
        
    BF01 <- BF
    BF10 <- 1/BF01
  }
    
    
  ## set limits plot
  xlim <- vector("numeric", 2)
    
  if (oneSided == FALSE) {
        
    xlim[1] <- min(-2, quantile(delta, probs = 0.01)[[1]])
    xlim[2] <- max(2, quantile(delta, probs = 0.99)[[1]])
        
    if (length(x) < 10) {
            
      if (addInformation) {
                
        stretch <- 1.52
      } else {
                
        stretch <- 1.4
      }
            
    } else {
            
      stretch <- 1.2
    }
        
  }
    
  if (oneSided == "right") {
        
    ## if (length(delta[delta >= 0]) < 10) return('Plotting is not
    ## possible: To few posterior samples in tested interval')
        
    xlim[1] <- min(-2, quantile(delta[delta >= 0], probs = 0.01)[[1]])
    xlim[2] <- max(2, quantile(delta[delta >= 0], probs = 0.99)[[1]])
        
    if (any(is.na(xlim))) {
            
      xlim[1] <- min(-2, .qShiftedT(0.01, parameters, oneSided = "right"))
      xlim[2] <- max(2, .qShiftedT(0.99, parameters, oneSided = "right"))
            
    }
        
    stretch <- 1.32
  }
    
  if (oneSided == "left") {
        
    ## if (length(delta[delta <= 0]) < 10) return('Plotting is not
    ## possible: To few posterior samples in tested interval')
        
    xlim[1] <- min(-2, quantile(delta[delta <= 0], probs = 0.01)[[1]])
    xlim[2] <- max(2, quantile(delta[delta <= 0], probs = 0.99)[[1]])
        
    if (any(is.na(xlim))) {
            
      xlim[1] <- min(-2, .qShiftedT(0.01, parameters, oneSided = "left"))
      xlim[2] <- max(2, .qShiftedT(0.99, parameters, oneSided = "left"))
            
    }
        
    stretch <- 1.32
  }
    
  xticks <- pretty(xlim)
    
  ylim <- vector("numeric", 2)
    
  ylim[1] <- 0
  dmax <- optimize(function(x) .dposteriorShiftedT(x, parameters = parameters, oneSided = oneSided),
                   interval = range(xticks), maximum = TRUE)$objective
  ylim[2] <- max(stretch * .dprior(0, r, oneSided = oneSided), stretch * 
                                                               dmax)  # get maximum density
    
  ## calculate position of 'nice' tick marks and create labels
  yticks <- pretty(ylim)
  xlabels <- formatC(xticks, 1, format = "f")
  ylabels <- formatC(yticks, 1, format = "f")
    
  ## compute 95% credible interval & median:
  if (oneSided == FALSE) {
        
    CIlow <- quantile(delta, probs = 0.025)[[1]]
    CIhigh <- quantile(delta, probs = 0.975)[[1]]
    medianPosterior <- median(delta)
        
    if (any(is.na(c(CIlow, CIhigh, medianPosterior)))) {
            
      CIlow <- .qShiftedT(0.025, parameters, oneSided = FALSE)
      CIhigh <- .qShiftedT(0.975, parameters, oneSided = FALSE)
      medianPosterior <- .qShiftedT(0.5, parameters, oneSided = FALSE)
    }
  }
    
  if (oneSided == "right") {
        
    CIlow <- quantile(delta[delta >= 0], probs = 0.025)[[1]]
    CIhigh <- quantile(delta[delta >= 0], probs = 0.975)[[1]]
    medianPosterior <- median(delta[delta >= 0])
        
    if (any(is.na(c(CIlow, CIhigh, medianPosterior)))) {
            
      CIlow <- .qShiftedT(0.025, parameters, oneSided = "right")
      CIhigh <- .qShiftedT(0.975, parameters, oneSided = "right")
      medianPosterior <- .qShiftedT(0.5, parameters, oneSided = "right")
    }
  }
    
  if (oneSided == "left") {
        
    CIlow <- quantile(delta[delta <= 0], probs = 0.025)[[1]]
    CIhigh <- quantile(delta[delta <= 0], probs = 0.975)[[1]]
    medianPosterior <- median(delta[delta <= 0])
        
    if (any(is.na(c(CIlow, CIhigh, medianPosterior)))) {
            
      CIlow <- .qShiftedT(0.025, parameters, oneSided = "left")
      CIhigh <- .qShiftedT(0.975, parameters, oneSided = "left")
      medianPosterior <- .qShiftedT(0.5, parameters, oneSided = "left")
    }
        
  }
    
  posteriorLine <- .dposteriorShiftedT(x = seq(min(xticks), max(xticks), 
                                               length.out = 1000), parameters = parameters, oneSided = oneSided)
    
  xlim <- c(min(CIlow, range(xticks)[1]), max(range(xticks)[2], CIhigh))
    
  plot(1, 1, xlim = xlim, ylim = range(yticks), ylab = "", xlab = "", type = "n", axes = FALSE)
    
  lines(seq(min(xticks), max(xticks), length.out = 1000), posteriorLine, lwd = lwd)
  lines(seq(min(xticks), max(xticks), length.out = 1000), .dprior(seq(min(xticks), max(xticks), length.out = 1000),
                                                                  r = r, oneSided = oneSided), 
        lwd = lwd, lty = 3)
    
  axis(1, at = xticks, labels = xlabels, cex.axis = cexAxis, lwd = lwdAxis)
  axis(2, at = yticks, labels = ylabels, , cex.axis = cexAxis, lwd = lwdAxis)
    
    
  if (nchar(ylabels[length(ylabels)]) > 4) {
    mtext(text = "Density", side = 2, las = 0, cex = cexYlab, line = 4)
  } else if (nchar(ylabels[length(ylabels)]) == 4) {
    mtext(text = "Density", side = 2, las = 0, cex = cexYlab, line = 3.25)
  } else if (nchar(ylabels[length(ylabels)]) < 4) {
    mtext(text = "Density", side = 2, las = 0, cex = cexYlab, line = 2.85)
  }
    
  mtext(expression(paste("Effect size", ~delta)), side = 1, cex = cexXlab, line = 2.5)
    
  points(0, .dprior(0, r, oneSided = oneSided), col = "black", pch = 21, bg = "grey", cex = cexPoints)
    
  if (oneSided == FALSE) {
    heightPosteriorAtZero <- .dposteriorShiftedT(0, parameters = parameters, oneSided = oneSided)

  } else if (oneSided == "right") {
    
    posteriorLineLargerZero <- posteriorLine[posteriorLine > 0]
    heightPosteriorAtZero <- posteriorLineLargerZero[1]
    
  } else if (oneSided == "left") {
        
    posteriorLineLargerZero <- posteriorLine[posteriorLine > 0]
    heightPosteriorAtZero <- posteriorLineLargerZero[length(posteriorLineLargerZero)]
  }
    
  points(0, heightPosteriorAtZero, col = "black", pch = 21, bg = "grey", 
         cex = cexPoints)
    
### 95% credible interval
    
  ## enable plotting in margin
  par(xpd = TRUE)
    
  yCI <- grconvertY(dmax, "user", "ndc") + 0.04
  yCI <- grconvertY(yCI, "ndc", "user")
    
  arrows(CIlow, yCI, CIhigh, yCI, angle = 90, code = 3, length = 0.1, lwd = lwd)
    
  medianText <- formatC(medianPosterior, digits = 3, format = "f")
    
  if (addInformation) {
        
    ## display BF10 value
    offsetTopPart <- 0.06
        
    yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
    yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
        
    xx <- min(xticks)
        
    if (BF10 >= 1000000 | BF01 >= 1000000) {
            
      BF10t <- formatC(BF10, 3, format = "e")
      BF01t <- formatC(BF01, 3, format = "e")
    }
        
    if (BF10 < 1000000 & BF01 < 1000000) {
            
      BF10t <- formatC(BF10, 3, format = "f")
      BF01t <- formatC(BF01, 3, format = "f")
    }
        
    if (oneSided == FALSE) {
      text(xx, yy2, bquote(BF[10] == .(BF10t)), cex = cexTextBF, pos = 4)
      text(xx, yy, bquote(BF[0][1] == .(BF01t)), cex = cexTextBF, pos = 4)
    }
        
    if (oneSided == "right") {
      text(xx, yy2, bquote(BF["+"][0] == .(BF10t)), cex = cexTextBF, pos = 4)
      text(xx, yy, bquote(BF[0]["+"] == .(BF01t)), cex = cexTextBF, pos = 4)
    }
        
    if (oneSided == "left") {
      text(xx, yy2, bquote(BF["-"][0] == .(BF10t)), cex = cexTextBF, pos = 4)
      text(xx, yy, bquote(BF[0]["-"] == .(BF01t)), cex = cexTextBF, pos = 4)
    }
        
    yy <- grconvertY(0.756 + offsetTopPart, "ndc", "user")
    yy2 <- grconvertY(0.812 + offsetTopPart, "ndc", "user")
        
    CIText <- paste("95% CI: [", bquote(.(formatC(CIlow, 3, format = "f"))), 
                    ", ", bquote(.(formatC(CIhigh, 3, format = "f"))), "]", sep = "")
    medianLegendText <- paste("median =", medianText)
        
    text(max(xticks), yy2, medianLegendText, cex = 1.1, pos = 2)
    text(max(xticks), yy, CIText, cex = 1.1, pos = 2)
        
    ## probability wheel
    if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
      xx <- grconvertX(0.44, "ndc", "user")
    }
        
    if (max(nchar(BF10t), nchar(BF01t)) == 5) {
      xx <- grconvertX(0.44 + 0.001 * 5, "ndc", "user")
    }
        
    if (max(nchar(BF10t), nchar(BF01t)) == 6) {
      xx <- grconvertX(0.44 + 0.001 * 6, "ndc", "user")
    }
        
    if (max(nchar(BF10t), nchar(BF01t)) == 7) {
      xx <- grconvertX(0.44 + 0.002 * max(nchar(BF10t), nchar(BF01t)), 
                       "ndc", "user")
    }
        
    if (max(nchar(BF10t), nchar(BF01t)) == 8) {
      xx <- grconvertX(0.44 + 0.003 * max(nchar(BF10t), nchar(BF01t)), 
                       "ndc", "user")
    }
        
    if (max(nchar(BF10t), nchar(BF01t)) > 8) {
      xx <- grconvertX(0.44 + 0.005 * max(nchar(BF10t), nchar(BF01t)), 
                       "ndc", "user")
    }
        
    yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")
        
    ## make sure that colored area is centered
    radius <- 0.06 * diff(range(xticks))
    A <- radius^2 * pi
    alpha <- 2/(BF01 + 1) * A/radius^2
    startpos <- pi/2 - alpha/2
        
    ## draw probability wheel
    plotrix::floating.pie(xx, yy, c(BF10, 1), radius = radius, col = c("darkred", "white"), lwd = 2,
                          startpos = startpos)
        
    yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
    yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
        
    if (oneSided == FALSE) {
      text(xx, yy, "data|H1", cex = cexCI)
      text(xx, yy2, "data|H0", cex = cexCI)
    }
        
    if (oneSided == "right") {
      text(xx, yy, "data|H+", cex = cexCI)
      text(xx, yy2, "data|H0", cex = cexCI)
    }
        
    if (oneSided == "left") {
      text(xx, yy, "data|H-", cex = cexCI)
      text(xx, yy2, "data|H0", cex = cexCI)
    }
        
    ## add legend
    CIText <- paste("95% CI: [", bquote(.(formatC(CIlow, 3, format = "f"))), 
                    " ; ", bquote(.(formatC(CIhigh, 3, format = "f"))), "]", sep = "")
        
    medianLegendText <- paste("median =", medianText)
  }
    
  mostPosterior <- mean(delta > mean(range(xticks)))
    
  if (mostPosterior >= 0.5) {
        
    legendPosition <- min(xticks)
    legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), 
           lty = c(1, 3), bty = "n", lwd = c(lwd, lwd), cex = cexLegend, 
           xjust = 0, yjust = 1, x.intersp = 0.6, seg.len = 1.2)
  } else {
        
    legendPosition <- max(xticks)
    legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), 
           lty = c(1, 3), bty = "n", lwd = c(lwd, lwd), cex = cexLegend, 
           xjust = 1, yjust = 1, x.intersp = 0.6, seg.len = 1.2)
  }
}

