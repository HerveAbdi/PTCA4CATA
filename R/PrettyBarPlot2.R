# functions in this file:
#  areColors
#  lighten
#  PrettyBarPlot2
#_____________________________________________________________________
# JolisPlots
# Jolie Plot: the ggplots version of prettyBar etc.
# Vincent Guillemot and Hervé Abdi
# Created September 14, 2018
#

#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  sinew::makeOxygen(areColors)
#
#_____________________________________________________________________


#_____________________________________________________________________
# First helper functions
#_____________________________________________________________________
#_____________________________________________________________________
# areColors ----
# Credit : https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
#' @title Check that color names are the names of some real colors.
#' @description  \code{areColors}: Check that colores names are the names of some real colors.
#' @param x a string vectors with (to be checked) naesm of colors.
#' @return a logical vector with \code{TRUE} if a color, \code{FALSE} if not.
#' @details credit:
#' \url{Credit: https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation}
#' @examples
#' areColors(c(NA, "black", "blackk", "1", "#00", "#000000"))
#' @rdname areColors
#' @export
areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}
# areColors(c(NA, "black", "blackk", "1", "#00", "#000000"))
#_____________________________________________________________________


#_____________________________________________________________________
# lighten ----
#_____________________________________________________________________
#  sinew::makeOxygen(lighten)
#_____________________________________________________________________
# lighten
# Credit : @Roland, https://stackoverflow.com/questions/30219738/is-there-a-way-to-programmatically-darken-the-color-given-rgb-values

#' @title gives a lighter shade of a color
#' @description \code{lighten} gives a lighter shade of a color.
#' @param colors a vector of color names.
#' @param   factor (Default: 1.4) the lightening factor
#' @param   maxColorValue (Default: 255) the maximum possible for a color component.
#' @return  A lightened color.
#' @details
#'  Credit to Roland, see:
#' \url{https://stackoverflow.com/questions/30219738/is-there-a-way-to-programmatically-darken-the-color-given-rgb-values}
#' @examples
#' lighten(c('#FF0000FC','#E6E6FAFC'), 1.3)
#' @rdname lighten
#' @export
#' @importFrom colorspace hex2RGB hex

lighten <- function(colors, factor=1.4, maxColorValue=255){
  # require(colorspace)
  cols1 <- colorspace::hex2RGB(colors)
  # transform to hue/lightness/saturation colorspace
  cols1 <- as(cols1, "HLS")
  # multiplicative decrease of lightness
  cols1@coords[, "L"] <- cols1@coords[, "L"] * factor
  # going via rgb seems to work better
  return(colorspace::hex(as(cols1, "RGB")))
}

#_____________________________________________________________________
#  sinew::makeOxygen(PrettyBarPlot2)
#_____________________________________________________________________
# PrettyBarPlot2 preamble ----
#' @title a \code{ggplot2}-based function to plot bar plots for a series
#' of variables:
#' Used for plotting bootstrap ratios or contributions in PCA, PLS, etc.
#'
#' @description \code{PrettyBarPlot2}:
#' a \code{ggplot2}-based version of \code{PrettybarPlot} and
#' \code{PrettyBarPlotColor}.
#' \code{PrettyBarPlot2}
#' creates bar plots for a set of items
#' (e.g., observations or variables) analyzed with multivariate methods
#' such as PCA, CA, MCA, PLS etc.
#' PrettyBarPlot is used to display the bootstrap ratios
#' or contributions plots for CA/MCA/PCA/PLS.
#' Significant or important items are plotted
#' in color, non-significant items are plotted in gray.


#' @param bootratio the bootstrap ratios (BR) or contributions
#' or similar statistics to be plotted.
#' (e.g., obtained from Boot4PTCA).
#' @param font.size PARAM_DESCRIPTION, Default: NULL
#' @param threshold  (Default: 2)
#' The critical value for significance
#'  which matches a \eqn{p} < .05
#' significance level. The |BR| < threshold are plotted in gray,
#' the other BR are plotted in colors.
#' @param ylim (Default: NULL)
#' a 2-element vector giving min and max for the y-axis;
#' when NULL (default) set to c(-max(abs(y)), max(abs(y)))
#' @param color.bar a 3-element vector
#' of color names for the bars for (respectively)
#' significant positive,
#' significant negative, and non-significant.
#' Default is c('lavender','darkolivegreen3','gray90').
#' @param color.bord The color for the boder of the bars,
#' Default: c("mediumpurple4", "darkolivegreen4", "gray75").
#' @param color.letter a 3-element vector of color names for the names of
#' the items for (respectively)
#' significant positive, significant negative, and non-significant.
#' Default is c("mediumpurple4",'darkolivegreen4','gray75')
#' @param color4ns (Default: \code{'gray75'})
#' color for the non-significant \code{bootratio}.
#' @param color4bar (Default: NULL) a vector of color names
#' (same dimension as \code{bootratio})
#' @param plotnames if TRUE (default) write the names of the items
#' @param main (default is \code{NULL}) a title for the graph
#' @param ylab (default is \code{NULL}) a label for the \eqn{y} axis (i.e., BR).
#' If \code{NULL} \code{ylab = 'Bootstrap ratios'}.
#' @param sortValues (Default: \code{FALSE}) when \code{TRUE}
#' sort the values to plot.
#' @param signifOnly (Default: \code{FALSE}) when \code{TRUE}
#' plot only the significant values.
#' @param angle.text  (Default: \code{90}, i.e., so slant)
#' the "slanting factor" for the text.
#' @param horizontal (Default: \code{TRUE}) when \code{TRUE},
#' the plot is horizontal, when \code{FALSE} the plot is vertical.
#' @return A \code{ggplot2} object containg the graph
#' (i.e., to be plotted with \code{print}).
#' @details \code{PrettyBarPlot2} intergrates \code{PrettyBarPlot}
#' and \code{PrettyBarPlotColors}.
#' @author Vincent Guillemot & Hervé Abdi
#' @seealso \code{\link{PrettyBarPlot}}
#' \code{\link{PrettyBarPlotColor}}
#' @examples
#' toto <- 8*runif(7)
#' names(toto) <- paste0('V', 1:7)
#' PrettyBarPlot2(toto)
#' @rdname PrettyBarPlot2
#' @export


PrettyBarPlot2 <- function(bootratio,
                           font.size = NULL,
                           threshold = 3,
                           ylim = NULL,
                           color.bar = c("lavender",
                                         "darkolivegreen3",
                                         "gray90"),
                           color.bord = c("mediumpurple4",
                                          "darkolivegreen4",
                                          "gray75"),
                           color.letter = c("mediumpurple4",
                                            "darkolivegreen4",
                                            "gray75"),
                           color4ns = "gray75",
                           color4bar = NULL,
                           plotnames = TRUE,
                           main = NULL,
                           ylab = NULL,
                           sortValues = FALSE,
                           signifOnly = FALSE,
                           angle.text = 90,
                           horizontal = TRUE) {
  if (signifOnly) {
    if (!is.null(color4bar)) {
      color4bar <- color4bar[abs(bootratio) > threshold]
    }
    bootratio <- bootratio[abs(bootratio) > threshold]
  }
  if (sortValues) bootratio <- sort(bootratio)
  if (is.null(ylim)) {
    lemax <- round(max(abs(bootratio))) + 1
    if (any(bootratio < 0)) {
      ylim <- c(-lemax, lemax)
    } else {
      ylim <- c(0, lemax)
    }
  }
  lesnoms <- names(bootratio)
  nel <- length(bootratio)
  if (is.null(color4bar)) {
    lescouleurs <- lescouleurs.font <- lescouleurs.bord <- rep(color.bar[3], nel)
    lescouleurs[bootratio >  threshold] <- color.bar[1]
    lescouleurs[bootratio < -threshold] <- color.bar[2]
    lescouleurs.bord[bootratio >  threshold] <- color.bord[1]
    lescouleurs.bord[bootratio < -threshold] <- color.bord[2]
    lescouleurs.font[bootratio >  threshold] <- color.letter[1]
    lescouleurs.font[bootratio < -threshold] <- color.letter[2]
  } else {
    if (length(color4bar) > nel) stop("Too many colors for the bars.")
    if (length(color4bar) < nel) stop("Not enough colors for the bars.")
    if (any(!areColors(color4bar))) stop("Something is wrong in color4bar.")
    lescouleurs.font <- lescouleurs.bord <- color4bar
    lescouleurs <- lighten(color4bar, factor = 1.3)
    lescouleurs[abs(bootratio) < threshold] <- color4ns
    lescouleurs.bord[abs(bootratio) < threshold] <- NA
    lescouleurs.font[abs(bootratio) < threshold] <- color4ns
  }
  if (is.null(font.size)) font.size <- 1
  lafont <- rep(font.size, nel)
  lafont[abs(bootratio) < threshold] = font.size / 2
  LesNoms2Print <- lesnoms
  dat <- data.frame(IDnum = factor(seq_along(LesNoms2Print)),
                    ID = LesNoms2Print,
                    bootratio = bootratio,
                    lescouleurs = lescouleurs,
                    lescouleurs.font = lescouleurs.font,
                    lescouleurs.bord = lescouleurs.bord,
                    stringsAsFactors = FALSE)
  if (horizontal) {
    p <- ggplot(dat, aes(x = IDnum, y = bootratio, fill = IDnum, color = IDnum)) +
      geom_hline(yintercept = c(threshold, -threshold),
                 col="red", alpha=0.5, linetype=2) +
      geom_col() +
      geom_text(aes(x = IDnum, y = 0, label = ID),
                hjust = ifelse(bootratio >= 0, 1.1, -0.1),
                size = lafont,
                angle = angle.text) +
      geom_hline(yintercept = 0) +
      scale_fill_manual(values=lescouleurs) +
      scale_color_manual(values=lescouleurs.bord) +
      guides(fill=FALSE, color=FALSE) +
      labs(x = "", y = ylab) +
      ggtitle(main) +
      theme_bw() +
      theme(axis.line.y = element_line(colour = "black"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      ylim(ylim)
  } else {
    p <- ggplot(dat, aes(x=IDnum, y=bootratio, fill=IDnum, color=IDnum)) +
      geom_hline(yintercept = c(threshold, -threshold),
                 col="red", alpha=0.5, linetype=2) +
      geom_col() +
      geom_text(aes(x = IDnum, y = 0, label = ID),
                hjust = ifelse(bootratio >= 0, 1.1, -0.1),
                size = lafont,
                angle = angle.text) +
      geom_hline(yintercept = 0) +
      scale_fill_manual(values=lescouleurs) +
      scale_color_manual(values=lescouleurs.bord) +
      guides(fill = FALSE, color = FALSE) +
      labs(x = "", y = ylab) +
      ggtitle(main) +
      theme_bw() +
      theme(axis.line.x      = element_line(colour = "black"),
            axis.title.y     = element_blank(),
            axis.text.y      = element_blank(),
            axis.ticks.y     = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border     = element_blank(),
            panel.background = element_blank()) +
      ylim(ylim) +
      coord_flip()
  }

  return(p)
}
#_____________________________________________________________________
#_____________________________________________________________________
