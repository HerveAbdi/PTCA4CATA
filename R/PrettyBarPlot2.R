# Entête -----
# functions in this file:
###  areColors ----
###  lighten ----
###  PrettyBarPlot2 ----
#_____________________________________________________________________
# JolisPlots
# JoliPlot: the ggplots version of prettyBar etc.
# Vincent Guillemot and Hervé Abdi
# Created September 14, 2018
# Last Changes Vincent G. & Hervé A.
# September 22, 2021.
# fix the warning
#  guides(<scale> = FALSE)` is deprecated.
#  Please use `guides(<scale> = "none")`
#
#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  sinew::makeOxygen(areColors)
#_____________________________________________________________________
# First helper functions
#_____________________________________________________________________
# areColors ----
# Credit : https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
#' @title Check that color names are
#' the names of some real colors.
#' @description  \code{areColors}: Check that color
#' names are the names of some real colors.
#' @param x a string vectors with (to be checked)
#' names of colors.
#' @return a logical vector with
#' \code{TRUE} if a color, \code{FALSE} if not.
#' @details credit:
#' \url{Credit: https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation}
#' @examples
#' areColors(c(NA, "black", "blackk", "1", "#00", "#000000"))
#' @rdname areColors
#' @importFrom grDevices col2rgb
#' @export
areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(
      is.matrix(grDevices::col2rgb(X)),
      error = function(e)
        FALSE
    )
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
#' @description \code{lighten} gives a
#' lighter shade of a color.
#' @param colors a vector of color names.
#' @param   factor (Default: 1.4) the lightening factor.
#' @param   maxColorValue (Default: 255)
#'  the maximum possible for a color component.
#' @return  A lightened color.
#' @details
#'  Credit to Roland, see:
#' \url{https://stackoverflow.com/questions/30219738/is-there-a-way-to-programmatically-darken-the-color-given-rgb-values}
#' @examples
#' lighten(c('#FF0000FC','#E6E6FAFC'), 1.3)
#' @rdname lighten
#' @export
#' @importFrom colorspace hex2RGB hex
#' @importFrom methods as


lighten <- function(colors,
                    factor = 1.4,
                    maxColorValue = 255) {
  # require(colorspace)
  cols1 <- colorspace::hex2RGB(colors)
  # transform to hue/lightness/saturation colorspace
  cols1 <- methods::as(cols1, "HLS")
  # multiplicative decrease of lightness
  cols1@coords[, "L"] <- cols1@coords[, "L"] * factor
  # going via rgb seems to work better
  return(colorspace::hex(as(cols1, "RGB")))
}

#_____________________________________________________________________
#  sinew::makeOxygen(PrettyBarPlot2)
#_____________________________________________________________________
# PrettyBarPlot2 preamble ----
#' @title a \code{ggplot2}-based
#' function to plot bar plots
#' for a series
#' of variables:
#' Used for plotting bootstrap ratios
#' or contributions for
#' PCA, CA, MCA, PLS, etc.
#'
#' @description \code{PrettyBarPlot2}:
#' a \code{ggplot2}-based version of
#' \code{PrettybarPlot} and
#' \code{PrettyBarPlotColor}.
#' \code{PrettyBarPlot2}
#' creates bar plots for a set of items
#' (e.g., observations or variables) analyzed
#' with multivariate methods
#' such as PCA, CA, MCA, PLS etc.
#' \code{PrettyBarPlot2}
#' is used to display the bootstrap ratios
#' or contributions plots for CA/MCA/PCA/PLS.
#' Significant or important items are plotted
#' in color, non-significant (or un-important)
#' items are plotted in gray.
#'
#' @details Note that this version
#' is still in
#' development. Current version generates
#' a (strange) warning message
#' from \code{ggplot2}
#' about \code{element_text()}.

#' @param bootratio
#' the bootstrap ratios
#' (BR) or contributions
#' or similar statistics to be plotted.
#' (e.g., obtained from \code{Boot4PTCA}).
#' @param font.size
#' (Default: \code{NULL})
#' the font size to write
#' the name of the item. When \code{NULL,
#' font.size = 1}.
#' @param threshold
#' (Default: \code{2})
#' The critical value for significance
#'  which matches a \eqn{p} < .05
#' significance level.
#' The |BR| < threshold are plotted in gray,
#' the other BR are plotted in colors.
#' @param ylim
#' (Default: \code{NULL})
#' a 2-element vector giving \code{min} and
#' \code{max} for the y-axis;
#' when NULL (default) set to
#' \code{c(-max(abs(y)), max(abs(y)))}
#' @param color.bar
#' a 3-element vector
#' of color names for the bars for (respectively)
#' significant positive,
#' significant negative, and non-significant.
#' Default is
#' \code{c('lavender','darkolivegreen3','gray90')}.
#' @param color.bord
#' The color for the border of the bars,
#' Default:
#' \code{c("mediumpurple4", "darkolivegreen4", "gray75")}.
#' @param color.letter
#' a 3-element vector of color names
#' for the names of
#' the items for (respectively)
#' significant positive, significant negative,
#' and non-significant.
#' Default is
#' \code{c("mediumpurple4",'darkolivegreen4','gray75')}
#' @param color4ns
#' (Default: \code{'gray75'})
#' color for the non-significant \code{bootratio}.
#' @param color4bar
#' (Default: \code{NULL}) a vector of color names
#' (same dimension as \code{bootratio}).
#' Needs to be either a name
#' from the set \code{colors()}
#' or in \code{hex} format.
#' @param plotnames
#' if code{TRUE} (default) write the names of the items.
#' @param main
#' (default is \code{NULL}) a title for the graph.
#' @param ylab
#' (default is \code{NULL}) a label for
#' the \eqn{y} axis (i.e., BR).
#' If \code{NULL} \code{ylab = 'Bootstrap ratios'}.
#' @param sortValues
#' (Default: \code{FALSE}) when \code{TRUE}
#' sort the values to plot.
#' @param signifOnly
#' (Default: \code{FALSE}) when \code{TRUE}
#' plot only the significant values.
#' @param angle.text
#' (Default: \code{90}, i.e., so slant)
#' the "slanting factor" for the text.
#' @param abbreviate_labels
#' (Default: \code{FALSE}).
#' @param make_labels_into_paragraphs
#' (Default: \code{FALSE}).
#' @param horizontal
#' (Default: \code{TRUE}) when \code{TRUE},
#' the plot is horizontal,
#' when \code{FALSE} the plot is vertical.
#' @param font.shrink
#' (\code{default = 1}): a proportion for
#'  how much the
#' non-significant font shrinks.
#' @param line.col
#' (\code{Default = 'red'})
#' the color for significance
#' for the critical value line.
#' @param line.type
#' The type of line for the critical line
#' (\code{Default = 2}, a dashed line).
#' @param line.size
#' (\code{Default = .5 }),
#' the thickness of the
#' critical line.
#' @param line.alpha
#' (\code{Default = .5 }),
#' the transparency of the
#' critical line (0 = all transparent, 1 = opaque).
#' @return
#' A \code{ggplot2} object containg the graph
#' (i.e., to be plotted with \code{print}).
#' @details
#' \code{PrettyBarPlot2} integrates \code{PrettyBarPlot}
#' and \code{PrettyBarPlotColors}.
#' @author
#' Vincent Guillemot & Hervé Abdi
#' @seealso
#' \code{\link{PrettyBarPlot}}
#' \code{\link{PrettyBarPlotColor}}
#' \code{\link{PrettyBarPlotColor4Q}}
#' @examples
#' toto <- 8*(.5 - runif(7))
#' names(toto) <- paste0('V', 1:7)
#' PrettyBarPlot2(toto)
#' @rdname PrettyBarPlot2
#' @importFrom stringr str_wrap
#' @importFrom gplots col2hex
#' @importFrom stats setNames
#' @export


PrettyBarPlot2 <- function(
                  bootratio,
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
                  horizontal = TRUE,
                  angle.text = if (horizontal) {
                             90
                           } else {
                             0
                           },
                abbreviate_labels = FALSE,
                make_labels_into_paragraphs = FALSE,
                font.shrink = 1,
                line.col = 'red',
                line.type = 2,
                line.size = .5,
                line.alpha = .5
 # label.size = 1 # a future feature?
) {

  if (is.vector(bootratio)) {
    if (!is.null(color4bar)){color4bar <- gplots::col2hex(color4bar)}
    if (is.null(names(bootratio))) {
      warning("The bootstrap ratio vector has no names, generating arbritary names.")
      names(bootratio) <- paste0("V", seq_along(bootratio))
    }
  } else if (class(bootratio) == "data.frame" |
             class(bootratio) == "matrix") {
    if (NCOL(bootratio) > 1)
      stop(
        "bootratio should be either a vector or a dataframe with one column or a matrix with one column"
      )
    tmp <- bootratio
    if (is.null(rownames(bootratio))) {
      warning("The bootstrap ratio vector has no names, generating arbritary names.")
      rownames(tmp) <- paste0("V", seq_along(bootratio[,1]))
    }
    bootratio <-
      setNames(drop(unlist(tmp)), rownames(tmp))
  } else {
    stop("bootratio should be a vector, a data frame or a matrix")
  }
  adapt_plot <- FALSE
  if (all(bootratio >= 0)) {
    adapt_plot <- TRUE
    plot_position <- "bottom"
    hjust <- 1
  } else if (all(bootratio <= 0)) {
    adapt_plot <- TRUE
    plot_position <- "top"
    hjust <- 0
  }

  if (signifOnly) {
    if (!is.null(color4bar)) {
      color4bar <- color4bar[abs(bootratio) > threshold]
    }
    bootratio <- bootratio[abs(bootratio) > threshold]
  }
  if (sortValues)
    bootratio <- sort(bootratio)
  if (is.null(ylim)) {
    # lemax <- round(max(abs(bootratio))) + 1
    expansion.factor = 1.1
    lemax <- max(0, max(bootratio) * expansion.factor)
    lemin <- min(0, min(bootratio) * expansion.factor)
    if (any(bootratio < 0) & any(bootratio > 0)) {
      ylim = c(lemin, lemax)
    } else if (all(bootratio >= 0)) {
      ylim <- c(0, lemax)
    } else {
      ylim <- c(lemin, 0)
    }
  }
  lesnoms <- names(bootratio)
  if (abbreviate_labels) {
    lesnoms <- abbreviate(lesnoms)
  }
  if (make_labels_into_paragraphs) {
    lesnoms <- stringr::str_wrap(string = lesnoms, width = 20)
  }

  nel <- length(bootratio)
  if (is.null(color4bar)) {
    lescouleurs <-
      lescouleurs.font <- lescouleurs.bord <- rep(color.bar[3], nel)
    lescouleurs[bootratio >  threshold] <- color.bar[1]
    lescouleurs[bootratio < -threshold] <- color.bar[2]
    lescouleurs.bord[bootratio >  threshold] <- color.bord[1]
    lescouleurs.bord[bootratio < -threshold] <- color.bord[2]
    lescouleurs.font[bootratio >  threshold] <- color.letter[1]
    lescouleurs.font[bootratio < -threshold] <- color.letter[2]
  } else {
    if (length(color4bar) > nel)
      stop("Too many colors for the bars.")
    if (length(color4bar) < nel)
      stop("Not enough colors for the bars.")
    if (any(!areColors(color4bar)))
      stop("Something is wrong in color4bar.")
    lescouleurs.font <- lescouleurs.bord <- color4bar
    lescouleurs <- lighten(color4bar, factor = 1.3)
    lescouleurs[abs(bootratio) < threshold] <- color4ns
    lescouleurs.bord[abs(bootratio) < threshold] <-
      color4ns # NA # HA
    # comment HA: the color font for ns seems to come from
    #   lescouleurs.bord rather than from lescouleurs.fonts. I am puzzled.
    lescouleurs.font[abs(bootratio) < threshold] <- color4ns
    # print(lescouleurs.font)
    # print(lescouleurs)
  }
  if (is.null(font.size))
    font.size <- 3
  lafont <- rep(font.size, nel)
  lafont[abs(bootratio) < threshold] = font.size * font.shrink # HA 90%
  LesNoms2Print <- lesnoms
  ID <- IDnum <- NULL # HA We need to avoid a strange error
  # building the package
  #print("lesnoms")
  #print(lesnoms)
  #print("LesNoms2Print")
  #print(LesNoms2Print)
  dat <- data.frame(
    IDnum = factor(seq_along(LesNoms2Print)),
    ID = LesNoms2Print,
    bootratio = bootratio,
    lescouleurs = lescouleurs,
    lescouleurs.font = lescouleurs.font,
    lescouleurs.bord = lescouleurs.bord,
    stringsAsFactors = FALSE
  )
  # define the "laLigneRouge" to be drawn only when there are ratios there
  # if (all(bootratio >= 0)){
  #  laLigneRouge =  geom_hline(yintercept = c(threshold),
  #                              col = col.line, alpha = 0.5, linetype = 2) }
  # if (all(bootratio <= 0)){
  #   laLigneRouge =  geom_hline(yintercept = c(-threshold),
  #                              col = col.line, alpha = 0.5, linetype = 2) }
  # if (any(bootratio >= 0) & any(bootratio <= 0)) {
  #   laLigneRouge = geom_hline(yintercept = c(threshold, -threshold),
  #                            col = col.line, alpha = 0.5, linetype = 2)
  #     # fix the lim problem. make sure that the lim is always printed. HA
  #   ylim = c(min(ylim[1],-threshold) , max(ylim[2],threshold))
  #   } # draw the red line
  #_____________________________________________________________________
  # The dimensions need to be better computed from the letter size
  #
  if (all(bootratio >= 0)) {
    yint <- c(threshold)   # fix the lim problem
    # ylim[2] <- max(ylim[2],  threshold)
    # ylim[1] <- min(ylim[1],  0)
  }
  if (all(bootratio <= 0)) {
    yint = -c(threshold)  # fix the lim problem
    ylim[1] <- min(ylim[1],-threshold)
    ylim[2] <- max(0, ylim[2])
  }
  if (any(bootratio >= 0) & any(bootratio <= 0)) {
    yint <-  c(threshold,-threshold)
    # fix the lim problem: Make sure that the lim is always printed. HA
    ylim = c(min(ylim[1], -threshold) , max(ylim[2], threshold))
  }
  laLigneRouge <- geom_hline(
    yintercept = yint,
    col = line.col,
    alpha = line.alpha,
    linetype = line.type,
    linewidth = line.size
  )
  #_____________________________________________________________________
  if (adapt_plot) {
    # commented by HA
    #  print("Inside adapt plot")
    #  print(lafont)
    p <- ggplot(dat, aes(
      x = factor(ID, levels = ID), # from Luke original x = ID
      y = bootratio,
      fill = IDnum,
      color = IDnum
    )) +
      laLigneRouge +
      geom_col() +
      geom_hline(yintercept = 0) +
      scale_fill_manual(values = lescouleurs) +
      scale_color_manual(values = lescouleurs.bord) +
      guides(fill = "none", color = "none") +
      labs(x = "", y = ylab) +
      ggtitle(main) +
      theme_bw() +
      theme(
        axis.line.y      = element_line(colour = "black", ),
        axis.ticks.x     = element_blank(),
        # axis.line.x      = element_line(colour = "black"),
        axis.text.x      = element_text(
          angle = angle.text, hjust = hjust,
          vjust = 0.5, color = lescouleurs.bord,
          size = rel(font.size/3)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank()
      ) +
      scale_x_discrete(position = plot_position) +
      ylim(ylim)
    if (horizontal) return(p)
    else return(p + coord_flip())
  }

  if (horizontal) {
    p <- ggplot(dat, aes(
      x = IDnum,
      y = bootratio,
      fill = IDnum,
      color = IDnum
    )) +
      laLigneRouge +
      #      geom_hline(yintercept = c(threshold, -threshold),
      #                 col="red", alpha=0.5, linetype=2) +
      geom_col() +
      geom_text(
        aes(x = IDnum, y = 0, label = ID),
        hjust = ifelse(bootratio >= 0, 1.1,-0.1),
        size = lafont,
        angle = angle.text
      ) +
      geom_hline(yintercept = 0) +
      scale_fill_manual(values = lescouleurs) +
      scale_color_manual(values = lescouleurs.bord) +
      guides(fill = "none", color = "none") +
      labs(x = "", y = ylab) +
      ggtitle(main) +
      theme_bw() +
      theme(
        axis.line.y      = element_line(colour = "black"),
        axis.title.x     = element_blank(),
        axis.text.x      = element_blank(),
        axis.ticks.x     = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank()
      ) +
      ylim(ylim)
    #_____________________________________________________________________
  } else {
    p <-
      ggplot(dat, aes(
        x = IDnum,
        y = bootratio,
        fill = IDnum,
        color = IDnum
      )) +
      laLigneRouge +
      #     geom_hline(yintercept = c(threshold, -threshold),
      #                 col="red", alpha=0.5, linetype=2) +
      geom_col() +
      geom_text(
        aes(x = IDnum, y = 0, label = ID),
        hjust = ifelse(bootratio >= 0, 1.1,-0.1),
        size = lafont,
        angle = angle.text
      ) +
      geom_hline(yintercept = 0) +
      scale_fill_manual(values = lescouleurs) +
      scale_color_manual(values = lescouleurs.bord) +
      guides(fill = "none", color = "none") +
      labs(x = "", y = ylab) +
      ggtitle(main) +
      theme_bw() +
      theme(
        axis.line.x      = element_line(colour = "black"),
        axis.title.y     = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank()
      ) +
      ylim(ylim)    + coord_flip()
  }
  return(p)
} # end of function PrettyBarPlot2
#_____________________________________________________________________
#_____________________________________________________________________
