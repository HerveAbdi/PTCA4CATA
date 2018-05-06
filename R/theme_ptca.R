# function theme_ptca()
# Hervé Abdi from original from VIncent Guillemot
# April 11 / 2018.
#_______________________________________________________________________________
# Preambule theme_ptca ----
#' @title  A ggplot2 theme that matches the defaults
#' of the factorial figures in PTCA4CATA
#'
#' @description \code{theme_ptca}
#' A ggplot2 theme that matches the defaults
#' of the factorial figures in PTCA4CATA.
#'
#' @param mire (Default = \code{TRUE}) when \code{TRUE}
#' plot the factorial axis at values \code{x0} and \code{y0}.
#' @param x0 (Default = 0) the X-axis origin. Relevant only
#' when \code{mire = TRUE}.
#' @param y0 (Default = 0) the Y-axis origin.
#' Relevant only
#' when \code{mire = TRUE}.
#' @author Vincent Guillemot
#' @import ggplot2
#' @examples
#'\dontrun{
#' x = c(5, 8, 1, 70)
#' y = c(10, 12, 20, 50)
#' truc <-  ggplot(as.data.frame(cbind(x,y)),
#'                  aes(x = x, y = y)) +
#'                     geom_point(size=2, shape=19) + theme_ptca()
#' }
#' @export
theme_ptca <- function(mire = TRUE, x0 = 0, y0 = 0) {
  col.fill <- adjustcolor('lavender', alpha.f = .2)
  col.bkg <- 'darkorchid'
  col.axes <- adjustcolor('darkorchid', alpha.f = .2)
  width.axes <- 1.1
  theme_list <- list(
    theme_grey() %+replace%
      theme(
        legend.key   = element_rect(fill = NA, color = NA),
        legend.title = element_text(color = 'purple4', face = 'bold'),
        legend.text  = element_text(color = 'purple4'),
        axis.text    = element_text(color = 'purple4'),
        axis.ticks   = element_line(color = 'purple4'),
        axis.title   = element_text(color = 'purple4'),
        panel.background = element_rect(fill = col.fill,
                                          color = col.bkg),
        aspect.ratio = 1
      ))
  if (mire) theme_list <- append(theme_list, list(
    geom_vline(xintercept = x0, color = col.axes, size = width.axes),
    geom_hline(yintercept = y0, color = col.axes, size = width.axes)))
  return(theme_list)
} # end of theme_ptca() ----
# ______________________________________________________________________________
