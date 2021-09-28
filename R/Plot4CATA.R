#_____________________________________________________________________
#_____________________________________________________________________
# Entéte ----
# This file contains the graphic routines
# for PTCA4CATA
# These ones are based on prettyPlots
# a second (newer) set based on ggplot2 is
# in development.
# Preamble ----
# Hervé Abdi. August 7, 2016
# Current functions here:
# PrettyBarPlot()
# ShadesColor()
# PrettyBarPlotColor()
# GraphPTCABoot()
# PlotScree()
# createlabel()
# createxyLabels
# createLabel.gen
# createxyLabels.gen
# Created August 05, 2016 by Hervé Abdi
# Documented with roxygen2
# Last Uptdate. February 04, 2018. HA
# Small changes March 07. HA.
# Problem with strange error when building the package on R
# change September 23, 2018. HA
# changed November  4, 2018 HA (from Ju-Chi)
# changed September 28, 2021 HA
#   (correct strange bug when all p values
#    are equal to alpha in PlotScree)
#_____________________________________________________________________
#_____________________________________________________________________

# ********************************************************************
# The functions start below
# ********************************************************************
# ********************************************************************
# function PrettyBarPlot. Create plot à la Wires (MATLAB)
# PrettyBarPlot ----
#' creates bar plots for a series of variables.
#' It is used for plotting bootstrap ratios or contributions from
#' principal component analysis or related methods
#' (e.g. correspondence analysis).
#'
#' \code{PrettyBarPlot}: Creates bar plots for a set of items
#' (e.g., observations or variables) analyzed with multivariate
#' methods such as PCA, CA, MCA, PLS, etc.
#'
#'  \code{PrettyBarPlot} is
#' used to display the bootstrap ratios or contributions plots
#' for CA/MCA/PCA/PLS, etc.
#' Significant or important items are plotted in color,
#' non-significant
#' items are plotted in gray.
#' @author Hervé Abdi
#' @param bootratio the bootstrap ratios (BR) to be plotted
#' (e.g.,  obtained from \code{Boot4PTCA}).
#' @param threshold
#' The critical value for significance
#' (default = 2, a value that matches a \eqn{p < }.05
#' significance level)
#' \code{|BR|} < threshold are plotted in gray
#' @param ylim a 2-element vector giving \code{min}
#'  and \code{max} for the y-axis;
#'  when NULL (default) set to
#'  \code{c(min(0, min(y)), max(0,max(y)))}.
#' @param color.bar a 3-element vector of color names
#' for the bars for (respectively)
#' significant positive, significant negative,
#' and non-significant.
#' Default is \code{c('lavender','darkolivegreen3','gray90')}.
#' @param color.letter a 3-element vector of color names
#' for the names of the items for (respectively)
#' significant positive, significant negative, and non-significant.
#' Default is \code{c("mediumpurple4",'darkolivegreen4','gray75')}.
#' @param  plotnames if \code{TRUE} (default) write the names of the items
#' @param main  (default is \code{NULL}) a title for the graph.
#' @param ylab  (default is \code{NULL})
#' a label for the y-axis (i.e., BR).
#' @return A list: 1) \code{ylim}: min and max for y,
#' and 2) threshold.
#' @import graphics
#' @details
#' \code{PrettyBarPlot2},
#' is a newer \code{ggplot2}-based version that can be used
#' \emph{in lieu} of \code{PrettyBarPlot}.
#' @seealso \code{\link{PrettyBarPlot2}}
#' @author Hervé Abdi, Derek Beaton, and Vincent Guillemot.
#' @export
PrettyBarPlot <- function(bootratio, threshold = 2, ylim = NULL,
            color.bar = c('lavender','darkolivegreen3','gray90'),
            color.letter = c("mediumpurple4",
                             'darkolivegreen4','gray75'),
            plotnames = TRUE, main = NULL, ylab = NULL
){
  # bootratio: the bootstrap ratios (BR) to be plotted
  # threshold: Threshold for significance, BR > threshold are plotted in gray
  # color.bar: colors for the
  #    positive-significant, negative significant, and ns values
  # color.letter: colors for the names
  # get the names to be plotted
  # plotnames: when TRUE, plot the names in the bars
  lesnoms = names(bootratio)
  if (is.null(ylim)){ # get the limits if they are not there
    # lemax = round(max(abs(bootratio)))+1
    expansion.factor = 1.1
    lemax <- max(0, max(bootratio) * expansion.factor)
    lemin <- min(0, min(bootratio) * expansion.factor)
    ylim = c(lemin, lemax)
  }
  # get the colors for the bars
  nel = length(bootratio) # how many observations to plot
  lescouleurs = rep(color.bar[3],nel) # default is gray
  lescouleurs[bootratio >  threshold]  = color.bar[1]
  lescouleurs[bootratio < -threshold]  = color.bar[2]
  # get the colors for the labels
  lescouleurs.font = rep(color.letter[3],nel) # default is gray
  lescouleurs.font[bootratio >  threshold] = color.letter[1]
  lescouleurs.font[bootratio < -threshold] = color.letter[2]
  lafont = rep(2,nel) # default font is bold for significant
  lafont[abs(bootratio) < threshold] = 1

  # Plot
  graphics::barplot(bootratio,names = "",
                    space = .1, width = .9,
                    col = lescouleurs,las=2,
          ylim = ylim,main = main, ylab = ylab,
          border = lescouleurs.font)
  graphics::abline(coef = c(0,0))
  graphics::abline(coef = c( threshold,0),col="red",lwd=.3)
  graphics::abline(coef = c(-threshold,0),col="red",lwd=.3)
  # now go for an ugly loop
  for (i in 1:nel){
    if (sign(bootratio[i]) == -1){
      # where are the names positioned compare to 0
      # needs to be fixed in a smarter way
      loc = -2.2   # NB 0 values will cross the line
      lapos = 2
      lex = i  # .45 #
    } else
    {lapos =  4
    loc = 	  2.2 # 1
    lex = i   - 1.4
    }
    # font = 2 for bold
    if (plotnames){#When plotnames is TRUE we plot the names
      graphics::text(x=lex,y=loc,labels=lesnoms[i],pos=lapos,
           col=lescouleurs.font[i],srt=90,font=lafont[i])
    }
  } # End for loop
  return(list(ylim=ylim,threshold=threshold))
} # End of function PrettyBarPlot
# ******************************************************************************
# ShadesColor ----
#' Create lighter and darker versions of a color.
#'
#' \code{ShadesColor}: Create lighter and darker
#' versions of a color
#'  used by \code{PrettyBarPlotColor}
#'  to create faked transparent colors.
#'
#' @author Hervé Abdi
#' @param aColor the color to use
#' @param jiffy  A small amount to make darker or clearer
#' default = 40.
#' @return A list:
#' 1) \code{testColorDarker}: the darker
#' version of \code{aColor};
#' 2) \code{testColorLighter} the lighter
#' version of \code{aColor}.
#' @examples
#' twoColors <- ShadesColor('Red',50)
#' @export

ShadesColor <- function(aColor,jiffy=40){
  # create lighter and darker shades of a color
  # to be used for writing names in bars for PrettyColorBars
  # step 1 get it RGB
  rgb_Col = col2rgb(aColor)
  # Jiffy A small amount to make darker or clearer
  darker_color = apply(rgb_Col-jiffy,1,function(i){max(i,0)} )
  lighter_color = apply(rgb_Col+jiffy,1,function(i){min(i,255)} )
  testColorDarker  = rgb(darker_color[1],
                         darker_color[2], darker_color[3],
                         maxColorValue=255)
  testColorLighter = rgb(lighter_color[1],
                         lighter_color[2],
                         lighter_color[3],
                         maxColorValue=255)
  # return the color as a list
  return(c(testColorDarker, testColorLighter))
}
# ******************************************************************************
# function PrettyBarPlotColor.
# PrettyBarPlotColor. ----
#' \code{PrettyBarPlotColor}: create pretty bar plots
#'  with defined colors for the sigificant columns
#'
#'  \code{PrettyBarPlotColor}: Create pretty bar plots
#'  with defined colors for the sigificant columns.
#'  NB \code{PrettyBarPlotColor} is a variation over \code{prettyBarPlot}.
#' @author Hervé Abdi
#' @param bootratio the bootstrap ratios (BR) to be plotted
#' (typically obtained from Boot4PTCA).
#' @param threshold critical.value The critical value for significance
#' (default = 2, which matches a $p < .05$ significance level)
#' |BR| < threshold are plotted in gray.
#' @param ylim a 2-element vector giving min and mac for y
#' if \code{NULL} (default) set to \code{c(-abs(BR),abs(BR))}.
#' @param color4bar a vector of color names (same dimension as BR)
#' @param  color4ns  (default is \code{'gray75'}) color
#' for the non-significant BRs.
#' @param  plotnames if \code{TRUE} (default)
#' write the names of the items.
#' @param main  (default is \code{NULL}) a title for the graph.
#' @param ylab  (default is \code{NULL})
#' a label for the y axis (i.e., BR).
#' @return A list: 1) \code{ylim} min and max for y,
#' and 2) threshold.
#' @details
#' \code{PrettyBarPlot2},
#' is a newer \code{ggplot2}-based plotting function
#' that can be used
#' \emph{in lieu} of \code{PrettyBarPlotColor}.
#' @seealso PrettyBarPlot2
#' @author Vincnet Guillemeot, Hervé Abdi.
#' # @examples #   PrettyBarPlotColor(boot.ratio.test, color4BR)
#' @export
PrettyBarPlotColor <- function(bootratio,threshold=2,ylim=NULL,
                               color4bar,
                               color4ns = 'gray75',
                               plotnames = TRUE,
                               main = NULL, ylab = NULL
){
  # bootratio: the bootstrap ratios (BR)
  # to be plotted
  # threshold: Threshold for significance,
  #  BR > threshold are plotted in gray
  # color4bar: the colors for the items
  # color4ns: the color for non-significant items
  #
  # get the names to be plotted
  # plotnames: when TRUE, plot the names in the bars
  lesnoms = names(bootratio)
  if (is.null(ylim)){ # get the limits if they are not there
    lemax = round(max(abs(bootratio)))+1
    ylim = c(-lemax, lemax)
  }
  # get the colors for the bars
  nel = length(bootratio) # how many observations to plot
  if (is.factor(color4bar)) color4bar <- as.character(color4bar)
  # color4bar factor creates a strange error later oon
  lescouleurs = color4bar
  lescouleurs[abs(bootratio) < threshold]  = color4ns
  ZeShades =  t(sapply(lescouleurs, ShadesColor, jiffy=40))
  Lighter = ZeShades[,2]
  #lighter = lescouleurs
  Darker = ZeShades[,1]
  # get the colors for the labels
  lescouleurs.font =  Darker
  lafont = rep(2,nel) # default font is bold for significant
  lafont[abs(bootratio) < threshold] = 1

  # Plot
  barplot(bootratio,names="",space=.1,width=.9,col=Lighter,las=2,
          ylim = ylim,main=main,ylab = ylab,
          border=Darker)
  abline(coef = c(0,0))
  abline(coef=c( threshold,0),col="red",lwd=.3)
  abline(coef=c(-threshold,0),col="red",lwd=.3)
  # now go for an ugly loop
  for (i in 1:nel){
    if (sign(bootratio[i]) == -1){
      # where are the names positioned compared to 0
      # needs to be fixed in a smarter way
      loc = - 2.2   # NB 0 values will cross the line
      # with lapos = 2 this means words start at -2
      lapos = 2
      lex = i +  -.45 # - .45
    } else
    {lapos =  4
    loc = 	2.2
    lex = i - .7
    }
    # font = 2 for bold
    if (plotnames){#When plotnames is TRUE we plot the names
      text(x = lex, y = loc, labels = lesnoms[i], pos = lapos,
           col = lescouleurs.font[i], srt = 90,font = lafont[i])
    }
  } # End for loop
  return(list(ylim=ylim,threshold=threshold))
} # End of function PrettyBarPlotColor
# *****************************************************************************
# *****************************************************************************
# ****************************   Boot PTCA graphs   ***************************
# GraphPTCABoot ----
#'GraphPTCABoot plots the bootstrapped results of a CA
#'
#' \code{GraphPTCABoot} plots the results of a CA
#' with bootstrap computed  Ellipsoids
#' or Convex Hulls confidence interval.
#'  \code{GraphPTCABoot}
#' needs packages \code{prettyGraphs} (for the graph)
#' and the convex Hulls
#' and \code{car} (for the ellipses).
#' @author Hervé Abdi
#' @param FS (factor scores) The coordinates
#' of the items to be plotted.
#' @param FBoot The Bootstrap cube of factor scores.
#' @param axis1 (default is 1)  position in \code{FS}
#' of the  horizontal axis
#' @param axis2 (default is 2),  position in \code{FS}
#'  of the  vertical axis
#' @param item.colors (default is \code{NULL}),
#' a color matrix/vector for the items
#'  (could be provided from a previous call to the graph routine).
#' @param ZeTitle (default is  \code{'PTCA-Bootstrap'})
#' a Title for the plot
#' @param constraints (default is \code{NULL})
#' the dimensions of the plot as a list
#' giving the constraints
#' = \code{list(minx=number1,miny=number2,
#' maxx=number3,maxy=number4)}.
#' Often provided from a previous plot.
#' @param nude  (default is \code{FALSE}),
#' when \code{TRUE} do not plot names.
#' @param Ctr (default is \code{NULL}),
#' if \code{TRUE}  have the dots for the item
#'  proportional to their contributions to the plane.
#' @param lwd  (default is  3.5)  line width for the ellipse
#' (from dataEllipse).
#' @param   ellipses (default is \code{TRUE})
#' when \code{TRUE} plot ellipses,
#' when \code{FALSE} plot convex hulls,
#' @param   fill  (default is \code{TRUE})
#' when \code{TRUE}
#' fill in the ellipses with lighter colors.
#' @param fill.alpha (default is .27)  alpha-transparency
#' for filling
#' (see parameter \code{fill}) ellipses/convex-hulls.
#' @param   percentage (default is 0.95)
#' critical value for the CIs
#' @param   dev.new  (default is \code{TRUE})  when \code{TRUE}
#' create a new window for plotting.
#' @param   new.plot  when  \code{TRUE} (default)
#' Clean the device.
#' @param   cex  (default is 1)   size of the dots
#' @param   text.cex (default is  .8) # size of text
#' (to avoid conflicts with cex)
#' @param   pos  (default is 2)  position of the text
#'  can be a scalar or a vector
#' @return PTCA.out: the output from prettyPlot
#' @import prettyGraphs
#' @import car
#' @export
  # NamesOnZeSide=FALSE# currently inactive,
  # will plot the names on the side for easy editing
  GraphPTCABoot <-function(# GraphPTCABoot Plot the results of a boostrap
    # With Ellipsoids or Convex Hulls
    # needs packadges prettyPlot (for the graph) car (for the ellipses)
    FS,  # The coordinates of the items to be plotted
    FBoot, # The Bootstrap cube of factor scores
    axis1=1, axis2=2,  # horizontal and vertical axes
    item.colors=NULL,  # a color matrix/vector for the items
    # could be provided from a previous call to the graph routine
    ZeTitle= 'PTCA-Bootstrap', # Title for the plot
    constraints=NULL, # the dimensions of the plot as a list
    # constraints
    # = list(minx=number1,miny=number2, maxx=number3,maxy=number4)
    # often provided from a previous plot
    nude = FALSE, # when TRUE do not plot names
    Ctr=NULL, # have the dots proportional to the contributions
    lwd = 3.5, # line width for the ellipse (from dataEllipse)
    ellipses=TRUE,  # when FALSE plot convex hulls
    fill = TRUE,  # fill in the ellipses with lighter colors
    fill.alpha = .27, # transparency for fill
    percentage=0.95, # ciritical value for the CIs
    dev.new = TRUE, # create a new window for plotting
    new.plot = TRUE, # Clean the devise
    cex = 1 , # size of the dots
    text.cex = .8, # size of text (to avoid conflicts with cex)
    pos = 2 #, # position of the text
    # NamesOnZeSide=FALSE# currently inactive,
    # will plot the names on the side for easy editing
  ){
    if (is.null(item.colors)){
      item.design <- diag(dim(FS)[1])
      item.colors <- as.matrix(prettyGraphsColors(
      )[createColorVectorsByDesign(item.design)$oc])
    }
    if(is.vector(item.colors)){item.colors = as.matrix(item.colors)}
    # When item.colors is a vector make it a matrix
    if(is.null(constraints)){
      real.minimum <- min(c(FS,FBoot))
      real.maximum <- max(c(FS,FBoot))
      real.value <- max(c(abs(real.minimum),abs(real.maximum)))
      #set up a constraints list
      constraints <- list(minx=-real.value,
                          maxx=real.value,miny=-real.value,maxy=real.value)
    }

    PTCA.Out <- prettyGraphs::prettyPlot(FS,
                 constraints=constraints,
                 col=item.colors,
                 main=ZeTitle,
                 x_axis=axis1,
                 y_axis=axis2,
                 contributionCircles = FALSE,
                 display_names=!nude,
                 text.cex = text.cex,
                 pos = pos,
                 dev.new = dev.new,
                 new.plot = new.plot
      )
    for(i in 1:dim(FS)[1]){
      if(ellipses){
        car::dataEllipse(x=FBoot[i,axis1,],
                    y=FBoot[i,axis2,], add=TRUE,
                    levels=percentage,
                    plot.points=FALSE,
                    center.pch = FALSE,
                    col = item.colors[i,],
                    lwd = lwd, fill=fill,fill.alpha=fill.alpha)
      }else{
        ##this intentionally overlays a black background hull.
        prettyGraphs::peeledHull(t(FBoot[i,,]),x_axis=axis1,y_axis=axis2,
                   percentage=percentage,lwd=lwd)
        if(lwd<3){
          color.lwd <- 1
        }else{
          color.lwd <- lwd-2
        }
        prettyGraphs::peeledHull(t(FBoot[i,,]),x_axis=axis1,y_axis=axis2,
                   percentage=percentage,col=item.colors[i,],lwd=color.lwd)
      }
    }
    return(PTCA.Out)   # return the graphs
  }
# *******************************************************************************
#_____________________________________________________________________
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
# 2. functions originally in file 'prettyHist4Julien.R'
#_____________________________________________________________________
# From Derek's function for pretty distributions
# October 17, 2013
# prettyHist ----
# Current version: 05/27/2021.
#' Plot a sampling distribution
#' along with a value of a criterion (e.g., for significance).
#'
#'  \code{prettyHist}: plots a sampling distribution
#'  (typically derived from a permutation test as
#'  performed, e.g., by
#'  \code{InPosition::epCA.inference.battery}).
#'  \code{pretyHist} plots the empirical critical value
#'  and position the
#'  value of the criterion.
#'  along with a value of a criterion.
#'  User is \emph{required} to provide a distribution
#'  and an observed value.
#' @author Derek Beaton & Hervé Abdi
#' @param distribution the empirical distribution
#' (e.g., from a permutation test).
#' @param observed the observed value of the criterion.
#' @param show.observed if \code{TRUE} (default)
#'  plot the value of the criterion.
#' @param observed.col a color for the criterion
#' (default is \code{"mediumorchid4"}).
#' @param xlim default is \code{c(-1.2, 1.2)}:
#' value for the \eqn{x}-axis.
#' The default
#' works well for CA or MCA but is unlikely
#' to work for other techniques
#' (such as, e.g., PCA, PLS, or Hellinger analysis).
#' @param breaks (default is \code{10}) How many bins
#' for the histogram of the distribution.
#' @param border (default is \code{"white"}) color
#' for the border of the graph.
#' @param  distr.col (default is (\code{"darksgreen"}) the
#' color for the distribution.
#' @param main (default "") the main title.
#' @param xlab (default "") the \eqn{x}-axis label.
#' @param ylab (default "") the \eqn{y}-axis label.
#' @param counts if \code{TRUE}
#' (default) shows the number of samples,
#' if \code{FALSE} shows a probability.
#' @param cutoffs [default is \code{c(0.025,0.975)}],
#' the \eqn{p}-values (default for alpha = .05).
#' @param show.cutoffs if \eqn{TRUE} (default)
#' show the critical value(s).
#' @param cutoff.col (default is \code{"firebrick3"})
#' the color
#' for the critical value.
#' @param gray.distr if \code{TRUE} (default) distribution
#'  is plotted in gray.
#' @param tail.dir if \code{"+"} (default) the critical value
#' is on the right of the
#' distribution, otherwise it is on the left.
#' @return h the histogram of the distribution
#' @importFrom  stats quantile
#' @export
##User is _required_ to provide a distribution
##  and an observed value
prettyHist <- function(distribution, observed,
                       show.observed = TRUE,
                       observed.col = "mediumorchid4",
                       xlim = c(-1.2,1.2),
                       breaks = 10,
                       border = "white",
                       distr.col = "darkseagreen",
                       main = "", xlab = "", ylab = "",
                       counts = TRUE,
                       cutoffs = c(0.025,0.975),
                       show.cutoffs = TRUE,
                       cutoff.col = "firebrick3",
                       gray.distr = TRUE,
                       tail.dir = "+"){
  # function starts here
  #	print(length(cutoffs)!=1 || length(cutoffs)!=2)
  if(!(length(cutoffs)!=1 || length(cutoffs)!=2)){
    print(length(cutoffs))
    stop("Too many tails!")
  }
  tail.locs <- quantile(distribution,prob=cutoffs)
  h <- hist(distribution, breaks = breaks, plot=FALSE)
  if(gray.distr & length(cutoffs)==1){
    if(tail.dir=="+"){
      plot(h, col = ifelse(h$mid<sort(distribution)[
        ceiling(length(distribution)*cutoffs)],'gray',distr.col),
           border = border, main = main,
           xlab = xlab, ylab = ylab,
           xlim = xlim, ylim = c(-1,max(h$counts)*1.05))
    }else{
      plot(h, col = ifelse(h$mid>sort(distribution,decreasing=TRUE)[
        ceiling(length(distribution)*cutoffs)],'gray',distr.col),
           border = border, main = main,
           xlab = xlab, ylab = ylab,
           xlim = xlim, ylim = c(-1,max(h$counts)*1.05))
    }
  }else if(gray.distr & length(cutoffs)==2){
    plot(h, col = ifelse(h$mid>sort(distribution,
                                    decreasing=TRUE)[
                                     ceiling(length(distribution)*cutoffs[2]
                                     )]&h$mid<sort(distribution)[
                                     ceiling(length(distribution
                                     )*cutoffs[2])],"gray",distr.col),
         border = border, main = main,
         xlab = xlab, ylab = ylab,
         xlim = xlim, ylim = c(-1,max(h$counts)*1.05))
  } else {
    plot(h, col = distr.col,
         border = border, main = main,
         xlab = xlab, ylab = ylab,
         xlim = xlim, ylim = c(-1,max(h$counts)*1.05))
  }
  if(counts){
    text(h$mids , h$counts , h$counts ,  col = "black" , pos = 3 , cex = 0.7)
  }
  if(show.observed){

    # abline(v=observed,lwd=3,col=observed.col)
    arrows(observed,max(h$counts),observed,0,
           lwd=2.49,col=observed.col,angle=10)
    if(observed < 0){
      text(observed, max(h$counts),
           paste("Observed value =",round(observed,digits=4)) ,
           pos = 2 , cex = 0.75, col=observed.col)
    }else{
      text(observed, max(h$counts),
           paste("Observed value =",round(observed,digits=4)) ,
           pos = 4 , cex = 0.75, col=observed.col)
    }

  }
  if(show.cutoffs){

    tail.locs <- stats::quantile(distribution, prob = cutoffs)
    abline(v = tail.locs, col = cutoff.col, lty = 2,lwd = 2)
    #lapply(tail.locs,abline,lwd=3,col=cutoff.col,lty=2)
    #abline(v=observed,lwd=3,col=observed.col)
    #text(observed, max(h$counts),
    # "Observed value" , pos = 2 , cex = 0.75, col=observed.col)
  }

  invisible(h)
}
#_____________________________________________________________________
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
#_____________________________________________________________________
# PrettyBarPlotColor4Q ----
#' \code{PrettyBarPlotColor4Q}
#' Plot the result of Cochran's \eqn{Q} for the columns of a CATA
#' table of data.
#'
#' \code{PrettyBarPlotColor4Q}:
#' Plot the result of Cochran's \eqn{Q} for the columns
#' of a \code{CATA}
#' table of data along with the critical value (user provided)
#' NB: This version is temporary and will likely be rewriten
#' (see \code{PrettybarPlot2} for a \code{ggplot2}-based
#' equivalent function).
#' @author Hervé Abdi
#' @param  bootratio  the \eqn{Q} values to be plotted
#' @param threshold (no default)
#' Treshold for significance, Q > threshold are plotted in gray
#' @param ylim the lower and upper limits for the y-axis
#' if NULL (default) set to c(-max(abs(y)) , max(abs(y)) )
#' @param color4bar the colors for the items
#' (default is 'darkorchid4')
#' @param color4ns the color for a non significant item
#' (default is 'gray75')
#' @param PrintSignificantOnly  when \code{TRUE} (default)
#' print only the significant items
#' @param plotnames when \code{TRUE} (default),
#'  plot the names in the bars
#' @param main a title for the plot (default is \code{NULL})
#' @param ylab a label for the \eqn{y} axis (default is \code{NULL})
#' # @examples # PrettyBarPlotColor4Q(bootratio,threshold)
#' @seealso PrettyBarPlot2
#' @export
PrettyBarPlotColor4Q <- function(bootratio,threshold,
                                 ylim=NULL,
                                 color4bar =  'darkorchid4',
                                 color4ns = 'gray75',
                                 PrintSignificantOnly=TRUE,
                                 plotnames = TRUE,
                                 main = NULL,
                                 ylab = NULL
){
  # bootratio: the bootstrap ratios (BR) to be plotted
  # threshold: Treshold for significance, BR > threshold are plotted in gray
  # color4bar: the colors for item
  # color4ns: the color for a non significant item
  # PrintSignificantOnly = TRUE print only the significant items
  # get the names to be plotted
  # plotnames: when TRUE, plot the names in the bars
  lesnoms = names(bootratio)
  if (is.null(ylim)){ # get the limits if they are not there
    lemax = round(max(abs(bootratio)))+1
    ylim = c(0, lemax)
  }
  # get the colors for the bars
  nel = length(bootratio) # how many observations to plot
  lescouleurs = color4bar
  lescouleurs[abs(bootratio) < threshold]  = color4ns
  ZeShades =  t(sapply(lescouleurs,ShadesColor,jiffy=40))
  Lighter = ZeShades[,2]
  # lighter = lescouleurs
  Darker = ZeShades[,1]
  # get the colors for the labels
  lescouleurs.font =  Darker
  lafont = rep(2,nel) # default font is bold for significant
  lafont[abs(bootratio) < threshold] = 1
  LesNoms2Print = lesnoms
  if  (PrintSignificantOnly==TRUE){
    LesNoms2Print[bootratio < threshold] <- ""
  }
  # Plot
  barplot(bootratio,names = LesNoms2Print,
          space=.1, width = .9, col = Lighter, las=2,
          ylim = ylim,main = main,ylab = ylab,cex.names = .6,
          border=Darker)
  abline(coef = c(0,0))
  abline(coef=c( threshold,0),col="red",lwd=.7)
  abline(coef=c(-threshold,0),col="red",lwd=.7)
  # now go for an ugly loop
  for (i in 1:nel){
    if (sign(bootratio[i]) == 1){
      # where are the names positioned compared to 0
      # needs to be fixed in a smarter way
      loc = - 2.2   # NB 0 values will cross the line
      # with lapos = 2 this means words start at -2
      lapos = 2
      lex = i +  -.45 # - .45
    } else
    {lapos =  4
    loc = 	2.2
    lex = i - .7
    }
    # font = 2 for bold
    if (plotnames){# When plotnames is TRUE we plot the names
      text(x = lex, y = loc, labels = lesnoms[i], pos = lapos,
           col = lescouleurs.font[i], srt =90 , font = lafont[i])
    }
  } # End for loop
  return(list(ylim=ylim,threshold=threshold))
}
#_____________________________________________________________________
#_____________________________________________________________________
# PlotScree ----
#' plot the scree for the eigenvalues
#' of an SVD based multivariate analysis.
#'
#' \code{PlotScree}: Plot the scree for the eigenvalues
#' of an SVD-based multivariate analysis.
#' Note that the function can recompute the
#' eigen-values when a percentage is given.
#' For example  \code{ExPosition} does not return all ev
#'        but only the requested one. but return all percentage
#'        so if max.ev is specified, it is used to recompute
#'        all eigenvalues.
#'  By default \code{PlotScree}
#'  will not plot the line corresponding to
#'  the average inertia (i.e., Kaiser criterion).
#'  If provided with probabilities,
#'  \code{PlotScree} will
#'  color differently the "significant"
#'  eigenvalues.
#' @author Hervé Abdi with help
#' from Derek Beaton and Ju-Chi Yu.
#' @param ev the eigenvalues to plot.
#' No default.
#' @param p.ev the probabilities
#' associated to the
#' eigen-values, (default = \code{NULL}).
#' @param max.ev
#' the max eigenvalue
#'        needed because \code{ExPosition}
#'        does not always return all
#'        eigenvalues
#'        but sometimes only the requested ones;
#'        however \code{ExPosition} always returns
#'        all percentages i.e., \code{tau}),
#'        so if \code{max.ev} is specified,
#'        it is used to recompute
#'        all eigenvalues.
#' @param alpha
#' threshold for significance
#'   \code{Default = .05}).
#' @param col.ns color for the non significant
#' eigenvalues. Default is \code{'Green'}.
#' @param col.sig  color for significant
#' eigen-values.
#' Default is \code{'Violet'}.
#' @param title a title for the graph
#' default is
#' \code{"Explained Variance per Dimension"}.
#' @param xlab The names of the dimensions
#' (default \code{'Dimensions '}).
#' @param plotKaiser  when \code{TRUE}
#' plot a line corresponding to the average inertia
#' (Kaiser criterion); do not plot when
#' \code{FALSE} (default).
#' @param color4Kaiser
#' color for Kaiser's
#' line
#' (default is \code{'darkorchid4'})
#' @param lwd4Kaiser \code{lwd} value
#' (i.e., width)
#' for Kaiser's criterion line.
#' (default is \code{'2.5'})
#' # @examples  # PlotScree(ev)
#' @export
PlotScree <- function(ev,
            p.ev = NULL,
            max.ev = NULL,
            alpha = .05,
            col.ns = '#006D2C',
            col.sig = '#54278F',
title = "Explained Variance per Dimension",
            xlab = 'Dimensions',
            plotKaiser = FALSE,
            color4Kaiser = 'darkorchid4',
            lwd4Kaiser = 2.5
){# fix the strange problem when all
  #  p.ev are larger than alpha
  if (!is.null(p.ev)){
    if (all(p.ev > alpha)) p.ev <- NULL
  }
  # percentage of inertia
  val.tau = (100*ev / sum(ev))
  Top.y = ceiling(max(val.tau) * .1) * 10
  # if ev is already a percentage convert it back
  if (!is.null(max.ev)){ev = ev * (max.ev / ev[1])}
  #
  par(mar = c(5,6,4,4))
  # plot.window(xlim = c(0, length(val.tau)+5),
  #         ylim = c(0,Top.y),asp = .6)
  plot(x = seq(1, length(val.tau)), y = val.tau,
       xlab = xlab,
       ylab = 'Percentage of Explained Variance',
       main = title,
       type = 'l', col = col.ns, lwd = 1,
       xlim = c(1, length(val.tau)),
       ylim = c(0,Top.y)
  )
  points(x = seq(1,length(val.tau)),y = val.tau,
         pch = 16,  cex = 1, col = col.ns, lwd = 2.5
  )
  if (!is.null(p.ev)){# plot the significant vp if exist
    # Plot the significant factors
    signi.vp = which(p.ev <= alpha)
    # These are the lines Ju-Chi changed ####
    lines(x = seq(1, max(signi.vp)),
                  y = val.tau[1 : max(signi.vp)],
          type = "l", col = col.sig, lwd = 1.5)
    points(x = signi.vp, y = val.tau[signi.vp],
        pch = 16, cex = 1.5, col = col.sig, lwd = 3.5)
    #______________________________________________
  } # end of plot significant vp
  par(new = TRUE)
  par(mar = c(5,6,4,4) + .5)
  le.max.vp = Top.y*(ev[1]/val.tau[1])
  plot(ev, ann = FALSE,axes = FALSE,type = "n",#line=3,
       ylim = c(0,le.max.vp))
  if (plotKaiser){
  abline(h = sum(ev)/length(ev),
          col = color4Kaiser, lwd = lwd4Kaiser)
  }
  mtext("Inertia Extracted by the Components", side = 4, line = 3)
  axis(4)
} # end of function

#_____________________________________________________________________
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
#_____________________________________________________________________
#_____________________________________________________________________
# X and Y Labels
#
# a function for labels
#'  Creates\eqn{x} and \eqn{y} labels for ggplots2 scatterplots
#'
#' \code{createLabel} creates \eqn{x} and \eqn{y}
#' labels for \code{ggplots2} scatterplots
#'(e.g., correspondence analysis or principal component analysis).
#'@param resCA the results of \code{epCA} or \code{epPCA}.
#'@param zeAxis the number of the axis (no default)
#'@param axisName the name for the axes
#'(default = \code{'Dimension'})
#'@author Hervé Abdi
#'@export
#'
createLabel <- function(resCA, zeAxis,
                        axisName = 'Dimension '
                      ){
  lambda <- round(resCA$ExPosition.Data$eigs[zeAxis],3)
  tau <- round(resCA$ExPosition.Data$t[zeAxis],0)
  genLabel <- createLabel.gen(zeAxis = zeAxis,
                              lambda = lambda,tau = tau,
                              axisName = axisName
  )
  genLabel <-
  return(genLabel)
} # End of function createLabel
# function createxyLabels
#'creates x and y labels for
#'\code{ggplots2} scaterplots.
#'
#'\code{createxyLabels} creates x and y labels for
#'\code{ggplots2} scaterplots
#'(e.g., correspondence analysis
#'or principal component analysis).
#'@param resCA the results of epCA or epPCA.
#'@param x_axis the number of the x axis; default = 1.
#'@param y_axis the number of the y axis; default = 2.
#'@param axisName the name for the axes
#'(default = \code{'Dimension'})
#'@author Hervé Abdi
#'@export
#'
createxyLabels <- function(resCA,
                           x_axis = 1,
                           y_axis = 2,
                           axisName = 'Dimension '){
  xyLabels = labs(x = createLabel(resCA, x_axis,axisName),
                  y = createLabel(resCA, y_axis,axisName) )
  return(xyLabels)
}
#_____________________________________________________________________
#_____________________________________________________________________
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
#_____________________________________________________________________
#_____________________________________________________________________
# X and Y Labels generic functions
#
# a function for labels
#' Creates \eqn{x} and \eqn{y} labels for \code{ggplots2} scatterplots
#'
#'\code{createLabel.gen}
#'creates \eqn{x} and \eqn{y} labels for \code{ggplots2} scatterplots
#'(e.g., correspondence analysis or principal component analysis).
#' Compared to \code{createLabels},   \code{createLabels.gen}
#' does not require the results from \code{ExPosition}
#' and so can be used for any \code{ggplot2}-based scatterplot.
#'
#'@param zeAxis the number of the axis (no default)
#'@param lambda the eigen-value
#' associated with this dimension
#'@param tau the percentage of variance
#'associated with this dimension
#'@param axisName the name for the axes
#'(default = \code{'Dimension'})
#'@author Herve Abdi
#'@export
#'
createLabel.gen <- function(zeAxis, lambda,tau,
                          axisName = 'Dimension '
){
  lambda <- round(lambda,3)
  tau    <- round(tau,0)
  genLabel <-
    bquote(.(axisName)*.(zeAxis)*.(
      '. ')~~lambda==.(lambda)*.~~tau==.(tau)*.('%'))
  return(genLabel)
} # End of function createLabel
# function createxyLabels
# createxyLabels ----
#' Creates \code{x} and \code{y} labels for ggplots2 scaterplots
#'
#'\code{createxyLabels.gen}:
#'creates \eqn{x} and \eqn{y} labels for
#'\code{ggplots2} scaterplots
#'(e.g., correspondence analysis
#'or principal component analysis)
#'such as, for example,
#'the plots created by \code{createFactorMap}.
#' Compared to \code{createxylabel}, \code{createxylabel.gen}
#' does not require the results from \code{ExPosition}.
#'@param x_axis the number of the \eqn{x} axis; default = 1.
#'@param y_axis the number of the \eqn{y} axis; default = 2.
#'@param lambda a vector of eigenvalues
#'(should have as many entries
#' as \code{max(axis_1, axis_2)}).
#'@param tau a vector of percentage of explained variance
#'(should have as many entries
#' as \code{max(axis_1, axis_2)}).
#'@param axisName the name for the axes
#'(default = \code{'Dimension '}).
#'@author Hervé Abdi
#'@export
#'
createxyLabels.gen <- function(
                           x_axis = 1,
                           y_axis = 2,
                           lambda,
                           tau,
                           axisName = 'Dimension '){
  xyLabels = labs(x = createLabel.gen(zeAxis = x_axis,
                                      lambda = lambda[x_axis],
                                      tau = tau[x_axis],axisName),
                  y = createLabel.gen(zeAxis = y_axis,
                                       lambda = lambda[y_axis],
                                       tau = tau[y_axis],axisName))
  return(xyLabels)
}
#_____________________________________________________________________
#_____________________________________________________________________
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
#_____________________________________________________________________
#_____________________________________________________________________

