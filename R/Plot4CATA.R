#
# This file contains the graphic routines for PTCA4CATA
# These ones are based on prettyPlots
# a second set baseD on ggplots is in developments
#
# Herve Abdi. August 7, 2016
# Current functions here:
# PrettyBarPlot()
# ShadesColor()
# PrettyBarPlotColor()
# GraphPTCABoot()
# Created August 05, 2016 by Hervé Abdi
# Documented with roxygen2
# Last Uptdate. August 07. HA

# *****************************************************************************
# The functions start below
# *****************************************************************************
# *****************************************************************************
# function PrettyBarPlot. Create plot à la Wires (MATLAB)

#' "pretty-plots" as bars the bootstrap ratios for a set of items
#' (e.g., observations or variables) analyzed with multivariate
#' methods such as PCA, CA, MCA, PLS etc.
#' Used to display the bootstrap ratios for CA/MCA/PCA/PLS.
#' Significant items are plotted in color, non-significant
#' items are plotted in gray.
#' @author Hervé Abdi
#' @param bootratio the bootstrap ratios (BR) to be plotted
#' (typically obtained from Boot4PTCA).
#' @param threshold critical.value The critical value for significance
#' (default = 2, which matches a $p < .05$ significance level)
#' |BR| < threshold are plotted in gray
#' @param ylim a 2-element vector giving min and max for the y-axis;
#'  when NULL (default) set to c(-max(abs(y)),max(abs(y)))
#' @param color.bar a 3-element vector of color names
#' for the bars for (respectively)
#' significant positive, significant negative, and non-significant.
#' Default is c('lavender','darkolivegreen3','gray90')
#' @param color.letter a 3-element vector of color names
#' for the names of the items for (respectively)
#' significant positive, significant negative, and non-significant.
#' Default is c("mediumpurple4",'darkolivegreen4','gray75')
#' @param  plotnames if TRUE (default) write the names of the items
#' @param main  (default is NULL) a title for the graph
#' @param ylab  (default is NULL) a label for the y axis (i.e., BR)
#' @return A list: 1) ylim min and max for y, 2) threshold
#' @import graphics
#' @export
PrettyBarPlot <- function(bootratio, threshold = 2, ylim = NULL,
                          color.bar = c('lavender','darkolivegreen3','gray90'),
                          color.letter = c("mediumpurple4",
                                           'darkolivegreen4','gray75'),
                          plotnames = TRUE, main = NULL, ylab = NULL
){
  # bootratio: the bootstrap ratios (BR) to be plotted
  # threshold: Treshold for significance, BR > threshold are plotted in gray
  # color.bar: colors for the
  #    positive-significant, negative significant, and ns values
  # color.letter: colors for the names
  # get the names to be plotted
  # plotnames: when TRUE, plot the names in the bars
  lesnoms = names(bootratio)
  if (is.null(ylim)){ # get the limits if they are not there
    lemax = round(max(abs(bootratio)))+1
    ylim = c(-lemax, lemax)
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
  graphics::barplot(bootratio,names="",space=.1,width=.9,col=lescouleurs,las=2,
          ylim = ylim,main=main,ylab = ylab,
          border=lescouleurs.font)
  graphics::abline(coef = c(0,0))
  graphics::abline(coef=c( threshold,0),col="red",lwd=.3)
  graphics::abline(coef=c(-threshold,0),col="red",lwd=.3)
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
#' ShadesColor Create create lighter and darker versions of a color
#'
#' ShadesColor Create create lighter and darker versions of a color
#'  used by PrettyBarPlotColor to create fake transparent colors
#' @author Hervé Abdi
#' @param AColor the color to use
#' @param jiffy  A small amount to make darker or clearer
#' default = 40
#' @return A list:
#' 1) TestColorDarker the darker
#' version of AColor
#' 2) TestColorLighter the lighter
#' version of AColor
#' # @examples #  twoColors <- ShadesColor('Red',50)
#' @export

ShadesColor <- function(AColor,jiffy=40){
  # create lighter and darker shades of a color
  # to be used for writing names in bars for PrettyColorBars
  # step 1 get it RGB
  rgb_Col = col2rgb(AColor)
  # Jiffy A small amount to make darker or clearer
  darker_color = apply(rgb_Col-jiffy,1,function(i){max(i,0)} )
  lighter_color = apply(rgb_Col+jiffy,1,function(i){min(i,255)} )
  TestColorDarker  = rgb(darker_color[1],
                         darker_color[2], darker_color[3],
                         maxColorValue=255)
  TestColorLighter = rgb(lighter_color[1],
                         lighter_color[2],
                         lighter_color[3],
                         maxColorValue=255)
  # return the color as a list
  return(c(TestColorDarker ,TestColorLighter))
}
# ******************************************************************************
# function PrettyBarPlotColor.

#' Create pretty bar plot
#'  with defined color for the sigificant columns
#'
#'  Create pretty bar plot
#'  with defined colors for the sigificant columns.
#'  Variation over prettyBarPlot
#' @author Hervé Abdi
#' @param bootratio the bootstrap ratios (BR) to be plotted
#' (typically obtained from Boot4PTCA).
#' @param threshold critical.value The critical value for significance
#' (default = 2, which matches a $p < .05$ significance level)
#' |BR| < threshold are plotted in gray
#' @param ylim a 2-element vector giving min and mac for y
#' if NULL (default) set to c(-abs(BR),abs(BR))
#' @param color4bar a vector of color names (same dimension as BR)
#' @param  color4ns  (default 'gray75') color
#' for the non-significant BR
#' @param  plotnames if TRUE (default) write the names of the items
#' @param main  (default is NULL) a title for the graph
#' @param ylab  (default is NULL) a label for the y axis (i.e., BR)
#' @return A list: 1) ylim min and max for y, 2) threshold
#' # @examples #   PrettyBarPlotColor(boot.ratio.test, color4BR)
#' @export
PrettyBarPlotColor <- function(bootratio,threshold=2,ylim=NULL,
                               color4bar,
                               color4ns = 'gray75',
                               plotnames = TRUE,
                               main = NULL, ylab = NULL
){
  # bootratio: the bootstrap ratios (BR) to be plotted
  # threshold: Treshold for significance,
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
  nel = length(bootratio) # how many observation to plot
  lescouleurs = color4bar
  lescouleurs[abs(bootratio) < threshold]  = color4ns
  ZeShades =  t(sapply(lescouleurs,ShadesColor,jiffy=40))
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
      text(x=lex,y=loc,labels=lesnoms[i],pos=lapos,
           col=lescouleurs.font[i],srt=90,font=lafont[i])
    }
  } # End for loop
  return(list(ylim=ylim,threshold=threshold))
} # End of function PrettyBarPlotColor
# *****************************************************************************
# *****************************************************************************
# ****************************   Boot PTCA graphs   ***************************
#'GraphPTCABoot plots the bootstrapped results of a CA
#'
#' plots the results of a CA
#' with boostrap computed  Ellipsoids
#' or Convex Hulls confidence interval
#' needs packages prettyGraphs (for the graph)
#' and the convex Hulls
#' car (for the ellipses)
#' @author Hervé Abdi
#' @param FS (factor scores) The coordinates
#' of the items to be plotted
#' @param FBoot The Bootstrap cube of factor scores
#' @param   axis1 (default is 1)  position in FS of the  horizontal axis
#' @param   axis2 (default is 2),  position in FS of the  vertical axis
#' @param item.colors (defaul is NULL),  a color matrix/vector for the items
#'  could be provided from a previous call to the graph routine
#' @param   ZeTitle (default is  'PTCA-Bootstrap') a Title for the plot
#' @param   constraints (default is NULL) the dimensions of the plot as a list
#' giving the constraints
#' = list(minx=number1,miny=number2, maxx=number3,maxy=number4)
#' often provided from a previous plot
#' @param nude  (default is FALSE),  when TRUE do not plot names
#' @param   Ctr (default is NULL), if TRUE  have the dots for the item
#'  proportional to their contributions to the plane
#' @param   lwd  (default is  3.5)  line width for the ellipse
#' (from dataEllipse)
#' @param   ellipses (default is TRUE)  when FALSE plot convex hulls
#' @param   fill  (default is TRUE)  fill in the ellipses with lighter colors
#' @param   fill.alpha (default is .27)  alpha-transparency for fill
#' @param   percentage (default is 0.95)  critical value for the CIs
#' @param   dev.new  (default is TRUE)  when TRUE
#' create a new window for plotting
#' @param   new.plot  when  TRUE (default)  Clean the devise
#' @param   cex  (default is 1)   size of the dots
#' @param   text.cex (default is  .8) # size of text
#' (to avoid conflicts with cex)
#' @param   pos  (default is 2)  position of the text
#'  can be a scalar or a vector
#' @return PTCA.out the output from prettyPlot
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
# --------------------------------------------------------------------
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
# 2. functions originally in file 'prettyHist4Julien.R'
# --------------------------------------------------------------------
# From Derek's function for pretty distributions
# October 17, 2013
#' Plot a sampling distribution
#' along with a value of a criterion
#'
#'  Plot a sampling distribution
#'  (typically derived from a permutation test as
#'  performed, e.g., by InPosition::epCA.inference.battery).
#'  Plot the empirical critical value and position the
#'  value of the criterion.
#'  along with a value of a criterion.
#'  User is _required_ to provide a distribution
#'  and an observed value.
#' @author Derek Beaton & Hervé Abdi
#' @param distribution the empirical distribution
#' (e.g., from a permutation test).
#' @param observed the observaed value of the criterion
#' @param show.observed if TRUE (default) plot the value of the criterion.
#' @param observed.col color for the criterion (default is "mediumorchid4").
#' @param xlim default is c(-1.2,1.2): value for the x-axis.
#' The default
#' works well for CA or MCA but is unlikely to work for other techniques
#' (such as, e.g., PCA, PLS, or Hellinger analysis).
#' @param breaks (default is 10) How many bins
#' for the histogram of the distribution.
#' @param border (default is "white") color for the border of the graph.
#' @param  distr.col default is ("darkseagreen") color for the distribution.
#' @param main (default "") the main title.
#' @param xlab (default "") the x-axis label.
#' @param ylab (default "") the y-axis label.
#' @param counts if (TRUE default) shows the number of samples
#' if FALSE shows a probability.
#' @param cutoffs [default is c(0.025,0.975)],
#' the p-values (default for alpha = .05).
#' @param show.cutoffs if TRUE (default) show the critical value(s).
#' @param cutoff.col (default is "firebrick3") color for the critical value.
#' @param gray.distr if TRUE (TRUE) distribution is plotted in gray.
#' @param tail.dir if "+" (default) the critical value
#' is on the right of the
#' distribution, otherwise it is on the left.
#' @return h the histogram of the distribution
#' @import stats
#' @export
##User is _required_ to provide a distribution
##  and an observed value
prettyHist <- function(distribution,observed,
                       show.observed=TRUE,observed.col="mediumorchid4",
                       xlim=c(-1.2,1.2),breaks=10,border="white",
                       distr.col="darkseagreen",main="",xlab="",ylab="",
                       counts=TRUE,cutoffs=c(0.025,0.975),
                       show.cutoffs=TRUE,cutoff.col="firebrick3",
                       gray.distr=TRUE,tail.dir="+"){
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

    tail.locs <- stats::quantile(distribution,prob=cutoffs)
    abline(v=tail.locs,col=cutoff.col,lty=2,lwd=2)
    #lapply(tail.locs,abline,lwd=3,col=cutoff.col,lty=2)
    #abline(v=observed,lwd=3,col=observed.col)
    #text(observed, max(h$counts),
    # "Observed value" , pos = 2 , cex = 0.75, col=observed.col)
  }

  return(h)
}
# --------------------------------------------------------------------
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
## -------------------------------------------------------------------
#' Plot the result of Cochran's Q for the columns of a CATA
#' table of data
#'
#' #' Plot the result of Cochran's Q for the columns of a CATA
#' table of data along with the critical values (user provided)
#'
#'  Plot the result of Cochran's Q for the columns of a CATA
#' table of data along with the critical values (user provided)
#' This version is temporary and will likely be rewriten
#' @author Hervé Abdi
#' @param  bootratio  the Q values to be plotted
#' @param threshold (no default)
#' Treshold for significance, Q > threshold are plotted in gray
#' @param ylim the lower and upper limits for the y-axis
#' if NULL (default) set to c(-max(abs(y)) , max(abs(y)) )
#' @param color4bar the colors for the items
#' (default is 'darkorchid4')
#' @param color4ns the color for a non significant item
#' (default is 'gray75')
#' @param PrintSignificantOnly  when TRUE (default)
#' print only the significant items
#' @param plotnames when TRUE (default),
#'  plot the names in the bars
#' @param main a title for the plot (default is NULL)
#' @param ylab a label for the y axis (default is NULL)
#' # @examples # PrettyBarPlotColor4Q(bootratio,threshold)
#' @export
PrettyBarPlotColor4Q <- function(bootratio,threshold,
                                 ylim=NULL,
                                 color4bar =  'darkorchid4',
                                 color4ns = 'gray75',
                                 PrintSignificantOnly=TRUE,
                                 plotnames = TRUE,
                                 main = NULL, ylab = NULL
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
# --------------------------------------------------------------------
# --------------------------------------------------------------------
#' plot the scree for the eigen values
#' of an SVD based multivariate analysis
#'
#' Plot the scree for the eigen-values
#' of an SVD based multivariate analysis.
#' Note that the function can recompute the
#' eigen-values when a percentage is given.
#' For example  ExPosition does not return all ev
#'        but only the requested one. but return all percentage
#'        so if max.ev is specified, it is used to recompute
#'        all eigenvalues
#'  If provide wit probabilities, PlotScree will
#'  color differently the "significant" eigenvalues
#' @author Hervé Abdi
#' @param ev the eigen values to plot. no default
#' @param p.ev the probabilities associated to the
#' eigen-values
#' @param max.ev the max eigen-value
#'        needed because ExPosition does not return all ev
#'        but only the requested one. but return all tau
#'        so if max.ev is specified, it is used to recompute
#'        all eigen-values
#' @param alpha threshold for significance. Default = .05
#' @param col.ns color for the non significant
#' ev. Default is Green
#' @param col.sig  color for significant eigen-values.
#' Default is Violet
#' @param title a title for the graph
#' default is "Explained Variance per Dimension"
#' # @examples  # PlotScree(ev)
#' @export
PlotScree <- function(ev,p.ev=NULL,max.ev=NULL,
                      alpha=.05,
                      col.ns = '#006D2C',col.sig='#54278F',
                      title = "Explained Variance per Dimension"
){
  # percentage of inertia
  val.tau = (100*ev/sum(ev))
  Top.y = ceiling(max(val.tau)*.1)*10
  # if ev is already a percentage convert it back
  if (!is.null(max.ev)){ev = ev*(max.ev/ev[1])}
  #
  par(mar=c(5,6,4,4))
  # plot.window(xlim = c(0, length(val.tau)+5),
  #         ylim = c(0,Top.y),asp = .6)
  plot(x = seq(1,length(val.tau)),y=val.tau,xlab='Dimensions',
       ylab = 'Percentage of Explained Variance',
       main = title,
       type = 'l', col = col.ns, lwd= 1,
       xlim = c(1, length(val.tau)),
       ylim = c(0,Top.y)
  )
  points(x = seq(1,length(val.tau)),y=val.tau,
         pch=16,  cex=1, col = col.ns, lwd= 2.5
  )
  if (!is.null(p.ev)){# plot the significant vp if exist
    # Plot the significant factors
    signi.vp = which(p.ev < alpha)
    lines(x = seq(1,length(signi.vp)),y=val.tau[signi.vp],
          type = 'l', col = col.sig, lwd= 1.5
    )
    points(x = seq(1,length(signi.vp)),y=val.tau[signi.vp],
           pch=16,  cex=1.5, col = col.sig, lwd= 3.5)
  } # end of plot significant vp
  par(new = TRUE)
  par(mar=c(5,6,4,4)+.5)
  le.max.vp = Top.y*(ev[1]/val.tau[1])
  plot(ev, ann=FALSE,axes=FALSE,type="n",#line=3,
       ylim = c(0,le.max.vp))
  mtext("Inertia Extracted by the Components",side=4,line=3)
  axis(4)
} # end of function

# --------------------------------------------------------------------
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
# --------------------------------------------------------------------
#===============================================================================
# X and Y Labels
#
# a function for labels
#'createLabel creates x and y labels for ggplots2 scatterplots
#'
#'createLabel creates x and y labels for ggplots2 scatterplots
#'(e.g., correspondence analysis or principal component analysis).
#'@param resCA the results of epCA or epPCA
#'@param zeAxis the number of the axis (no default)
#'@param axisName the name for the axes (default = 'Dimension')
#'@author Herve Abdi
#'@export
#'
createLabel <- function(resCA, zeAxis,
                        axisName = 'Dimension'){
  lambda <- round(resCA$ExPosition.Data$eigs[zeAxis],3)
  tau <- round(resCA$ExPosition.Data$t[zeAxis],0)
  genLabel <-
    bquote(.(axisName)*.(zeAxis)*.(
      '. ')~~lambda==.(lambda)*.~~tau==.(tau)*.('%'))
  return(genLabel)
} # End of function createLabel
# function createxyLabels
#'createxyLabels creates x and y labels for ggplots2 scaterplots
#'
#'createxyLabels creates x and y labels for ggplots2 scaterplots
#'(e.g., correspondence analysis or principal component analysis).
#'@param resCA the results of epCA or epPCA
#'@param x_axis the number of the x axis; default = 1
#'@param y_axis the number of the y axis; default = 2
#'@param axisName the name for the axes (default = 'Dimension')
#'@author Herve Abdi
#'@export
#'
createxyLabels <- function(resCA,
                           x_axis = 1,
                           y_axis = 2,
                           axisName = 'Dimension'){
  xyLabels = labs(x = createLabel(resCA, x_axis,axisName),
                  y = createLabel(resCA, y_axis,axisName) )
  return(xyLabels)
}
#====================================================================

# --------------------------------------------------------------------
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
# --------------------------------------------------------------------
#======================================================================

