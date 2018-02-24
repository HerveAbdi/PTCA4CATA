#---------------------------------------------------------------------
# Filename: SaveAllGraphsIntoPttx
# Contains saveGraph2pptx:
# A nifty routine
# save all the graphs present in the Global Environment
# Created February 20, 2018. by Hervé Abdi
# Current version February 23, 2018
#
# This file contains the functions
# saveGraph2pptx() (includes the internal function sauveImage() )
# print.savePptx  (print function
# for objects of class savePptx created by saveGraph2pptx() )
#
#*********************************************************************

#---------------------------------------------------------------------
#*********************************************************************
#' \code{saveGraph2pptx}:  saves all the graphics
#' from the Global Environment into a powerpoint file.
#'
#' \code{saveGraph2pptx}: a nifty function that
#' saves all the graphics
#' from the Global Environment (i.e., \code{.GlobalEnv})
#' into a powerpoint file. Requires packages \code{rsv}
#' and \code{officer}. Graphics saved are those created
#' with \code{recordPlot()} or \code{ggplot2}.
#' @param file2Save.pptx the name of the powerpoint
#' file for saving the graphs. If this file already exists,
#' the old file is renamed and a warning message is printed
#' in the console.
#' @param title main title of the powerpoint file.
#' \code{default:} "All Graphics for Current Analysis. As of: " +
#' date.
#' @param addGraphNames when TRUE: use the name of
#' the graph as its title for the powerpoint slide
#' (default is \code{FALSE}).
#' @return a list (of class \code{"savePptx"})
#' with  \code{listOfsavedGraphs} (the list of the graph objects
#' saved)  and
#' \code{nameOfSavingFile4pptx} (name of the files where
#' the graphics are saved).
#' Note: to print one of the graphs from
#' \code{nameOfSavingFile4pptx}, use
#' \code{print(get())}. For example,
#' to print the first graph of the list
#' saved as \code{listOfGraph} use
#' \code{print(get(listOfGraph$listOfsavedGraphs[[1]]))}.
#' @author Herve Abdi
#' @examples \dontrun{
#' toto <- saveGraph2pptx("myFile.pptx", "Pretty Graphs of the Day")
#' }
#' @importFrom officer add_slide ph_with_text read_pptx
#' @importFrom rvg ph_with_vg
#' @export


saveGraph2pptx <- function(file2Save.pptx,
                           title = NULL,
                           addGraphNames = FALSE
                           ){
  # First a private function
  # A helper function to save recorded plots and ggplots
  # function in development to save the graphs in officer format
  #*******************************************************************
  #* First a local function: sauveImage
  sauveImage <- function(pptxName, # the name of the officer object
                         graph, # the graph to file
                         title = "" # The title of the graph
  ){
    # test what type of graph this is
    typeG  <- class(graph)[1]
    if ( !(typeG %in% c("recordedplot", "gg") )){
      stop("Unknown type of graph. Only recordedplot and gg are supported")
    }
    # A new Slide with text and a graph saved as en editble rvg graph
    pptxName <- officer::add_slide(pptxName,
                                   layout = "Title and Content",
                                   master = "Office Theme")
    pptxName <- officer::ph_with_text(pptxName, type = 'title',
                                      str =  title )# The title
    pptxName <- rvg::ph_with_vg(pptxName, code = print(graph),
                                type = "body") # The ggplot2 picture
  } # End of sauveImage
  #-------------------------------------------------------------------
  laDate = substr(as.POSIXlt(Sys.time()),1,10)
  # Make default title
  if (is.null(title)) {
    title = paste0('All Graphics for Current Analysis. As of: ',
                   laDate)
  }
  # Make default file name
  pptx.type = 'pptx'
  if(tools::file_ext(file2Save.pptx) != pptx.type){
    file2Save.pptx <- paste0(file2Save.pptx,'.',pptx.type)
  }
  if (file.exists(file2Save.pptx)){# if file already exists: rename it
    LaDate = substr(as.POSIXlt(Sys.time()),1,10)
    OldFilename = sub(paste0('[.]', pptx.type),
                      paste0('-',LaDate,'.',pptx.type),file2Save.pptx)
    file.rename(from = file2Save.pptx, to = OldFilename)
    warning(paste0("File: ",file2Save.pptx,' already exists.\n',
                   ' Oldfile has been renamed: ', OldFilename),
            call. = FALSE)
  }

  # create a General Title
  # open the file
  doc <- officer::read_pptx() # Create the pptx file
  # Create title slide
  doc <- add_slide(doc, layout = "Title Only", master = "Office Theme")
  doc <- ph_with_text(doc, type = 'title',
                      str =  title )
  #-------------------------------------------------------------------
  #-------------------------------------------------------------------
  # Save  in a powerpoint
  #   all the graphs created by either recordPlot or ggplots
  listOfGraphs <- list()
  k = 0
  alist <- ls(.GlobalEnv, sorted = TRUE)
  # list all the objects in the Global environment
  nObj <- length(alist)
  for (i in 1:nObj){
    isGraph <- class(eval(as.symbol(alist[[i]])))[[1]]
    if ((isGraph == 'gg') | (isGraph == "recordedplot") ){
      anImage <- get(alist[[i]], pos = -1)
      if (addGraphNames) {aTitle <- alist[[i]]} else {aTitle = ""}
      suppressMessages(
        sauveImage(doc, anImage , title = aTitle )
      )
      k = k + 1
      listOfGraphs[[k]] <- alist[[i]]
    }
  }
  #-------------------------------------------------------------------
  # Save the powerpoint Presentation
  suppressMessages(
    print(doc, target = file2Save.pptx )
  )
  # et voila
  #
  #-------------------------------------------------------------------
  return.list <- structure(
    list(
      listOfsavedGraphs = listOfGraphs,
      nameOfSavingFile4pptx = file2Save.pptx
    ),  class = "savePptx")


  return(return.list) #  Return the name of the file
} # End of function saveGraph2pptx
#*********************************************************************
# ********************************************************************
# ********************************************************************
#' Change the print function for object of class savePptx
#'
#'  Change the print function for objects of class \code{savePptx}.
#'
#' @param x a list: object of class \code{savePptx},
#'   output of function: \code{saveGraph2pptx}.
#' @param ... everything else for the function
#' @author Herve Abdi
#' @export
print.savePptx <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n List of Saved Graphics by function saveGraph2pptx \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$listOfsavedGraphs     ", "The list of the names of the saved graphs")
  cat("\n                       ", "  NB To print a graph use print(get())")
  cat("\n$nameOfSavingFile4pptx ", "The powerpoint file where the graphs were saved")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.bootRatios
#---------------------------------------------------------------------

#---------------------------------------------------------------------
# Test the function here
#res.save.pptx <- saveGraph2pptx(file2Save.pptx = 'toto.pptx',
#                                title = 'Distatis Test')
#---------------------------------------------------------------------
