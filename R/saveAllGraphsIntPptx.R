#_________________________________________________
# Filename: SaveAllGraphsIntoPttx.
# Currently part of PTCA4CATA package.
# Current version November 04, 2025.
#*************************************************
# Contains saveGraph2pptx:
# A nifty routine:
# saves all the graphs present
# in the Global Environment.
# First version:
# created:     February 20, 2018 by Hervé Abdi.
#  stabilized: March    09, 2020, HA + VG
# New Version November 04, 2025, HA + VG.
#
# Fixing a problem with ggplot2.
# Old version identifies ggplots graphs as "gg"
#     New version uses "ggplot2::ggplot"
# This file contains the functions
# saveGraph2pptx()
# (which includes the internal function
# sauveImage() )
# print.savePptx  (print function
# for objects of class savePptx
# created by saveGraph2pptx() )
#
#*************************************************

#_________________________________________________
#*************************************************
#* Preamble saveGraph2pptx ----
#'  saves all the graphics present in
#'  the Global Environment into a PowerPoint file.
#'
#' \code{saveGraph2pptx}: a nifty function that
#' saves all the graphics
#' present in the Global Environment
#' (i.e., \code{.GlobalEnv})
#' into a PowerPoint file.
#' Requires packages \code{rvg}
#' and \code{officer}.
#' Graphics saved are only those created
#' with \code{recordPlot()} or \code{ggplot2}.
#' @param file2Save.pptx
#' the name of the PowerPoint
#' file for saving the graphs.
#' If this file already exists,
#' the old file is renamed and
#' a warning message is printed
#' in the console.
#' @param title main title of the PowerPoint file.
#' \code{default:}
#' "\code{All Graphics
#' for Current Analysis. As of:}" +
#' \code{date}.
#' @param addGraphNames when
#' \code{TRUE}: use the name of
#' the graph as its title
#' for the PowerPoint slide
#' (default is \code{FALSE}).
#' @return a list (of class \code{"savePptx"})
#' with  \code{listOfsavedGraphs}
#' (the list of the graph objects
#' saved)  and
#' \code{nameOfSavingFile4pptx}
#' (name of the files where
#' the graphics are saved).
#' Note: to print one of the graphs from
#' \code{nameOfSavingFile4pptx}, use
#' \code{print(get())}.
#' For example,
#' to print the first graph
#' of the list
#' saved as \code{listOfGraph} use
#' \code{print(get(listOfGraph$listOfsavedGraphs[[1]]))}.
#' @author Hervé Abdi
#' @examples \dontrun{
#' toto <- saveGraph2pptx("myFile.pptx",
#' "Pretty Graphs of the Day")
#' }
#' @import rvg officer
# #' # Below. Old @importFrom with specific import
# #' # does not work anymore as of 03-03-2020. HA
# ## ' @importFrom officer add_slide ph_with_text read_pptx
# ##  @importFrom rvg ph_with_vg
# ## ' @importFrom officer ph_with
#' @export


saveGraph2pptx <- function(file2Save.pptx,
                           title = NULL,
                           addGraphNames = FALSE
){
  # First a private function
  # A helper function to save
  # recorded plots and ggplots.
  # as powerpoint graphs in officer format
  # Function still in development.
  #***********************************************
  #* First a local function: sauveImage
  #_______________________________________________
  sauveImage <- function(pptxName,
                         # the name of the officer object
                         graph, # the graph to save
                         title = "" # The title of the graph
  ){
    # test what type of graph this is
    typeG  <- class(graph)[1]
    if ( !(typeG %in% c("recordedplot",
                        "ggplot2::ggplot") )){
      stop("\nUnknown type of graph.\n",
           "Only 'recordedplot' and 'ggplot2::ggplot'\n",
           "are supported")
    }
    # A new Slide with
    # text and a graph saved
    # as en editable rvg graph
    pptxName <- officer::add_slide(pptxName,
                                   layout = "Title and Content",
                                   master = "Office Theme")
    pptxName <- officer::ph_with(pptxName,
                                 title,
                                 officer::ph_location_type(
                                   type = 'title'))# The title
    # Note that old code ph_with_vg
    # is now deprecated
    # pptxName <- rvg::ph_with_vg(pptxName,
    # code = print(graph),
    #  type = "body") # The ggplot2 picture

    pptxName <- officer::ph_with(pptxName,
                                 rvg::dml(print(graph)),
                                 officer::ph_location_type(
                                   type = 'body'))
    # The ggplot2 picture
  } # End of sauveImage
  #_______________________________________________
  laDate = substr(as.POSIXlt(Sys.time()),1,10)
  # Make default title
  if (is.null(title)) {
    title = paste0(
      'All Graphics for Current Analysis. As of: ',
      laDate)
  }
  # Make default file name
  pptx.type = 'pptx'
  if(tools::file_ext(file2Save.pptx) != pptx.type){
    file2Save.pptx <- paste0(file2Save.pptx,'.',pptx.type)
  }
  if (file.exists(file2Save.pptx)){
    # if file already exists: rename it
    LaDate = substr(as.POSIXlt(Sys.time()),1,10)
    OldFilename = sub(paste0('[.]', pptx.type),
                      paste0('-',LaDate,'.',pptx.type),
                      file2Save.pptx)
    file.rename(from = file2Save.pptx,
                to = OldFilename)
    warning(paste0("File: ",file2Save.pptx,'
                    already exists.\n',
                   ' Oldfile has been renamed: ',
                   OldFilename),
            call. = FALSE)
  }

  # create a General Title
  # open the file
  doc <- officer::read_pptx() # Create the pptx file
  # Create title slide
  doc <- officer::add_slide(doc,
                            layout = "Title Only",
                            master = "Office Theme")
  #doc <- ph_with_text(doc, type = 'title',
  #                    str =  title )
  doc <- officer::ph_with(doc,
                          value = title ,
                          location = officer::ph_location_type(
                            type = 'title')
  )

  #
  #________________________________________________
  #_______________________________________________
  # Save  in a powerpoint
  #   all the graphs created
  #   by either recordPlot or ggplots
  listOfGraphs <- list()
  k = 0
  alist <- ls(.GlobalEnv, sorted = TRUE)
  # list all the objects in the Global environment
  nObj <- length(alist)
  for (i in 1:nObj){
    isGraph <- class(eval(as.symbol(alist[[i]])))[[1]]
    if ((isGraph == 'ggplot2::ggplot') |
        (isGraph == "recordedplot") ){
      anImage <- get(alist[[i]], pos = -1)
      if (addGraphNames) {aTitle <- alist[[i]]} else {aTitle = ""}
      suppressMessages(
        sauveImage(doc, anImage , title = aTitle )
      )
      k = k + 1
      listOfGraphs[[k]] <- alist[[i]]
    }
  }
  #_______________________________________________
  # Save the powerpoint Presentation
  suppressMessages(
    print(doc, target = file2Save.pptx )
  )
  # et voila
  #
  #_______________________________________________
  return.list <- structure(
    list(
      listOfsavedGraphs = listOfGraphs,
      nameOfSavingFile4pptx = file2Save.pptx
    ),  class = "savePptx")


  return(return.list) #  Return the name of the file
} # End of function saveGraph2pptx ----
#*************************************************
# ************************************************
# ************************************************
#' Change the print function
#' for object of class savePptx
#'
#'  Change the print function
#'  for objects of class \code{savePptx}.
#'
#' @param x a list: object of class
#' \code{savePptx},
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
  cat("\n$listOfsavedGraphs     ",
      "The list of the names of the saved graphs")
  cat("\n                       ",
      "  NB To print a graph use print(get())")
  cat("\n$nameOfSavingFile4pptx ",
      "The powerpoint file where the graphs were saved")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.savePptx
#_________________________________________________

#_________________________________________________
# Test the function here
#res.save.pptx <-
# saveGraph2pptx(file2Save.pptx = 'toto.pptx',
#                    title = 'Distatis Test')
#_________________________________________________
