% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readCATACubeData.R
\name{read.xls.CATA}
\alias{read.xls.CATA}
\title{Reads the CATA data from an Excel File and creates
a "Brick" of data
to be analyzed by PTCA4CATA}
\usage{
read.xls.CATA(
  path2file,
  sheet2read,
  orderProducts = TRUE,
  threshold4cleaning = 0
)
}
\arguments{
\item{path2file}{the name of the \code{xls} file with the data.}

\item{sheet2read}{the name of the sheet in the excel file.}

\item{orderProducts}{if \code{TRUE} (default) alphabetically
order the products.}

\item{threshold4cleaning}{the cleaning threshold:
The columns whose total
is smaller than \code{threshold4cleaning} are eliminated from
\code{CleanedContingencyTable}.
Default: \code{threshold4cleaning = 0} (i.e.,
columns with a zero sum are eliminated).
To keep even the columns with a zero sum, use
\code{threshold4cleaning = -1}}
}
\value{
A list with \code{CATA.Brick}
a "product by adjective by judge" Brick of 0/1 data
(1 if Judge chose adjective for product, 0 if not);
 \code{ContingencyTable}:
A "product by adjective" contingency table;
 \code{CleanedContingencyTable}:
  A "product by adjective" cleaned contingency table
  (columns whose sum is equal or below threshold are dropped).
}
\description{
\code{read.xls.CATA}
reads the CATA data from an Excel File and creates a Brick of Data
and a contingency table. The contingency table can be analyzed
by correspondence analysis or Hellinger analysis.
The cube of data is needed to perform correct
cross-validation methods such as
permutations
tests,  bootstrap confidence intervals, and
bootstrap ratios.
With \eqn{I} products,  \eqn{J} adjectives and  \eqn{K} judges
The data in the excel file are organized as:
        row 1 column 1 = name of Judge 1
          row 1 columns  (2 to  \eqn{J} + 1) name of the adjectives
        row 2 column 1 = name of product 1.
          row 2 column (2 to  \eqn{J} + 1) 0/1 answers
            of Judge 1 to the  \eqn{J} adjectives for product 1
        ....
        row  \eqn{I}+1 column 1 = name of product  \eqn{I}
           row 2 column (2 to  \eqn{J} + 1) 0/1 answers
        row  \eqn{I} + 2 column 1 = name of product 1.
          row 2 column (2 to  \eqn{J} + 1) 0/1 answers
            of Judge 2 to the  \eqn{J} adjectives for product 1
        ....
        And so on till the last judge.
        See the help for the excel
        file \code{OrangeJuiceCATARawData.xlsx}
        for an example of how the excel file should
        be organized.
 @section Implementation
Current version uses Wickham's \code{readxl} package
and so does not need \code{rJava}.
Current version: is July 11, 2017.
}
\author{
Herve Abdi
}
