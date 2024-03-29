% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot4CATA.R
\name{PlotScree}
\alias{PlotScree}
\title{plot the scree for the eigenvalues
of an SVD based multivariate analysis.}
\usage{
PlotScree(
  ev,
  p.ev = NULL,
  max.ev = NULL,
  alpha = 0.05,
  col.ns = "#006D2C",
  col.sig = "#54278F",
  title = "Explained Variance per Dimension",
  xlab = "Dimensions",
  plotKaiser = FALSE,
  color4Kaiser = "darkorchid4",
  lwd4Kaiser = 2.5
)
}
\arguments{
\item{ev}{the eigenvalues to plot.
No default.}

\item{p.ev}{the probabilities
associated to the
eigen-values, (default = \code{NULL}).}

\item{max.ev}{the max eigenvalue
needed because \code{ExPosition}
does not always return all
eigenvalues
but sometimes only the requested ones;
however \code{ExPosition} always returns
all percentages i.e., \code{tau}),
so if \code{max.ev} is specified,
it is used to recompute
all eigenvalues.}

\item{alpha}{threshold for significance
\code{Default = .05}).}

\item{col.ns}{color for the non significant
eigenvalues. Default is \code{'Green'}.}

\item{col.sig}{color for significant
eigen-values.
Default is \code{'Violet'}.}

\item{title}{a title for the graph
default is
\code{"Explained Variance per Dimension"}.}

\item{xlab}{The names of the dimensions
(default \code{'Dimensions '}).}

\item{plotKaiser}{when \code{TRUE}
plot a line corresponding to the average inertia
(Kaiser criterion); do not plot when
\code{FALSE} (default).}

\item{color4Kaiser}{color for Kaiser's
line
(default is \code{'darkorchid4'})}

\item{lwd4Kaiser}{\code{lwd} value
(i.e., width)
for Kaiser's criterion line.
(default is \code{'2.5'})
# @examples  # PlotScree(ev)}
}
\description{
\code{PlotScree}: Plot the scree for the eigenvalues
of an SVD-based multivariate analysis.
Note that the function can recompute the
eigen-values when a percentage is given.
For example  \code{ExPosition} does not return all ev
       but only the requested one. but return all percentage
       so if max.ev is specified, it is used to recompute
       all eigenvalues.
 By default \code{PlotScree}
 will not plot the line corresponding to
 the average inertia (i.e., Kaiser criterion).
 If provided with probabilities,
 \code{PlotScree} will
 color differently the "significant"
 eigenvalues.
}
\author{
Hervé Abdi with help
from Derek Beaton and Ju-Chi Yu.
}
