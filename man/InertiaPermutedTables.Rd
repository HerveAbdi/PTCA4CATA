% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BootRatio.R
\name{InertiaPermutedTables}
\alias{InertiaPermutedTables}
\title{\code{InertiaPermutedTables}
A function to Compute the inertia of a set of random permutations
of the "Check-Mark" (e.g. CATA) type of data.}
\usage{
InertiaPermutedTables(DataChecks, nPerm = 1000)
}
\arguments{
\item{DataChecks}{An \eqn{I}*\eqn{J} matrix
storing integers.
\eqn{I} are Participants, \eqn{J} are Stimuli.
The entries in Datacheks are integers that match the descriptors
(i.e., 5 for Datacheks[2,3] means that Participant 2}

\item{nPerm}{number of random permutations (default = \code{1000}).
Note that the number of Descriptors is "guessed" by the program
as the largest number is the dataset.}
}
\value{
returns a 1*\code{nPerm} vector with
the \code{nPerm} values
 of the inertia computed with the \code{nPerm} random Permutations.
}
\description{
\code{InertiaPermutedTables} creates a cube of data
from the results of a "Check-Mark"
data set collected in \code{DataChecks}.
These data correspond to participants matching (or not)
one Descriptor to
each stimulus of a set of stimuli.
The Stimuli are the columns of \code{DataChecks}
The Participants are the rows of \code{DataChecks}
The Descriptors are the numbers in \code{DataChecks}
(i.e., 5 for \code{Datacheks[2,3]} means that Participant 2,
chose Descriptor 5 for Stimulus 3)
}
\examples{
\dontrun{
RandomnInertia <- InertiaPermutedTables(ACubeOfDataChecks)
}
}
\author{
Hervé Abdi
}
