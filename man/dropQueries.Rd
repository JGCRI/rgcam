% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manageProjectData.R
\name{dropQueries}
\alias{dropQueries}
\title{Drop specified queries from scenarios.}
\usage{
dropQueries(
  proj,
  queries,
  invert = FALSE,
  writeback = is.character(proj),
  scenarios = NULL,
  invertScenario = FALSE
)
}
\arguments{
\item{proj}{Project data or data file name.}

\item{queries}{Name(s) of the queries to drop.}

\item{invert}{If \code{TRUE} then delete all queries \emph{except} the ones
in \code{queries}.}

\item{writeback}{If \code{TRUE} then write the change to the backing data
file.}

\item{scenarios}{Drop the queries only from the specified scenarios (default
is to drop from all scenarios).}

\item{invertScenario}{Drop queries from the scenarios \emph{not} listed in
\code{scenarios}.}
}
\description{
This function removes the specified queries from a data set.  By default the
queries are removed from every scenario they appear in, but the operation can
be limited to certain scenarios if desired.  If the operation is performed on
a file, the file is also modified.  If it is performed on a data set that has
already been loaded, then the change is written back to the file only if
\code{writeback} is set to TRUE.  Either way, the modified data set is
returned.
}
\details{
As with \code{\link{dropScenarios}}, it is possible and perhaps occasionally
useful to specify a file but to force the file not to be updated by
specifying \code{writeback=FALSE}.
}
