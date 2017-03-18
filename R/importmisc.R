## Functions for importing data that doesn't come from GCAM queries, such as the
## results of post-run analysis codes.

#' Import a table of data into a project data set as a new query.
#'
#' This function allows you to add a free-standing table to your project data.
#' Unlike \code{\link{addScenario}}, it does not attempt to run the GCAM Model
#' Interface; therefore, it is suitable for cases where the data has come from
#' some other source, such as the output of a post-run analysis code.
#'
#' The data to be read can be either a data frame or the name of a file
#' containing the data.  In the latter case, the data will be read using
#' \code{read.csv}.  The format of the table should be similar to the format of
#' tables produced by the Model Interface.  Namely:
#' \itemize{
#'   \item There should be a "scenario" column with the scenario name in it.  The
#' date of the run, if any, will be stripped off.
#'   \item There should be one or more columns that identify each data point,
#' such as "region" or "sector".  Column names will be converted to lower case.
#'   \item There should be a "Units" column that gives the measurement unit for
#' the data.
#'   \item The results for model years should appear in the remaining columns,
#' one for each year.  For historical reasons, most utilities for handling GCAM
#' data expect these columns to start with 'X', for example: "X2050"; however,
#' this convention is not enforced here.
#' }
#'
#' You can have multiple scenarios in a single table if you want.  In that case
#' the relevant data will be added to each scenario that appears in the
#' "scenario" column.  However, because the run date is stripped off, the
#' scenarios must have distinct scenario names.  Having multiple copies of the
#' same scenario (i.e., the same scenario name, but different run dates) is
#' \emph{not} supported, and attempting to load such a table will result in a
#' single query with duplicated rows.
#'
#' The scenarios found will be added to the project data set supplied.  This can
#' be either the data set itself, or the name of a file containing the data
#' set.  Either way, the modified data set will be written back to the file it
#' came from.  If the attempt to add the query to \emph{any} scenarios fails,
#' then the function will throw an error and the project data will be
#' unmodified.
#'
#' @param project The project data set to add the query to.  This can be either
#' a project data structure the name of a project data file.
#' @param qdata Query data to add.  This can be either a data frame or the name
#' of a file containing the data in csv format.
#' @param queryname The name to use for this query when it is stored in the
#' project.
#' @param clobber Flag: if \code{TRUE}, then the operation can replace a query
#' in an existing scenario from the data set.  If \code{FALSE}, then attempting
#' to replace an existing query will cause the entire operation to fail.
#' @param transformation Transformation function to the data after it has been
#' cleaned up but before it has been added to the project.
#' @param saveProj A flag to save the project to disk after data has been added.
#' A user may want to avoid it if they are for instance calling this method several
#' times and would prefer to save at the end.  Users can always save at anytime by
#' calling \code{saveProject}.
#' @param strict.rundate Flag: if \code{TRUE}, then require that the run dates
#' match in order to add queries to a scenario, and fail the entire operation if
#' they don't.  If \code{FALSE}, then ignore dates in the new data set.
#' @export
addQueryTable <- function(project, qdata, queryname, clobber=FALSE,
                          transformation=NULL, saveProj=TRUE,
                          strict.rundate=FALSE)
{
    ## Check to see if either of the inputs are file names and if so replace
    ## them with the actual data.
    if(is.character(qdata)) {
        qdata <- read.csv(qdata, row.names=FALSE)
    }

    project <- loadProject(project)

    ## standardize the case of column names.
    qdata <- stdcase(qdata)

    ## Check to see if the scenario name includes a date marker.  If so, split
    ## the date into its own column.
    if(all(grepl('date=', qdata$scenario))) {
        sd <- sep.date(qdata$scenario)
        qdata <- dplyr::mutate(qdata, scenario=sd[['scenario']],
                               rundate=sd[['date']])
    }
    else {
        ## don't have dates, so put in an NA placeholder for now
        qdata <- dplyr::mutate(qdata, rundate=NA)
    }

    if(!is.null(transformation)) {
        qdata <- transformation(qdata)
    }

    qdsplit <- split(qdata, qdata[['scenario']])

    for(scenario in names(qdsplit)) {
        ## process each scenario.
        if(! scenario %in% listScenarios(project)) {
            warning('Scenario ', scenario,
                    ' does not exist in this project.  Creating.')
            project[[scenario]] <- list()
        }

        scenqdata <- qdsplit[[scenario]]
        if(strict.rundate) {
            rundate <- unique(scenqdata[['rundate']])
            if(length(rundate) > 1) {
                ## The table for this scenario has more than one run date
                stop('addQueryTable: Inconsistent run dates in scenario ',
                     scenario)
            }
            if(is.na(rundate) || rundate != getRundates(project,scenario)) {
                stop('addQueryTable: run date does not match for scenario ',
                     scenario, '  old date: ', getRundates(project, scenario),
                     '  new date: ', rundate)
            }
        }

        ## Check validity
        status <- query.valid(scenqdata)
        if(status != 0) {
            stop('addQueryTable: query table is malformed for scenario ',
                 scenario, '  return code was ', status)
        }

        ## Add the new table to the scenario.
        scenqdata$rundate <- NULL       # Not needed anymore.
        if(queryname %in% listQueries(project, scenario)) {
            if(clobber) {
                ## Ok to replace
                project[[scenario]][[queryname]] <- scenqdata
            }
            else {
                warning('addQueryTable: query ', queryname,
                     ' already exists for scenario ', scenario,
                     ' and noclobber is set.')
            }
        }
        else {
            ## Add as new query
            project[[scenario]][[queryname]] <- scenqdata
        }
    }
    ## If we make it here, then all scenarios have been successfully added.
    ## Save the project back to its permanent storage.
    if(saveProj) {
        saveProject(project)
    }
    project                             # remove the 'invisible' attribute
}
