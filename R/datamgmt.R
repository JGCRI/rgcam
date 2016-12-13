#' Add a scenario from a GCAM output database to a project data set
#'
#' This function will run the GCAM Model Interface to extract the query data for
#' a scenario in a GCAM output database.  The query data is added to a project
#' data file.  You can optionally specify an XML file of queries to run;
#' otherwise, the built-in default queries will be run.  The resulting data file
#' will be suitable for loading into the dashboard's user interface.
#'
#' The scenario being added to the project data set is not checked to verify
#' that it has the same queries as previously existing scenarios.  Having a
#' mismatch in the queries available between scenarios is not a problem for
#' looking at individual scenario results, but may cause an error when looking
#' at difference plots between scenarios, which presume that the query being
#' displayed exists in both data sets.
#'
#' The date value will be clipped from the scenario name and discarded.  If a
#' newly-read scenario is a duplicate of one already in the file, the operation
#' will fail unless \code{clobber = TRUE}, in which case the old scenario will
#' be silently overwritten.
#'
#' If everything goes as expected, the new scenario will be added to the data
#' set and written back into the project data file.  The new data set will also
#' be returned from the function so that it can be used without having to reread
#' it.
#'
#' @param dbFile GCAM database to extract scenario from.
#' @param projFile Project file to add extracted results to.
#' @param scenario Name of scenario to extract.  If \code{NULL}, use the last
#' scenario in the GCAM database.
#' @param queryFile XML query file to pass to the GCAM Model Interface.  If
#' \code{NULL}, use a default query file containing commonly used queries.
#' @param clobber If \code{TRUE}, overwrite any existing scenario of the same
#' name; otherwise, fail if \code{scenario} already exists in the data set.
#' @param mijar Java jar file for the GCAM Model Interface.
#' @return The project dataset with the new scenario added.
#' @importFrom dplyr %>%
#' @export
addScenario <- function(dbFile, projFile, scenario, queryFile=NULL,
                        clobber=FALSE,
                        mijar='~/ModelInterface/ModelInterface.jar') {
    prjdata <- load(projFile)
    scen <- sep.date(scenario)          # list(scenario=..., date=...)
    if(!clobber && scen$scenario %in% names(prjdata)) {
        msg <- paste('Scenario', scen$scenario,
                     'already exists in the data set, and clobber=FALSE. Aborting.')
        message(msg)
        return
    }

    outFile <- runModelInterface(dbFile, scenario, queryFile, mijar)
    tables <- gcammaptools::parse_mi_output(outFile) %>%
        lapply(table.scen.trim)

    attr(tables, 'date') <- scen$date
    prjdata[[scen$scenario]] <- tables

    save(prjdata, file=projFile, compress='xz')

    prjdata
}


#' List the scenarios in a project data set
#'
#' Return the names of the scenarios available in a project data set.  The input
#' can be either the name of a file containing the data set, or the data
#' structure previously loaded from such a file.
#'
#' @param projData The data set to report on.
#' @export
listScenarios <- function(projData) {
    if(is.character(projData)) {
        pd <- load(projData)
    }
    else {
        pd <- projData
    }

    names(pd)
}


#' List the queries available for a scenario
#'
#' Return the names of the queries available for a scenario in a project data
#' set.  Unlike \code{\link{listScenarios}}, this function requires the data set
#' to have been previously loaded, so it cannot take a file name.
#'
#' @param projData The data set to report on.
#' @param scenario The name of the scenario to report on.
#' @export
listQueries <- function(projData, scenario) {
    if(is.character(projData)) {
        stop('This function requires the data set to have been already loaded.')
    }

    if(scenario %in% names(projData)) {
        names(projData[[scenario]])
    }
    else {
        msg <- paste("Scenario", scenario, "is not in this data set.")
        warn(msg)
        NULL
    }
}


#' Separate the scenario and date in GCAM scenario tags.
#'
#' GCAM scenarios are named according to a user-supplied name and the date the
#' scenario was run.  The resulting format is
#' "Scenario_Name,date=YYYY-DD?-MM?THH:MM:SS-hh:mm".  ('X?' represents an X that might
#' or might not be present.  The user string and date are separated by exactly
#' one space.)  This function, therefore, splits the name string from the date.
#' The scenario name is returned as is, and the date is converted to a date
#' object.  The results are returned as a list with elements 'scenario' and
#' 'date'.
#'
#' This function assumes that it can split the
#'
#' @param scenstr The scenario string.  This may be a vector, such as the
#' 'scenario' column in a table of GCAM results
#' @return \code{list(scenario=Scenario_strings, date=Dates)}
sep.date <- function(scenstr) {
    mtx <- stringr::string_split_fixed(scenstr,',date=',2)
    scenario <- mtx[,1]
    date <- lubridate::ydm_hms(mtx[,2])
    list(scenario=scenario, date=date)
}

#' Run the GCAM Model Interface with results placed into a temporary file.
#'
#' This function will run the Model Interface on the selected GCAM output
#' database.  The results will be placed in a temporary file, the name of which
#' will be returned from the function.
#'
#' @param dbFile The GCAM output database to run queries on.
#' @param scenario The name of the scenario to query.  If \code{NULL}, then take
#' the last one in the database.
#' @param queryFile Name of the file containing the queries.  If \code{NULL},
#' then use the built-in default.
runModelInterface <- function(dbFile, scenario=NULL, queryFile=NULL,
                              mijar='~/ModelInterface/ModelInterface.jar') {

    ## XXX Someday, when we have more time we should replace all of this with
    ## code that invokes the Model Interface functionality directly using
    ## rjava.

    if(is.null(queryFile)) {
        queryFile <- system.file('extdata','default-query.xml',
                                 package='GCAMdashboard')
    }

    batch <- prototype.batch            # XML file with placeholders for us to
                                        # fill in
    if(is.null(scenario)) {
        batch <- batch[grep('\\[SCENARIO\\]', batch, invert=TRUE)] # drop the
                                        # scenario designator in this case.
    }
    else {
        batch <- sub('\\[SCENARIO\\]', scenario, batch)
    }
    batch <- sub('\\[DBFILE\\]', dbFile, batch)
    batch <- sub('\\[QUERYFILE\\]', queryFile, batch)
    outfile <- tempfile(fileext='.csv')
    batch <- sub('\\[OUTFILE\\]', outfile, batch)
    batchfile <- tempfile(fileext='.xml')
    write(batch, file=batchfile)

    system2('java', c('-jar', mijar, '-b', batchfile))
    outfile
}

#' Trim the 'date=' from the scenario column in a table
#'
#' Return a version of a GCAM results table in which the scenario name contains
#' just the name of the scenario, without the date information that is typically
#' packed into that column.
table.scen.trim <- function(tbl) {
    dplyr::mutate(tbl, scenario=sep.date(scenario)$scenario)
}
