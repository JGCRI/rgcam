################################################################################
#### manageProjectData.R:  Functions for managing project data
################################################################################

#' Load a project data file.
#'
#' This function will load a project data file and return the project data
#' structure.  Project files can be created with \code{\link{addScenario}}.
#'
#' Project data is stored as an R 'save' file with a single variable called
#' 'prjdata'.  If the data file doesn't have the expected variable in it, the
#' function will throw an error.  It also does some minimal checks to ensure
#' that the data is valid, but there is a limit to what it can do.
#' Specifically, it checks to see that the structure loaded is:
#'     1. a list,
#'     2. with at least one element, and
#'     3.   all of those elements are lists,
#'     4.   with at least one element, and
#'     5.     all of those elements are data frames,
#'     6.     with a scenario column, and
#'     7.     at least one other column.
#' If the validation fails, the error message will indicate which of these steps
#' it failed on.
#'
#' Despite these checks, it is possible to construct a data set that passes and
#' yet still contains bad data.  When in doubt load the file directly and check
#' to see that it contains the data you expect it to.
#' @param projFile The file to load the data from.
#' @export
loadProject <- function(projFile) {
    load(projFile)                  # Loads the variable prjdata.
    if(!exists("prjdata", inherits=FALSE)) {
        ## Something went wrong with the load.  Probably projFile exists but
        ## isn't a valid project file.
        message(paste("File", projFile,
                      "does not contain valid project data."))
        message("Try loading the file into an R session and verify that it contains the variable 'prjdata'.")
        stop(paste("Unable to load project file", projFile))
    }

    stat <- project.valid(prjdata)
    if(stat != 0) {
        stop(paste("Invalid project data in",
                   projFile,".  Validation failed at step", stat))
    }

    ## Check the file name stored in the data.
    if(is.null(attr(prjdata,"file")) || attr(prjdata, "file") != projFile) {
        ## This generally means that the file has been moved or renamed
        ## since it was written.  Notify the user, but it's not an error.
        message("Project file name (",
                projFile,") does not match the name recorded in the data (",
                attr(prjdata,'file'), ").  Updating to match new file name.")
        attr(prjdata, 'file') <- projFile
    }

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
        warning(msg)
        NULL
    }
}

#' Check whether a project data structure is valid.
#'
#' This function will check the 7 conditions described in
#' \code{\link{loadProject}}.  It will return 0 if they pass, or the number of
#' the first condition to fail otherwise.
#' @param prjdata The data structure to test.
project.valid <- function(prjdata) {
    if(!is.list(prjdata))
        return(1)

    if(length(prjdata) == 0)
        return(2)

    scenario.checks <- sapply(prjdata, scenario.valid)
    if(any(scenario.checks>0))
        return(min(scenario.checks[scenario.checks>0]))
    else
        return(0)
}

#' Check whether a scenario component in a project data structure is valid.
#'
#' This function will check conditions 3-7 for a single scenario component in a
#' project data structure.  It will return the number of the first check to
#' fail, or 0 if all pass.
#'
#' @param scendata The data structure to test
scenario.valid <- function(scendata) {
    if(!is.list(scendata))
        return(3)

    if(length(scendata) == 0)
        return(4)

    query.checks <- sapply(scendata, query.valid)
    if(any(query.checks>0))
        return (min(query.checks[query.checks>0]))
    else
        return(0)
}

#' Check whether a query table in a scenario component structure is valid.
#'
#' This function will check conditions 5-7 for a single query table in a
#' scenario data structure.  It will return the number of the first check to
#' fail, or 0 if all pass.
#' @param querytable The query table to test.
query.valid <- function(querytable) {
    if(!is.data.frame(querytable))
        return(5)

    if(! 'scenario' %in% names(querytable))
        return(6)

    if(ncol(querytable) < 2)
        return(7)
    else
        return(0)
}
