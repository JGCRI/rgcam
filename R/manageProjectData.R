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
        stop("Unable to load project file ", projFile)
    }

    stat <- project.valid(prjdata)
    if(stat != 0) {
        stop("Invalid project data in ",
                   projFile,".  Validation failed at step", stat)
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

#' Save project data to a backing file.
#'
#' Write a project data structure to its data file.  This should rarely be
#' necessary, since most of the functions that modify a project file have an
#' option to write the data back to the file.  Mostly it is intended for
#' internal use, but it's exported in case it's useful to someone for some
#' reason.
#'
#' The project data structure includes the name of its backing file, which is
#' why you can call this function with no file argument.  When you save with an
#' explicit file name, the function returns a version of the data structure with
#' an updated file attribute.  If you want the data structure in the calling
#' frame to have its backing file updated, you have to assign this return value
#' back to the original object; otherwise you will write a copy of the data set,
#' but your working copy will continue to be backed by the original file.
#'
#' For example:
#' \preformatted{
#' > prj <- loadProject(file1.dat)
#' > saveProject(prj, 'file2.dat')       # prj is still backed by 'file1.dat'
#' ### ... modify prj ...
#' > saveProject(prj)                    # new data written to 'file1.dat'
#' > prj <- saveProject(prj,'file3.dat') # prj now backed by 'file3.dat'
#' ### ... modify prj again ...
#' > saveProject(prj)                    # newest data written to 'file3.dat'
#' }
#' @param prjdata Project data object.
#' @param file Filename to save to.  If \code{NULL}, use the file the project
#' was loaded from.
#' @export
saveProject <- function(prjdata, file=NULL) {
    ## validate data first
    stat <- project.valid(prjdata)
    if(stat != 0) {
        stop("saveProject:  invalid project data object, stat=", stat)
    }
    if(is.null(file))
        file <- attr(prjdata, 'file')
    else
        attr(prjdata, 'file') <- file
    save(prjdata, file=file, compress='xz')
    invisible(prjdata)
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

#' Remove a scenario from a project data set
#'
#' This function removes one or more scenarios from a project data set and
#' returns the newly modified data.  If this operation is performed on a file,
#' the file is also modified.  If it is performed on a data set that has already
#' been loaded, then the change is passed onto the file only if \code{writeback}
#' is set to TRUE.
#'
#' Technically is is possible to pass a filename for \code{proj} with
#' \code{writeback} set to \code{FALSE}.  This usage will cause the data set to
#' be loaded and filtered without modifying the original, albeit in a somewhat
#' nonintuitive way.
#'
#' @param proj Project data or data file name.
#' @param scenarios Name(s) of the scenario(s) to drop
#' @param invert If \code{TRUE} then delete all scenarios \emph{except} the ones
#' given in \code{scenarios}.
#' @param writeback If \code{TRUE} then write the change into the backing data
#' file.
#' @export
dropScenarios <- function(proj, scenarios, invert=FALSE, writeback=is.character(proj)) {
    writeback <- writeback              # magic!
    if(is.character(proj)) {
        proj <- loadProject(proj)
    }
    else {
        proj <- proj
    }

    for(scen in listScenarios(proj)) {
        if(invert && !(scen %in% scenarios) )
            proj[[scen]] <- NULL
        else if(!invert && scen %in% scenarios)
            proj[[scen]] <- NULL
    }

    if(writeback) {
        saveProject(proj)
    }

    proj
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
        stop("This function requires the data set to have been already loaded.")
    }

    if(scenario %in% names(projData)) {
        names(projData[[scenario]])
    }
    else {
        warning("Scenario ", scenario, " is not in this data set.")
        NULL
    }
}


#' Return the run date for one or more scenarios in a data set.
#'
#' The run dates are recorded in the tables produced by the GCAM Model Interface
#' and copied into the project data set on import.  This function retrieves the
#' run dates for the selected scenarios and returns them as a named vector.
#'
#' @param projData The data set to report on.
#' @param scenarios The names of the scenarios
#' @export
getRundates <- function(projData, scenarios=NULL)
{
    if(is.null(scenarios))
        scenarios <- listScenarios(projData)

    ## Calls like sapply (annoyingly) convert date objects to numeric, losing
    ## the information about the epoch.  This should give us a named vector of
    ## POSIXct objects.
    datevec <-
        lapply(scenarios, function(scen) {attr(projData[[scen]], 'date')}) %>%
            do.call(c,.)
    names(datevec) <- scenarios
    datevec
}

#' Retrieve a query for one or more scenarios
#'
#' Return a data frame with the results for a query for all of the selected
#' scenarios.
#'
#' @param projData The data set to extract from.
#' @param query The name of the query to extract.
#' @param scenarios Vector of scenario names.  If NULL, use all scenarios in the
#' data set.
#' @export
getQuery <- function(projData, query, scenarios=NULL) {
    if(is.null(scenarios)) {
        scenarios <- listScenarios(projData)
    }

    queries <- lapply(scenarios, function(s) {projData[[s]][[query]]})

    do.call(rbind, queries)
}

#' Drop specified queries from scenarios.
#'
#' This function removes the specified queries from a data set.  By default the
#' queries are removed from every scenario they appear in, but the operation can
#' be limited to certain scenarios if desired.  If the operation is performed on
#' a file, the file is also modified.  If it is performed on a data set that has
#' already been loaded, then the change is written back to the file only if
#' \code{writeback} is set to TRUE.  Either way, the modified data set is
#' returned.
#'
#' As with \code{\link{dropScenarios}}, it is possible and perhaps occasionally
#' useful to specify a file but to force the file not to be updated by
#' specifying \code{writeback=FALSE}.
#'
#' @param proj Project data or data file name.
#' @param queries Name(s) of the queries to drop.
#' @param invert If \code{TRUE} then delete all queries \emph{except} the ones
#' in \code{queries}.
#' @param writeback If \code{TRUE} then write the change to the backing data
#' file.
#' @param scenarios Drop the queries only from the specified scenarios (default
#' is to drop from all scenarios).
#' @param invertScenario Drop queries from the scenarios \emph{not} listed in
#' \code{scenarios}.
#' @export
dropQueries <- function(proj, queries, invert=FALSE,
                        writeback=is.character(proj), scenarios=NULL,
                        invertScenario=FALSE) {
    writeback <- writeback
    if(is.character(proj)) {
        proj <- loadProject(proj)
    }

    if(is.null(scenarios)) {
        scenarios <- listScenarios(proj)
    }
    if(invertScenario) {
        allscen <- listScenarios(proj)
        scenarios <- allscen[! allscen %in% scenarios]
    }

    for(scen in scenarios) {
        n <- names(proj[[scen]])
        ## This line picks the queries to keep.  If invert==TRUE, then that's
        ## the queries in the list; otherwise it's the ones not in the list.
        proj[[scen]] <- proj[[scen]][(n %in% queries) == invert]
    }

    if(writeback) {
        saveProject(proj)
    }

    proj
}

#' Check whether a project data structure is valid.
#'
#' This function will check the 7 conditions described in
#' \code{\link{loadProject}}.  It will return 0 if they pass, or the number of
#' the first condition to fail otherwise.
#' @param prjdata The data structure to test.
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
