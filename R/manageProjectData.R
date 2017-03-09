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
#' @param proj Project to add extracted results to.  Can be either a project
#' data structure or the name of a project data file.  The file will be created
#' if it doesn't already exist.
#' @return The project dataset which may or may not have acutally had to be loaded.
#' @export
loadProject <- function(proj) {

    if(is.character(proj)) {
        projFile <- proj
        if(file.exists(projFile)) {
            projFile <- normalizePath(projFile)
            if(file.access(projFile, mode=6)!=0) { # 6 == read and write permission
                ## file.access returns 0 on success
                stop("File ", projFile,
                     " exists but lacks either read or write permission.")
            }
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
        }
        else {
            prjdata <- list()
            save(prjdata, file=projFile)    # have to create file to use normalizePath
            projFile <- normalizePath(projFile)
            attr(prjdata, 'file') <- projFile
            unlink(projFile)            # ...but we don't actually want to
            # create the file unless we succeed.
        }
    }
    else if(project.valid(proj) %in% c(0, 2)) {
        projFile <- attr(proj,'file')
        prjdata <- proj
    }
    else {
        stop("addScenario: invalid object passed as proj argument; proj must be a filename or project data object.")
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

#' Merge a list projects into a single project
#'
#' Users can use this function to collapse multiple project into a single
#' project.  This could be useful for instance to import someone else's
#' data into your project.  A user must explicitly specify the new name
#' the new project will recieve.  In addition they can control what
#' happens with scenario/query collisions with the \code{clobber} param.
#'
#' @param prjname The name of a project data file for the merged project.
#' @param prjlist A list of projects that need to be merged together.  Note
#' each project will be run through \code{\link{loadProject}} in case it has
#' not yet been loaded.
#' @param clobber If \code{TRUE}, overwrite any existing scenario of the same
#' name; otherwise, fail if scenario/query already exists in the data set.
#' @param saveProj A flag to save the project to disk after data has been added.
#' A user may want to avoid it if they are for instance calling this method several
#' times and would prefer to save at the end.  Users can always save at anytime by
#' calling \code{saveProject}.
#' @return The project dataset with the projects merged.
#' @export
mergeProjects <- function(prjname, prjlist, clobber=FALSE, saveProj=TRUE) {
    finalproj <- loadProject(prjname)

    # for loops!
    for(prj in prjlist) {
        prjdata <- loadProject(prj)
        for(scn in names(prjdata)) {
            for(qn in names(prjdata[[scn]])) {
                if(!clobber && !is.null(finalproj[[scn]]) && !is.null(finalproj[[scn]][[qn]])) {
                    warning(paste("Skipping data in",scn,"/",qn,"as clobber is false."))
                } else {
                    finalproj[[scn]][[qn]] <- prjdata[[scn]][[qn]]
                }
            }
        }
    }

    if(saveProj) {
        saveProject(finalproj)
    }

    finalproj
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
#' @param scenarios The name(s) of the scenario(s) to report on.  If NULL,
#' report on all of them.
#' @param anyscen If \code{TRUE}, then list queries that are in any scenario.
#' If \code{FALSE}, list queries that are in all scenarios.
#' @export
listQueries <- function(projData, scenarios=NULL, anyscen=TRUE) {
    if(is.character(projData)) {
        stop("This function requires the data set to have been already loaded.")
    }

    if(is.null(scenarios)) {
        scenarios <- listScenarios(projData)
    }
    else {
        scenok <- scenarios %in% names(projData)
        if(!all(scenok)) {
            for(scen in scenarios[!scenok])
                warning('listQueries: Scenario ', scen,
                        ' is not in this data set.')
            scenarios <- scenarios[scenok]
            if(length(scenarios) == 0)
                stop('listQueries: No valid scenarios given.')
        }
    }

    ## fetch the names of the valid scenarios
    sqlist <- lapply(scenarios, function(scen) {names(projData[[scen]])})

    combine <- if(anyscen) union else intersect
    Reduce(combine, sqlist)
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
#' @importFrom dplyr bind_rows
#' @export
getQuery <- function(projData, query, scenarios=NULL) {
    if(is.null(scenarios)) {
        scenarios <- listScenarios(projData)
    }

    if(! query %in% listQueries(projData))
        stop('getQuery: Query ', query,
             ' is not in any scenarios in the data set.')

    queries <- lapply(scenarios, function(s) {projData[[s]][[query]]})

    bind_rows(queries)
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
        rd <- attr(proj[[scen]], 'date')
        ## This line picks the queries to keep.  If invert==TRUE, then that's
        ## the queries in the list; otherwise it's the ones not in the list.
        proj[[scen]] <- proj[[scen]][(n %in% queries) == invert]
        ## Have to restore the date attribute
        attr(proj[[scen]], 'date') <- rd
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
