################################################################################
#### importgcam.R:  Functions related to importing data from GCAM databases.
################################################################################

#' Add a scenario from a GCAM output database to a project data set
#'
#' This function will run the GCAM Model Interface to extract the query data for
#' a scenario in a GCAM output database.  The query data is added to a project
#' data file.  You can optionally specify an XML file of queries to run;
#' otherwise, the built-in default queries will be run.  The resulting data file
#' will be suitable for loading into the dashboard's user interface.  It is not
#' necessary, or even helpful, to load a project file before adding a scenario
#' to it.
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
#' You may optionally specify transformations to apply to the tables returned by
#' the model interface.  Examples of transformations you might want to apply
#' include aggregating values or dropping unused columns.  Specify
#' transformations as a named list of function objects, where the names in the
#' list indicate which queries the transformations will be applied to.  Each
#' function should take a single argument, which will be the original table and
#' should return the modified table as a data frame.  Do not drop the "scenario"
#' column as part of one of your transformations; certain types of plots need
#' it.
#'
#' If everything goes as expected, the new scenario will be added to the data
#' set and written back into the project data file.  The new data set will also
#' be returned from the function so that it can be used without having to reread
#' it.
#'
#' @param dbFile GCAM database to extract scenario from.
#' @param proj Project to add extracted results to.  Can be either a project
#' data structure or the name of a project data file.  The file will be created
#' if it doesn't already exist.
#' @param scenario Name of scenario to extract.  If \code{NULL}, use the last
#' scenario in the GCAM database.
#' @param queryFile XML query file to pass to the GCAM Model Interface.  If
#' \code{NULL}, use a default query file containing commonly used queries.
#' @param clobber If \code{TRUE}, overwrite any existing scenario of the same
#' name; otherwise, fail if \code{scenario} already exists in the data set.
#' @param miclasspath Java class path for the GCAM Model Interface.  If
#' \code{NULL}, use a bult-in default.
#' @param transformations Transformation functions to apply to the queries (see
#' details).
#' @param migabble Control what happens to the model interface console output.
#' This is passed to the stdout/stderr argument of \code{system2}.  Default is
#' to discard.
#' @return The project dataset with the new scenario added.
#' @importFrom dplyr %>%
#' @export
addScenario <- function(dbFile, proj, scenario=NULL, queryFile=NULL,
                        clobber=FALSE, miclasspath=NULL, transformations=NULL,
                        migabble=NULL) {

    if(is.character(proj)) {
        projFile <- proj
        if(file.exists(projFile)) {
            projFile <- normalizePath(projFile)
            if(file.access(projFile, mode=6)!=0) { # 6 == read and write permission
                ## file.access returns 0 on success
                msg <- paste("File", projFile,
                             "exists but lacks either read or write permission.")
                stop(msg)
            }
            prjdata <- loadProject(projFile)
        }
        else {
            prjdata <- list()
            save(prjdata, file=projFile)    # have to create file to use normalizePath
            projFile <- normalizePath(projFile)
            attr(prjdata, 'file') <- projFile
        }
    }
    else if(project.valid(proj)==0) {
        projFile <- attr(proj,'file')
        prjdata <- proj
    }
    else {
        stop('addScenario: invalid object passed as proj argument; proj must be a filename or project data object.')
    }

    if(!is.null(scenario)) {
        ## Since we have the scenario name, we can make a quick check to see if
        ## the scenario already exists in the data set and abort if clobber is
        ## not set.  If scenario is NULL (meaning we will get the scenario name
        ## from the database, then we will have to run the queries and check the
        ## scenario after the fact.
        scen <- sep.date(scenario)          # list(scenario=..., date=...)
        if(!clobber && scen$scenario %in% names(prjdata)) {
            msg <- paste('Scenario', scen$scenario,
                         'already exists in the data set, and clobber=FALSE. Aborting.')
            message(msg)
            return(prjdata)
        }
    }

    outFile <- runModelInterface(dbFile, scenario, queryFile, miclasspath, migabble)
    tables <- parse_mi_tables(outFile)
    if(length(tables) == 0) {
        stop('Queries returned no data.')
    }

    if(is.null(scenario)) {
        ## The scenario name was pulled from the database.  It will be in the
        ## scenario column of the tables.
        scenario <- tables[[1]]$scenario[1]
        scen <- sep.date(scenario)

        ## Now that we have the scenario name we can check to see if it already
        ## exists.
        if(!clobber && scen$scenario %in% names(prjdata)) {
            msg <- paste('Scenario', scen$scenario,
                         'already exists in the data set, and clobber=FALSE. Aborting.')
            message(msg)
            return(prjdata)
        }
    }

    tables <- lapply(tables, table.scen.trim)

    ## apply transformations, if any
    if(!is.null(transformations)) {
        for(xform in names(transformations)) {
            if(! xform %in% names(tables)) {
                warning("addScenario: cannot apply transform", xform,
                        ". No such table in data set.")
            }
            else {
                ## apply this transformation to the query of the same name and
                ## store it back in its original slot.
                tables[[xform]] <- transformations[[xform]](tables[[xform]])
            }
        }
    }

    attr(tables, 'date') <- scen$date
    prjdata[[scen$scenario]] <- tables

    save(prjdata, file=projFile, compress='xz')

    prjdata
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
    mtx <- stringr::str_split_fixed(scenstr,',date=',2)
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
#' @param miclasspath Classpath for the GCAM model interface.  If \code{NULL},
#' then use the built-in default.
#' @param migabble Control what happens to Model Interface console output.
runModelInterface <- function(dbFile, scenario=NULL, queryFile=NULL,
                              miclasspath=NULL, migabble=NULL) {

    ## XXX Someday, when we have more time we should replace all of this with
    ## code that invokes the Model Interface functionality directly using
    ## rjava.

    if(is.null(miclasspath)) {
        miclasspath <- DEFAULT.MICLASSPATH
    }

    if(is.null(queryFile)) {
        queryFile <- SAMPLE.QUERIES
    }

    batch <- PROTOTYPE.MIBATCH          # XML file with placeholders for us to
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

    system2("java", c("-cp", miclasspath, "ModelInterface/InterfaceMain", "-b",
                      batchfile), stdout=migabble)
    outfile
}

#' Parse the GCAM ModelInterface output
#'
#' Parse the raw output of a GCAM batch query into a set of tables.
#'
#' @param fn Name of the file containing the output from the GCAM Model Interface.
parse_mi_tables <- function(fn) {
    ## transplanted from the gcammaptools package.
    tables <- list()

    # See if the user has provided any values overriding our defaults
    use_tablenames <- TRUE
    headerline <- "scenario"
    yearpat <- "X[0-9]{4}"

    ## The original version of this function had a bunch of logging commands.
    ## I've disabled these because I didn't want to bring the logging functions
    ## along, but I've left the commented logging commands in place in case we
    ## need them for debugging in the future.
    ##printlog("Reading", fn, "...", cr = F, level = LOGLEVEL_SUMMARY)
    tryCatch({
        fdata <- scan(fn, what = character(), sep = "\n", blank.lines.skip = F, quiet = T)
    }, error = function(err) {
        stop(paste('error reading file', as.character(err)))
    })

    ##printlog("OK.", ts = F)
    tableheaders <- grep(headerline, fdata)
    ##printlog("Table headers located in lines", tableheaders)
    table_name <- NA

    for (i in seq_along(tableheaders)) {
        if (use_tablenames) {
            table_name <- fdata[tableheaders[i] - 1]
        } else {
            table_name <- i
        }
        ##printlog("Table", i, "name is", table_name)

        nskip <- tableheaders[i] - 1
        headers <- fdata[tableheaders[i]]
        extrafields <- 0
        while (substr(headers, nchar(headers), nchar(headers)) == ",") {
            headers <- substr(headers, 1, nchar(headers) - 1)
            extrafields <- extrafields + 1
        }

        if (i == length(tableheaders)) {
            nrows <- -1
        } else {
            nrows <- tableheaders[i + 1] - tableheaders[i] - 1 - use_tablenames  # i.e., subtract 1 is using table names
        }

        ##printlog("Reading table", i, "in", fn, "( skip =", nskip, " nrows =", nrows, ")")
        tempdata <- read.table(fn, row.names = NULL, skip = nskip, nrows = nrows, header = T,
                               sep = ",", comment.char = '#', stringsAsFactors = F)

        # Remove extra columns on end - this is often present in the MI output
        if (extrafields > 0) {
            ##printlog("Removing", extrafields, "extra fields")
            tempdata <- tempdata[-seq(ncol(tempdata) - extrafields + 1, ncol(tempdata))]
        }

        ##printlog("Table", i, "name is", table_name)

        # Get rid of 'X' in year names names(tempdata)<-ifelse(grepl('(X2)|(X1)', names(tempdata)),
        # sub('X','',names(tempdata)), names(tempdata))

        tables[[table_name]] <- tempdata
    }

    tables
}  # parse_mi_tables

#' Parse the GCAM ModelInterface output (DEPRECATED)
#'
#' Parse the raw output of a GCAM batch query into a set of tables.
#' Normally you will pass the structure returned by this function to
#' \code{process_batch_q} extract (and optionally do some filtering
#' and processing on) the table you are looking for.
#'
#' This function is obsolete and is included only for backward compatibility
#' with old code that uses it.  Instead, use \code{\link{addScenario}} to
#' process GCAM data into a native structure.
#'
#' @param fn Name of the file containing the output from the GCAM Model
#' Interface.
#' @export
parse_mi_output <- function(fn) {
    .Deprecated('addScenario','rgcam',
                'This function will be removed in a future release.  Consider importing your data with addScenario instead.')
    parse_mi_tables(fn)
}


## Trim the 'date=' from the scenario column in a table
##
## Return a version of a GCAM results table in which the scenario name contains
## just the name of the scenario, without the date information that is typically
## packed into that column.
table.scen.trim <- function(tbl) {
    dplyr::mutate(tbl, scenario=sep.date(scenario)[['scenario']])
}

#' Default java class path for running the Model Interface
#'
#' This is the class path you get if you pass \code{NULL} to any of the
#' functions that run (directly or indirectly) the model interface.  The
#' necessary jar files are bundled with the package, so unless you need a
#' customized version of the model interface for some reason, this should take
#' care of all your GCAM query needs.
DEFAULT.MICLASSPATH <- paste0(system.file("ModelInterface", "jars",
                                          package="rgcam"),"/\\*:",
                              system.file("ModelInterface",
                                          "ModelInterface.jar",
                                          package="rgcam"))

#' Sample GCAM database file.
#'
#' This file can be used for testing the mechanics of running GCAM queries using
#' the functions in this package.  A lot of the data normally included in the
#' database has been stripped out to control the total size, so a lot of queries
#' won't work properly.  The \code{\link{SAMPLE.QUERIES}} should all work.
SAMPLE.GCAMDB <- system.file("extdata","sample_basexdb",
                             package="rgcam")

#' Sample GCAM query file
#'
#' These queries should work with the sample data in
#' \code{\link{SAMPLE.GCAMDB}}.
SAMPLE.QUERIES <- system.file("ModelInterface", "sample-queries.xml",
                              package="rgcam")

#' Prototype Model Interface batch file
#'
#' This is a prototype for the batch file that drives the Model Interface.  It
#' has a bunch of tags like [SCENARIO], [DBFILE], etc. for
#' \code{\link{runModelInterface}} to fill in with the particulars of the
#' queries we are trying to run.
PROTOTYPE.MIBATCH <- readLines(system.file("ModelInterface",
                                           "batch-prototype.xml",
                                           package="rgcam"))
