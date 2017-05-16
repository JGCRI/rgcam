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
#' @param conn A GCAM database to extract scenario from.  This can be either a
#' filename or a connection opened with localDBConn or remoteDBConn.  If a
#' filename is passed, it will be opened as a local connection.
#' @param proj Project to add extracted results to.  Can be either a project
#' data structure or the name of a project data file.  The file will be created
#' if it doesn't already exist.
#' @param scenario Name of scenario to extract.  If \code{NULL}, use the last
#' scenario in the GCAM database.
#' @param queryFile XML query file to pass to the GCAM Model Interface.  If
#' \code{NULL}, use a default query file containing commonly used queries.
#' @param clobber If \code{TRUE}, overwrite any existing scenario of the same
#' name; otherwise, fail if \code{scenario} already exists in the data set.
#' @param transformations Transformation functions to apply to the queries (see
#' details).
#' @param saveProj A flag to save the project to disk after data has been added.
#' A user may want to avoid it if they are for instance calling this method several
#' times and would prefer to save at the end.  Users can always save at anytime by
#' calling \code{saveProject}.
#' @param saveProjEach A flag to save the project to disk after each query has
#' completed. This would be useful if a user suspects a failure in the middle
#' of running queries and would like to not loose progress made.
#' @return The project dataset with the new scenario added.
#' @export
addScenario <- function(conn, proj, scenario=NULL, queryFile=NULL,
                        clobber=FALSE, transformations=NULL,
                        saveProj=TRUE, saveProjEach=FALSE) {

    if(is.character(conn)) {
        conn <- localDBConn(dirname(conn), basename(conn))
    }

    prjdata <- loadProject(proj)

    if(is.null(queryFile)) {
        queryFile <- SAMPLE.QUERIES
    }

    queries <- parse_batch_query(queryFile)

    # If no transformations are set just make it an empty list to simplify logic
    if(is.null(transformations)) {
        transformations <- list()
    }

    scen_names <- sep.date(scenario)[["scenario"]]

    for(qn in names(queries)) {
        # While putting a clobber check here is duplicative to the one in
        # addQueryTable it would still be useful since we could then potentially
        # avoid running queries.
        if(!clobber && all(scen_names %in% listScenarios(prjdata)) &&
           (qn %in% listQueries(prjdata, scen_names, anyscen=FALSE)))
        {
            warning(paste("Skipping running query", qn, "since clobber is false and already exists in project."))
        } else {
            bq <- queries[[qn]]
            table <- runQuery(conn, bq$query, scenario, bq$regions)
            prjdata <- addQueryTable(prjdata, table, qn, clobber, transformations[[qn]], saveProj && saveProjEach)
        }
    }

    if(saveProj) {
        saveProject(prjdata)
    }

    prjdata
}

#' Add a data by running a single on a GCAM output database to a project data set
#'
#' This function will run the GCAM Model Interface to extract the query data for
#' a scenario in a GCAM output database.  The query data is added to a project
#' data file.  This function accepts just a single query to be run as apposed to
#' a batch file with several queries. This is typically provided as the
#' XML typically found in the Main_queries.xml.  See examples for possible syntax
#' to specify these.
#'
#' The date value will be clipped from the scenario name and discarded.  If a
#' newly-read scenario/query is a duplicate of one already in the file, the operation
#' will fail unless \code{clobber = TRUE}, in which case the old data will
#' be silently overwritten.
#'
#' You may optionally specify transformations to apply to the tables returned by
#' the model interface.  Examples of transformations you might want to apply
#' include aggregating values or dropping unused columns.  Specify
#' transformation as a function object, the function should take a single
#' argument, which will be the original table and should return the modified table
#' as a data frame.  Do not drop the "scenario" column as part of one of your
#' transformations; certain types of plots need it.
#'
#' If everything goes as expected, the new scenario will be added to the data
#' set and written back into the project data file.  The updated project will also
#' be returned from the function so that it can be used without having to reread
#' it.
#'
#' @param conn A GCAM database to connection extract scenario from.
#' @param proj Project to add extracted results to.  Can be either a project
#' data structure or the name of a project data file.  The file will be created
#' if it doesn't already exist.
#' @param qn The query name to use when storing the results.  We have to provide
#' this since it might not always be obvious what this is by looking at the \code{query}.
#' @param query A Model Interface query to run.  See examples for possible syntax.
#' @param scenario Name of scenario to extract.  If \code{NULL}, use the last
#' scenario in the GCAM database.
#' @param regions A list of regions to query.  If \code{NULL}, all regions will
#' be queries.
#' @param clobber If \code{TRUE}, overwrite any existing scenario of the same
#' name; otherwise, fail if \code{scenario} already exists in the data set.
#' @param transformations Transformation functions to apply to the queries (see
#' details).
#' @param saveProj A flag to save the project to disk after data has been added.
#' A user may want to avoid it if they are for instance calling this method several
#' times and would prefer to save at the end.  Users can always save at anytime by
#' calling \code{saveProject}.
#' @return The project dataset with the new scenario added.
#'
#' @examples
#' # The query must be the same XML found in a GCAM query file:
#' SAMPLE.GCAMDBLOC <- system.file("extdata",package="rgcam")
#' db_connection <- localDBConn(SAMPLE.GCAMDBLOC, "sample_basexdb")
#' query_name <- "CO2 concentrations"
#' co2_query <- '<ClimateQuery title="CO2 concentrations">
#'                 <axis1 name="CO2-concentration">none</axis1>
#'                 <axis2 name="Year">CO2-concentration[@year]</axis2>
#'                 <xPath buildList="true" dataName="CO2-concentration" group="false" sumAll="false">climate-model/CO2-concentration/text()</xPath>
#'                 <comments/>
#'               </ClimateQuery>'
#' addSingleQuery(db_connection, "test.proj", query_name, co2_query)
#'
#' # However it could also be given for instance as a query string that will result in such XML:
#' SAMPLE.QF <- system.file("ModelInterface", "sample-queries-interactive.xml", package="rgcam")
#' co2_query <- paste0("doc('", SAMPLE.QF, "')//*[@title='",
#'                     query_name, "']")
#' addSingleQuery(db_connection, "test.proj", query_name, co2_query)
#'
#' # Alternatively a user may use an XML package if for instance their query file is
#' # stored locally but are running queries on some remote machine:
#' library(xml2)
#' queries <- read_xml(SAMPLE.QF)
#' co2_query <- xml_find_first(queries, paste0("//*[@title='", query_name, "']"))
#' addSingleQuery(db_connection, "test.proj", query_name, co2_query)
#'
#' @export
addSingleQuery <- function(conn, proj, qn, query, scenario=NULL, regions=NULL,
                           clobber=FALSE, transformations=NULL,
                           saveProj=TRUE) {

    prjdata <- loadProject(proj)

    scen_names <- sep.date(scenario)[["scenario"]]

    if(is.null(regions)) {
        regions <- c()
    }

    # While putting a clobber check here is duplicative to the one in
    # addQueryTable it would still be useful since we could then potentially
    # avoid running queries.
    if(!clobber && all(scen_names %in% listScenarios(prjdata)) &&
       (qn %in% listQueries(prjdata, scen_names, anyscen=FALSE)))
    {
        warning(paste("Skipping running query", qn, "since clobber is false and already exists in project."))
    } else {
        table <- runQuery(conn, query, scenario, regions)
        prjdata <- addQueryTable(prjdata, table, qn, clobber, transformations, saveProj)
    }

    if(saveProj) {
        saveProject(prjdata)
    }

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
#' @param scenstr The scenario string.  This may be a vector, such as the
#' 'scenario' column in a table of GCAM results
#' @return \code{list(scenario=Scenario_strings, date=Dates)}
#' @importFrom stringr str_split_fixed
#' @importFrom lubridate ydm_hms
#' @keywords internal
sep.date <- function(scenstr) {
    mtx <- stringr::str_split_fixed(scenstr,',date=',2)
    scenario <- mtx[,1]
    havedate <- mtx[,2] != ''
    date <- rep(as.POSIXct(NA), length(mtx[,2]))
    if(any(havedate)) {
        date[havedate] <- lubridate::ydm_hms(mtx[havedate,2]) # Don't try to do this
                                        # with ifelse, or you will be sad.
    }
    list(scenario=scenario, date=date)
}

#' Parse the GCAM ModelInterface output
#'
#' Parse the raw output of a GCAM batch query into a set of tables.
#'
#' @param fn Name of the file containing the output from the GCAM Model Interface.
#' @importFrom readr read_delim cols
#' @importFrom dplyr %>% matches mutate
#' @importFrom tidyr gather
#' @keywords internal
parse_mi_tables <- function(fn) {
    ## transplanted from the gcammaptools package.
    tables <- list()

    # See if the user has provided any values overriding our defaults
    use_tablenames <- TRUE
    headerline <- "scenario"
    yearpat <- "[0-9]{4}"

    ## The original version of this function had a bunch of logging commands.
    ## I've disabled these because I didn't want to bring the logging functions
    ## along, but I've left the commented logging commands in place in case we
    ## need them for debugging in the future.
    ##printlog("Reading", fn, "...", cr = F, level = LOGLEVEL_SUMMARY)
    tryCatch({
        fdata <- scan(fn, what = character(), sep = "\n", blank.lines.skip = F, quiet = T)
    }, error = function(err) {
        stop("error reading file ", as.character(err))
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
            nrows <- Inf
        } else {
            nrows <- tableheaders[i + 1] - tableheaders[i] - 1 - use_tablenames  # i.e., subtract 1 is using table names
        }

        ##printlog("Reading table", i, "in", fn, "( skip =", nskip, " nrows =", nrows, ")")
        tempdata <- read_delim(fn, delim = ",", skip = nskip, n_max = nrows, col_names = T,
                               comment = '#', col_types=cols()) # Setting default col_types suppresses col_types messages.

        # Remove extra columns on end - this is often present in the MI output
        if (extrafields > 0) {
            ##printlog("Removing", extrafields, "extra fields")
            tempdata <- tempdata[-seq(ncol(tempdata) - extrafields + 1, ncol(tempdata))]
        }

        ##printlog("Table", i, "name is", table_name)

        # Gather and clean up the year columns
        tempdata <- tempdata %>% gather(key=Year, value=value, matches(yearpat)) %>%
            mutate(Year=as.integer(Year))

        tables[[table_name]] <- tempdata
    }

    tables
}  # parse_mi_tables

#' Parse the GCAM ModelInterface output (DEPRECATED)
#'
#' Parse the raw output of a GCAM batch query and add them into a project.
#'
#' This function could be usefult for users who have exsisting CSV batch query
#' output they would like to import without having to re-run the queries.
#'
#' @param fn Name of the file containing the output from the GCAM Model
#' Interface.
#' @param proj Project to add extracted results to.  Can be either a project
#' data structure or the name of a project data file.  The file will be created
#' if it doesn't already exist.
#' @param clobber If \code{TRUE}, overwrite any existing scenario of the same
#' name; otherwise, fail if scenario/query already exists in the data set.
#' @param transformations Transformation functions to apply to the queries (see
#' details).
#' @param saveProj A flag to save the project to disk after data has been added.
#' A user may want to avoid it if they are for instance calling this method several
#' times and would prefer to save at the end.  Users can always save at anytime by
#' calling \code{saveProject}.
#' @param saveProjEach A flag to save the project to disk after each query has
#' completed. This would be useful if a user suspects a failure in the middle
#' of running queries and would like to not loose progress made.
#' @return The project dataset with the new scenario added.
#' @export
addMIBatchCSV <- function(fn, proj, clobber=FALSE, transformations=NULL,
                          saveProj=TRUE, saveProjEach=FALSE){
    proj <- loadProject(proj)

    q_tables <- parse_mi_tables(fn)

    # If no transformations are set just make it an empty list to simplify logic
    if(is.null(transformations)) {
        transformations <- list()
    }

    for(q in names(q_tables)) {
        proj <- addQueryTable(proj, q_tables[[q]], q, clobber, transformations[[q]], saveProj && saveProjEach)
    }

    if(saveProj) {
        saveProject(proj)
    }

    proj
}

#' Parse a Model Interface batch query file
#'
#' Given a Model Interface batch query file (\emph{i.e.,} an XML file detailing
#' queries that the user might wish to run), parse the file to produce a list of
#' queries that can be run by \code{\link{runQuery}}.
#'
#' The queries parsed from the batch query file are returned as a list of query
#' information struatures. Each structure \code{q} has elements \code{q$regions}
#' (regions specified in the query), \code{q$title} (title of the query), and
#' \code{q$query} (the XML specification of the query from the batch file).  The
#' \code{query} and \code{regions} items are suitable for passing to the
#' corresponding arguments of \code{runQuery}.  However, it is not
#' \emph{required} to use the \code{regions} argument; one can instead supply a
#' different list of regions, or no list at all (implicitly running all
#' regions), as described in the \code{\link{runQuery}} documentation.
#'
#' @param fn Name of the batch query file to parse
#' @return A list of query structures for the queries found in the batch file.
#' @importFrom xml2 read_xml xml_children xml_text xml_find_all xml_find_first xml_attr
#' @export
parse_batch_query <- function(fn) {
    batch_xml <- read_xml(fn)
    a_queries <- xml_children(batch_xml)
    query_list <- lapply(a_queries, function(a_query) {
        ret <- list()
        ret$regions <- xml_text(xml_find_all(a_query, "./region/@name"))
        ret$query <- xml_find_first(a_query, "./*[@title]")
        ret$title <- xml_attr(ret$query,"title")
        # turn the query into text so we do not need to worry about
        # errors such as "external pointer is not valid" if the xml2
        # library gets reloaded
        ret$query <- as.character(ret$query)
        return(ret)
    })
    names(query_list) <- sapply(query_list, function(a_query) {
        a_query$title
    })

    return(query_list)
}

## Apply the scenario trim and column standardize functions to a table
##
## This is just a helper function for use in lapply and similar constructs.  All
## it does is apply the two functions mentioned back to back.  It standardizes
## first to ensure lower case for the "scenario" column.
table.cleanup <- function(tbl)
{
    stdcase(tbl) %>% table.scen.trim
}

## Trim the 'date=' from the scenario column in a table
##
## Return a version of a GCAM results table in which the scenario name contains
## just the name of the scenario, without the date information that is typically
## packed into that column.
table.scen.trim <- function(tbl) {
    dplyr::mutate(tbl, scenario=sep.date(scenario)[['scenario']])
}

#' Standardize the case of a table's columns.
#'
#' GCAM is a little inconsistent about the case of the column names in its
#' tables.  Usually it's all lower case, but occasionally we get a 'Region'
#' instead of 'region'.  To keep analysis functions from having to guess at the
#' case they need to be looking for we standardize to all lower case, except for
#' the XYYYY year columns, and 'Units', which existing tools all expect to be in
#' title case.
#'
#' @param tbl The table to standardize
#' @keywords internal
stdcase <- function(tbl)
{
    lccols <- !grepl('^X[0-9]{4}', names(tbl))
    units <- grepl('units', names(tbl), ignore.case=TRUE)
    names(tbl)[lccols] <- tolower(names(tbl)[lccols])
    names(tbl)[units] <- 'Units'
    tbl
}

#' Default java class path for running the Model Interface
#'
#' This is the class path you get if you pass \code{NULL} to any of the
#' functions that run (directly or indirectly) the model interface.  The
#' necessary jar files are bundled with the package, so unless you need a
#' customized version of the model interface for some reason, this should take
#' care of all your GCAM query needs.
DEFAULT.MICLASSPATH <- paste0(system.file("ModelInterface", "jars",
                                          package="rgcam"),"/\\*",
                              .Platform$path.sep,
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
