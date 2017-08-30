################################################################################
#### querymi.R:  Functions for making queries through BaseX and ModelInterface
################################################################################

#' Run queries via a GCAM database connection and recieve results as data.frame
#'
#' To run a query users need to supply a database connection, the query to run,
#' the scenarios to query, and regions to query.
#'
#' @param dbConn The connection to a database which will handle running the query.
#' @param query A Model Interface query to run.  This is typically provided as the
#' XML typically found in the Main_queries.xml but could also be given for instance
#' as a query string that will result in XML such as:
#' \code{doc('../output/queries/Main_queries.xml')//*[@title='Cogeneration by sector']}
#' @param scenarios A character vector of scenarios to query. Passing
#' \code{NULL} in this argument will query the last scenario in the database.
#' Each value can either be the scenario name or the scenario name and date
#' seperated by a space.  Note the date is specified exactly as in appears in
#' the database.
#' @param regions A character vector of regions to query. \code{NULL} will
#' run the query over all regions.
#' @param warn.empty Flag: Issue a warning if a query is empty.
#' @return A data.frame with the results.
#' @export
runQuery <- function(dbConn, query, scenarios=NULL, regions=NULL,
                     warn.empty=TRUE)
    UseMethod("runQuery")


#' Create a connection that can be used to run queries on a local GCAM database
#'
#' Given a directory in which a GCAM database is located and the name of the
#' database, return a connection that can be used to run queries on the
#' database.
#'
#' By default, the a version of the GCAM ModelInterface and BaseX libraries
#' supplied with the package will be used to run the query.  You can replace
#' these by specifying a Java classpath minimally including the replacement
#' ModelInterface.jar and BaseX.jar files.
#'
#' @param dbPath The path in which the BaseX DBs are located.
#' @param dbFile GCAM database to extract scenario from.
#' @param miclasspath Java class path for the GCAM Model Interface.
#' @param migabble If \code{TRUE}, discard model interface console output.  If
#' \code{FALSE}, display console output.
#' @return A connection to a local BaseX databasse which can be used to run
#' queries.
#' @export
localDBConn <- function(dbPath, dbFile, miclasspath=NULL, migabble=TRUE) {
    if(is.null(miclasspath)) {
        miclasspath = DEFAULT.MICLASSPATH
    }
    db_inst <- structure(
        list(miclasspath=miclasspath, dbPath=normalizePath(dbPath), dbFile=dbFile, migabble=migabble),
        class="localDBConn")

    # TODO: ensure this connection is working

    return(db_inst)
}

#' @describeIn runQuery Run a query on a local GCAM database
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr %>% group_by_ summarize ungroup
runQuery.localDBConn <- function(dbConn, query, scenarios=NULL, regions=NULL,
                                 warn.empty=TRUE) {
    xqScenarios <- ifelse(length(scenarios) == 0, "()", paste0("('", paste(scenarios, collapse="','"), "')"))
    xqRegion <- ifelse(length(regions) == 0, "()", paste0("('", paste(regions, collapse="','"), "')"))
    # strip newlines from queries to avoid errors on windows
    query <- gsub('\n', '', query)
    cmd <- c(
        "java",
        paste("-cp", dbConn$miclasspath),
        "-Xmx2g", #TODO: memory limits?
        paste0("-Dorg.basex.DBPATH=", dbConn$dbPath),
        paste0("-DModelInterface.SUPPRESS_OUTPUT=", dbConn$migabble),
        "org.basex.BaseX",
        "-smethod=csv",
        "-scsv=header=yes",
        paste0("-i", dbConn$dbFile),
        shQuote(paste0("import module namespace mi = 'ModelInterface.ModelGUI2.xmldb.RunMIQuery';",
                       "mi:runMIQuery(", query, ",", xqScenarios, ",", xqRegion, ")"))
        )
    if(dbConn$migabble) {
        suppress_col_spec <- readr::cols()
    }
    else {
        suppress_col_spec <- NULL
    }
    results <- read_csv(pipe(paste(cmd, collapse=" ")), col_types=suppress_col_spec)
    ## The results for runMIQuery have not been aggregated (if for instance we
    ## are querying by region) so we should do that now.
    miquery_post(results, query, scenarios, regions, warn.empty)
}

#' Create a connection to a remote database that can be use to run queries on.
#'
#' This connection will attempt to connect to a remote BaseX server via a
#' webserver using the BaseX REST API.  In order to connect a user must supply
#' the server address, port, username/password as configured in the BaseX
#' server which as read access.
#' In addition we require TODO: optionally?) the name of the database to query.
#'
#' @param dbFile GCAM database to extract scenario from.
#' @param username A username configured with READ access on the remote BaseX
#' database server.
#' @param password The password for the said username. WARNING: currently just
#' stored and sent as plain text, does BaseX even support https?
#' @param address The server address such as IP or domain name address.  Default
#' is "localhost"
#' @param port The server port.  The default is 8984, the same as the default
#' used by BaseX.
#' @return A connection to a remote BaseX databasse which can be used to run
#' queries.
#' @export
remoteDBConn <- function(dbFile, username, password, address="localhost", port=8984 ) {
    db_inst <- structure(
        list(address=address, port=port, username=username, password=password, dbFile=dbFile),
        class="remoteDBConn")

    # TODO: ensure this connection is working

    return(db_inst)
}

#' @describeIn runQuery Run query specialization for remote databases
#' @export
#' @importFrom httr POST authenticate http_error content
#' @importFrom dplyr %>% group_by_ summarize ungroup
runQuery.remoteDBConn <- function(dbConn, query, scenarios=NULL, regions=NULL,
                                  warn.empty=TRUE) {
    xqScenarios <- ifelse(length(scenarios) == 0, "()", paste0("('", paste(scenarios, collapse="','"), "')"))
    xqRegion <- ifelse(length(regions) == 0, "()", paste0("('", paste(regions, collapse="','"), "')"))
    restQuery <- paste(
        '<rest:query xmlns:rest="http://basex.org/rest">',
            '<rest:text><![CDATA[',
            paste0("import module namespace mi = 'ModelInterface.ModelGUI2.xmldb.RunMIQuery';",
                   "mi:runMIQuery(", query, ",", xqScenarios, ",", xqRegion, ")"),
            ']]></rest:text>',
            '<rest:parameter name="method" value="csv"/>',
            '<rest:parameter name="media-type" value="text/csv"/>',
            '<rest:parameter name="csv" value="header=yes"/>',
        '</rest:query>' )
    url <- paste0("http://", dbConn$address, ':', dbConn$port, "/rest/", dbConn$dbFile)

    response <- POST(url, config=authenticate(dbConn$username, dbConn$password), body=restQuery )

    # error if the POST did not return with a success
    if(http_error(response)) {
        stop(content(response, "text"))
    }

    results <- content(response, "parsed")
    ## The results for runMIQuery have not been aggregated (if for instance we
    ## are querying by region) so we should do that now.

    miquery_post(results, query, scenarios, regions, warn.empty)
}

#' Post-process raw query results
#'
#' The results returned by the ModelInterface query are not aggregated (if, for
#' instance, the query is by region).  This function performs that aggregation,
#' if applicable.
#'
#' @param results Table returned by the ModelInterface query.
#' @param query The original query string.
#' @param scenarios The scenarios requested in the query.
#' @param regions The regions requested in the query.
#' @param warn.empty Flag: issue warning if the results table is empty.
#' @keywords internal
miquery_post <- function(results, query, scenarios, regions, warn.empty) {
    if(nrow(results) == 0) {
        if(warn.empty) {
            ## extract the title from the query string
            m <- regexec('title="([^"]*)"', query)
            qtitle <- regmatches(query, m)[[1]][2] # first capture group is in
                                        # the second element (whole match is in
                                        # the first)
            warning('Query returned empty table:\nquery: ', qtitle, '\nscenarios: ',
                    scenarios, '\nregions: ', regions)
        }
    }
    else {
        results <- results %>%
          group_by_(.dots=paste0('`',names(results)[names(results) != "value"],'`')) %>%
          summarize(value=sum(value)) %>% ungroup()
    }
    results
}
