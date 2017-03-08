################################################################################
#### querymi.R:  Functions for making queries through BaseX and ModelInterface
################################################################################

#' Run queries via a database connection and recieve results as data.frame.
#'
#' To run a query users need to supply a database connection, the query to run,
#' the scenarios to query, and regions to query.
#'
#' @param dbConn The connection to a database which will handle running the query.
#' @param query A Model Interface query to run.  This is typically provided as the
#' XML typically found in the Main_queries.xml but could also be given for instance
#' as a query string that will result in XML such as:
#' \code{collection('../output/queries/Main_queries.xml')//*[@title='Cogeneration by sector']}
#' @param scenarios An array of scenarios to query. TODO: more about syntax
#' @param regions An array of regions to query. TODO: allow null to mean all?
#' @return A data.frame with the results.  TODO: what about errors?
#' @export
runQuery <- function(dbConn, query, scenarios, regions) UseMethod("runQuery")


#' Create a connection to a local database that can be use to run queries on.
#'
#' In order to establish a so called connection, really we track the location
#" of all the ModelInterface and BaseX Java Jar files (and their dependent jar
#' files as well) which is the classpath TODO: link to documentation and syntax
#' In addition we require the path in which the databases reside and (TODO:
#' optionally?) the name of the database to query.
#'
#' @param miclasspath Java class path for the GCAM Model Interface.
#' @param dbPath The path in which the BaseX DBs are located.
#' @param dbFile GCAM database to extract scenario from.
#' @param migabble Control what happens to the model interface console output.
#' This is passed to the stdout/stderr argument of \code{system2}.  Default is
#' to discard.
#' @return A connection to a local BaseX databasse which can be used to run
#' queries.
#' @export
localDBConn <- function(miclasspath, dbPath, dbFile) {
    db_inst <- structure(
        list(miclasspath=miclasspath, dbPath=dbPath, dbFile=dbFile),
        class="localDBConn")

    # TODO: ensure this connection is working

    return(db_inst)
}

#' Run query specialization for local databases
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr %>% group_by_ summarize ungroup
runQuery.localDBConn <- function(dbConn, query, scenarios, regions) {
    xqScenarios <- ifelse(length(scenarios) == 0, "()", paste0("('", paste(scenarios, collapse="','"), "')"))
    xqRegion <- ifelse(length(regions) == 0, "()", paste0("('", paste(regions, collapse="','"), "')"))
    cmd <- c(
        "java",
        paste("-cp", dbConn$miclasspath),
        "-Xmx2g", #TODO: memory limits?
        paste0("-Dorg.basex.DBPATH=", dbConn$dbPath),
        "org.basex.BaseX",
        "-smethod=csv",
        "-scsv=header=yes",
        paste0("-i", dbConn$dbFile),
        shQuote(paste0("import module namespace mi = 'ModelInterface.ModelGUI2.xmldb.RunMIQuery';",
                       "mi:runMIQuery(", query, ",", xqScenarios, ",", xqRegion, ")"))
        )
    results <- read_csv(pipe(paste(cmd, collapse=" ")))
    # The results for runMIQuery have not been aggregated (if for instance we are querying by region)
    # so we should do that now.
    results <- results %>%
        group_by_(.dots=names(results)[names(results) != "value"]) %>%
        summarize(value=sum(value)) %>% ungroup()
    return(results)
}

#' Create a connection to a remote database that can be use to run queries on.
#'
#' This connection will attempt to connect to a remote BaseX server via a
#' webserver using the BaseX REST API.  In order to connect a user must supply
#' the server address, port, username/password as configured in the BaseX
#' server which as read access.
#' In addition we require TODO: optionally?) the name of the database to query.
#'
#' @param address The server address such as IP or domain name address.
#' @param port The server port.
#' @param username A username configured with READ access on the remote BaseX
#' database server.
#' @param password The password for the said username. WARNING: currently just
#' stored and sent as plain text, does BaseX even support https?
#' @param dbFile GCAM database to extract scenario from.
#' @return A connection to a remote BaseX databasse which can be used to run
#' queries.
#' @export
remoteDBConn <- function(address, port, username, password, dbFile) {
    db_inst <- structure(
        list(address=address, port=port, username=username, password=password, dbFile=dbFile),
        class="remoteDBConn")

    # TODO: ensure this connection is working

    return(db_inst)
}

#' Run query specialization for remote databases
#' @export
#' @importFrom httr POST http_error content
#' @importFrom dplyr %>% group_by_ summarize ungroup
runQuery.remoteDBConn <- function(dbConn, query, scenarios, regions) {
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
    url <- paste0("http://", dbConn$username, ":", dbConn$password, "@",
                  dbConn$address, ':', dbConn$port, "/rest/", dbConn$dbFile)

    response <- POST(url, body=restQuery)

    # error if the POST did not return with a success
    if(http_error(response)) {
        stop(content(response, "text"))
    }

    results <- content(response, "parsed")
    # The results for runMIQuery have not been aggregated (if for instance we are querying by region)
    # so we should do that now.
    results <- results %>%
        group_by_(.dots=names(results)[names(results) != "value"]) %>%
        summarize(value=sum(value)) %>% ungroup()
    return(results)
}

