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
#' @param regions An array of regions to query. An empty list will imply "all-regions".
#' @return A data.frame with the results.  TODO: what about errors?
#' @export
runQuery <- function(dbConn, query, scenarios, regions) UseMethod("runQuery")


#' Create a connection to a local database that can be use to run queries on.
#'
#' In order to establish we require the path in which the databases reside and
#' the name of the database to query.  Optionally you can specify a Java
#' classpath minimally including the ModelInterface.jar and BaseX.jar.  If no
#' classpath is given the version of ModelInterface and BaseX included in this
#' package will be used.
#'
#' @param dbPath The path in which the BaseX DBs are located.
#' @param dbFile GCAM database to extract scenario from.
#' @param miclasspath Java class path for the GCAM Model Interface.
#' @param migabble Control what happens to the model interface console output.
#' Default is to discard.
#' @return A connection to a local BaseX databasse which can be used to run
#' queries.
#' @export
localDBConn <- function(dbPath, dbFile, miclasspath=NULL, migabble=NULL) {
    if(is.null(miclasspath)) {
        miclasspath = DEFAULT.MICLASSPATH
    }
    if(is.null(migabble)) {
        migabble <- TRUE
    }
    db_inst <- structure(
        list(miclasspath=miclasspath, dbPath=dbPath, dbFile=dbFile, migabble=migabble),
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
    # The results for runMIQuery have not been aggregated (if for instance we are querying by region)
    # so we should do that now.
    results <- results %>%
        group_by_(.dots=paste0('`',names(results)[names(results) != "value"],'`')) %>%
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

#' Run query specialization for remote databases
#' @export
#' @importFrom httr POST authenticate http_error content
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
    url <- paste0("http://", dbConn$address, ':', dbConn$port, "/rest/", dbConn$dbFile)

    response <- POST(url, config=authenticate(dbConn$username, dbConn$password), body=restQuery )

    # error if the POST did not return with a success
    if(http_error(response)) {
        stop(content(response, "text"))
    }

    results <- content(response, "parsed")
    # The results for runMIQuery have not been aggregated (if for instance we are querying by region)
    # so we should do that now.
    results <- results %>%
        group_by_(.dots=paste0('`',names(results)[names(results) != "value"],'`')) %>%
        summarize(value=sum(value)) %>% ungroup()
    return(results)
}

