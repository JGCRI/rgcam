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

#' Lists the Scenarios contained in a GCAM Database
#'
#' To run a query users typically need to know the names of the scenarios in the
#' database.  If they are the ones to generate the data in the first place they
#' may already know this information.  Otherwise they could use this method to find
#' out.  The result of this call will be a table with columns \code{name}, \code{date}, \code{version}, and \code{fqName}.
#' The name and date are exactly as specified in the datbase. The fqName is the fully
#' qualified scenario name which a user could use in the scenarios argument of \code{runQuery}
#' if they need to disambiguate scenario names.  We also include the GCAM version tag that was used
#' to generate the scenario in the format: \code{ver_<major>.<minor>_r<git describe value>}.
#'
#' @param dbConn The connection to a database which will handle listing the scenarios.
#' @return A table with columns \code{name}, \code{date}, \code{version}, and \code{fqName} and rows for
#' each scenario in the database.
#' @export
listScenariosInDB <- function(dbConn)
    UseMethod("listScenariosInDB")


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
#' @param validatedb If \code{TRUE}, check that a simple db query works on the
#' connection; otherwise, don't run the check.
#' @param maxMemory Sets the maximum memory for Java which will be used to run
#' the queries.  The default value is \code{"4g"}.  Users may need to reduce this
#' value if they are using a 32-bit Java or increase it if they suspect they are
#' running out of memory (by using \code{migabble} to check the log).  Note the
#' numeric value can be suffixed with "g" for Gigabyte or "m" for Megabyte.
#' @return A connection to a local BaseX databasse which can be used to run
#' queries.
#' @export
localDBConn <- function(dbPath, dbFile, miclasspath=NULL, migabble=TRUE, validatedb=TRUE, maxMemory="4g") {
    if(is.null(miclasspath)) {
        miclasspath = DEFAULT.MICLASSPATH()
    }
    db_inst <- structure(
        list(miclasspath=miclasspath, dbPath=normalizePath(dbPath), dbFile=dbFile, migabble=migabble, maxMemory=maxMemory),
        class="localDBConn")

    if(validatedb) {
        ## Print the scenarios in the database.  This will also allow us to check
        ## whether the database is working
        dbscen <- listScenariosInDB(db_inst)

        if(nrow(dbscen) == 0) {
            stop('Database does not exist or is invalid: ',
                 file.path(dbPath, dbFile))
        }
        else {
            message('Database scenarios:  ', paste(dbscen$name, collapse=', '))
        }
    }

    return(db_inst)
}

#' @describeIn runQuery Run a query on a local GCAM database
#' @export
#' @importFrom readr read_csv
runQuery.localDBConn <- function(dbConn, query, scenarios=NULL, regions=NULL,
                                 warn.empty=TRUE) {
    xqScenarios <- ifelse(length(scenarios) == 0, "()", paste0("('", paste(scenarios, collapse="','"), "')"))
    xqRegion <- ifelse(length(regions) == 0, "()", paste0("('", paste(regions, collapse="','"), "')"))
    # strip newlines and excess space from queries to avoid errors on windows
    query <- gsub('\n', '', query)
    query <- gsub('\\s+', ' ', query)
    tmp_query_fn <- tempfile()
    tmp_output_fn <- tempfile()
    cmd_args <- c(
        paste("-cp", shQuote(dbConn$miclasspath)),
        paste0("-Xmx", dbConn$maxMemory),
        paste0("-Dorg.basex.DBPATH=", shQuote(dbConn$dbPath)),
        paste0("-DModelInterface.SUPPRESS_OUTPUT=", dbConn$migabble),
        "org.basex.BaseX",
        "-smethod=csv",
        "-scsv=header=yes,format=xquery",
        # send output to a temp file, see NOTE below
        paste0("-o", shQuote(tmp_output_fn)),
        paste0("-i", dbConn$dbFile),
        paste("RUN", tmp_query_fn, sep=" ")
        )
    tmp_query_conn <- file(tmp_query_fn, open = "w")
    cat(paste0("import module namespace mi = 'ModelInterface.ModelGUI2.xmldb.RunMIQuery';",
                       "mi:runMIQuery(", query, ",", xqScenarios, ",", xqRegion, ")"), file = tmp_query_conn, sep="\n")
    close(tmp_query_conn)
    if(dbConn$migabble) {
        suppress_col_spec <- readr::cols()
    }
    else {
        suppress_col_spec <- NULL
    }
    # NOTE: since readr 1.4 all connections will go through a temporary file before
    # being parsed.  Ideally we would stream this potentially large output, however
    # given readr doesn't support it and `pipe` is unreliable on Windows anyhow we
    # ask BaseX to write to a temporary file directly and read that into read_csv
    system2("java", args=paste(cmd_args, collapse=" "))
    results <- read_csv(tmp_output_fn, col_types=suppress_col_spec)
    unlink(tmp_query_fn)
    unlink(tmp_output_fn)

    ## The results for runMIQuery have not been aggregated (if for instance we
    ## are querying by region) so we should do that now.
    miquery_post(results, query, scenarios, regions, warn.empty)
}

#' @describeIn listScenariosInDB List scenarios in a local GCAM database
#' @export
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr mutate
listScenariosInDB.localDBConn <- function(dbConn) {
    tmp_output_fn <- tempfile()
    cmd_args <- c(
        paste("-cp", shQuote(dbConn$miclasspath)),
        paste0("-Xmx", dbConn$maxMemory),
        paste0("-Dorg.basex.DBPATH=", shQuote(dbConn$dbPath)),
        "org.basex.BaseX",
        "-smethod=csv",
        "-scsv=header=yes",
        # send output to a temp file, see NOTE below
        paste0("-o", shQuote(tmp_output_fn)),
        paste0("-i", dbConn$dbFile),
        shQuote("let $scns := collection()/scenario return document{ element csv { for $scn in $scns return element record { element name  { text { $scn/@name } }, element date { text { $scn/@date } }, element version { text{ $scn/model-version/text() } } } } }")
    )

    # NOTE: since readr 1.4 all connections will go through a temporary file before
    # being parsed.  Ideally we would stream this potentially large output, however
    # given readr doesn't support it and `pipe` is unreliable on Windows anyhow we
    # ask BaseX to write to a temporary file directly and read that into read_csv
    system2("java", args=paste(cmd_args, collapse=" "))
    result <- read_csv(tmp_output_fn, col_types=cols(name=col_character(), date=col_character(), version=col_character()))
    unlink(tmp_output_fn)
    if(nrow(result) > 0) {
        result <- mutate(result, fqName = paste(name, date, sep=" "))
    }

    result
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
#' @param validatedb If \code{TRUE}, check that a simple db query works on the
#' connection; otherwise, don't run the check.
#' @return A connection to a remote BaseX databasse which can be used to run
#' queries.
#' @export
remoteDBConn <- function(dbFile, username, password, address="localhost",
                         port=8984, validatedb=TRUE ) {
    db_inst <- structure(
        list(address=address, port=port, username=username, password=password, dbFile=dbFile),
        class="remoteDBConn")

    if(validatedb) {
        ## Print the scenarios in the database.  This will also allow us to check
        ## whether the database is working
        dbscen <- listScenariosInDB(db_inst)

        if(nrow(dbscen) == 0) {
            stop('Cannot read database : ', address, ':', port, 'user= ',
                 username, '  file= ', dbFile)
        }
        else {
            message('Database scenarios:  ', paste(dbscen$name, collapse=', '))
        }
    }
    return(db_inst)
}

#' @describeIn runQuery Run query specialization for remote databases
#' @export
#' @importFrom httr POST authenticate http_error content
runQuery.remoteDBConn <- function(dbConn, query, scenarios=NULL, regions=NULL,
                                  warn.empty=TRUE) {
    xqScenarios <- ifelse(length(scenarios) == 0, "()", paste0("('", paste(scenarios, collapse="','"), "')"))
    xqRegion <- ifelse(length(regions) == 0, "()", paste0("('", paste(regions, collapse="','"), "')"))
    # handle nested CDATA tags
    query <- gsub(']]>', ']]]]><![CDATA[>', query)
    restQuery <- paste(
        '<rest:query xmlns:rest="http://basex.org/rest">',
            '<rest:text><![CDATA[',
            paste0("import module namespace mi = 'ModelInterface.ModelGUI2.xmldb.RunMIQuery';",
                   "mi:runMIQuery(", query, ",", xqScenarios, ",", xqRegion, ")"),
            ']]></rest:text>',
            '<rest:parameter name="method" value="csv"/>',
            '<rest:parameter name="media-type" value="text/csv"/>',
            '<rest:parameter name="csv" value="header=yes,format=xquery"/>',
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

#' @describeIn listScenariosInDB List scenarios in a remote database
#' @export
#' @importFrom httr POST authenticate http_error content
#' @importFrom readr cols col_character
#' @importFrom dplyr mutate
listScenariosInDB.remoteDBConn <- function(dbConn) {
    restQuery <- paste(
        '<rest:query xmlns:rest="http://basex.org/rest">',
        '<rest:text><![CDATA[',
        'let $scns := collection()/scenario return document{ element csv { for $scn in $scns return element record { element name  { text { $scn/@name } }, element date { text { $scn/@date } }, element version { text{ $scn/model-version/text() } } } } }',
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

    result <- content(response, "parsed", col_types=cols(name=col_character(), date=col_character(), version=col_character()))
    if(nrow(result) > 0) {
        result <- mutate(result, fqName = paste(name, date, sep=" "))
    }

    result
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
#' @importFrom dplyr %>% group_by_at vars summarize ungroup
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
          group_by_at(vars(-value)) %>%
          summarize(value=sum(value)) %>% ungroup()
    }
    table.cleanup(results)
}
