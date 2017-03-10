## Functions implementing commonly used data transformations

#' Compute a difference between two scenarios
#'
#' This function takes a single query for two scenarios and computes the
#' difference between them for all years in the scenario. Entries that exist in
#' one of the scenarios but not the other will be assigned a NA value in the
#' output.
#'
#' The input data should consist of a data frame with a \code{scenario} column,
#' one or more id columns (\code{region}, \code{sector}, etc.), and a series of
#' columns containing the data.  We assume that any column with numeric data in
#' it is a data column.  Any columns other than the key columns and data columns
#' will be dropped as part of the aggregation.
#'
#' By default, both scenarios are expected to be contained in a single data
#' frame, distinguished by the \code{scenario} column.  There can be more than
#' two scenarios in the data frame; the \code{scenarios} argument will determine
#' which ones actually get used.  The \code{scenarios} argument also determines
#' the order of the differencing (i.e., \code{scenario1 - scenario2}
#' vs. \code{scenario2 - scenario1}.
#'
#' By default the data is aggregated by region before differencing, but this
#' behavior can be changed with the \code{keycols} argument.  You can specify
#' one or more key columns to change the aggregation variable, or you can set
#' \code{keycols=NULL} to retain all of the input columns and not do any
#' aggregation.  Aggregation is by summing, by default, but an alternative
#' aggregation function can be specified in the \code{aggfun} argument.
#'
#' If you happen to have the data for the two scenarios in separate data frames,
#' you can pass the second data frame in the \code{data2} argument.  In this
#' case, the \code{scenarios} argument will be ignored, and the difference will
#' be computed as \code{data - data2}.
#'
#' @param data The input query data.  The expected format is described below.
#' @param scenarios The scenarios to difference from the input data.  Order is
#' significant; it determines which scenario will be subtracted, and which will
#' be subtracted from.
#' @param keycols Columns by which to aggregate the data.  The default is to
#' aggregate by region.  If \code{NULL}, then don't do any aggregation.
#' @param data2 If the two scenarios are in separate data frames, pass the
#' scenario to be subtracted in this argument.  In this case, the
#' \code{scenarios} argument will be ignored.
#' @param aggfun Function to use in aggregation.  The default is \code{sum}.
#' @return Data frame with the difference between the two scenarios.  The key
#' columns and year columns will be retained; all other columns (including the
#' \code{scenario} column will be dropped.
#' @importFrom dplyr %>%
#' @export
diffScenarios <- function(data, scenarios=NULL, keycols='region', data2=NULL,
                          aggfun=sum) {
    ## Split data.  If data2 was supplied, then this is not necessary.
    if(is.null(data2)) {
        if(is.null(scenarios)) {
            stop('diffScenarios: ',
                 '`scenarios` argument is required if only one data frame given.')
        }
        if(length(scenarios) != 2) {
            stop('diffScenarios: ',
                 'You must specify exactly two scenarios to difference.')
        }
        data <- dplyr::filter_(data, ~scenario %in% scenarios)
    }
    else {
        scenarios <- c(unique(data[['scenario']]), unique(data2[['scenario']]))
        if(length(scenarios) != 2) {
            stop('diffScenarios: ',
                 'when data and data2 are both supplied, each must contain exactly one scenario.')
        }
        data <- rbind(data, data2)
    }

    ## perform aggregation, if required
    if(is.null(keycols)) {
        aggdata <- data
        lcols <- !sapply(aggdata,is.numeric) & names(aggdata) != 'scenario'
        keycols <- names(aggdata)[lcols]
    }
    else {
        idcols <- c('scenario', keycols)
        aggdata <- dplyr::group_by_(data,.dots=idcols) %>%
            dplyr::summarise_if(is.numeric, aggfun)
    }

    ## We have to arrange rows by scenario to ensure that the subtraction gets
    ## done in the right order.
    if(scenarios[1] < scenarios[2])
        aggdata <- dplyr::arrange_(aggdata, "scenario")
    else
        aggdata <- dplyr::arrange_(aggdata, "desc(scenario)")

    ## At this point each unique combination of keycols will have (at most) two
    ## entries: one for each scenario.  Group by the key columns and perform a
    ## subtraction reduction.
    dplyr::ungroup(aggdata) %>% dplyr::group_by_(.dots=keycols) %>%
        dplyr::summarise_if(is.numeric, rsub)
}

#' Perform a difference reduction
#'
#' This is the subtraction equivalent of sum(...).  Specifically, rsub(x1, x2,
#' ..., xn) is equivalent to x1 - sum(x2, ... xn).  However, we make one
#' exception:  if only a single argument is passed, we return NA.  This is so
#' that the result makes sense in \code{\link{diffScenarios}}.
#'
#' @param ... The values to be reduced
#' @return The difference reduction of the values, with special cases handled as
#' described in details.
#' @keywords internal
rsub <- function(...) {
    args <- c(...)
    if(length(args) < 2)
        NA
    else {
        x <- args[1]
        rest <- args[-1]
        x - sum(rest)
    }
}


