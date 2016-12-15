################################################################################
#### obsolete.R: Obsolete functions that may still be used in some older code
################################################################################


#' Extract a query from a list of queries and filter to desired output.
#'
#' Extract the desired table from the structure produced by
#' \code{\link{parse_mi_output}}.  Optionally, perform some filtering
#' and transformation on the data.  NB:  This function is obsolete and is
#' included only for backward compatibility with older code.  See details for
#' alternatives.
#'
#' Though this function is not (yet) officially deprecated, it should't be used
#' in new code.  It isn't actually needed for retrieving tables; use
#' \code{projectData[[scenario]][[queryName]]} for that.  Likewise, the
#' filtering and aggregating capabilities are a little arcane.  It's better just
#' to use \code{dplyr} to make whatever transformations you need to make.  You
#' can also make the transformations permanent by applying them at import time
#' using the \code{transformations} argument to \code{\link{addScenario}}.
#'
#' @param batchq The structure containing the GCAM results (produced
#' by \code{\link{parse_mi_output}}).
#' @param query The name of the table to extract; i.e., the name of
#' one of the queries cointained in the GCAM output.
#' @param scen The name of the scenario.  Partial matches are allowed.
#' @param filters A named vector of filtering criteria in the form
#' \code{c(header1=value1, header2=value2,...)}.  Headers are the
#' names of columns in the data frame.  If aggregating data, use the
#' value 'Aggregate'.  (XXX: Needs further explanation!)
#' @param func Operation to apply to the aggregated data.  (XXX: Does
#' this mean that this option is active only when using the
#' 'Aggregate' option above?)
#' @export
process_batch_q <- function(batchq, query, scen, filters, func = sum) {

    qdata <- as.data.frame(batchq[[query]])

    # Filter for scenario; allow partial lookup
    # Bug: if partial has multiple matches, will return multiple scenarios
    qdata <- qdata[grepl(scen, qdata$scenario), ]

    # Get years and aggregate value if applicable
    years <- grep("(X1)|(X2)", names(qdata), value = T)
    ag <- names(filters[filters[names(filters)] == "Aggregate"])  #Super clunky

    nms <- !(names(qdata) %in% years | names(qdata) %in% ag)

    # Filter to query of interest using filters
    for (name in names(filters)) {
        if (filters[[name]] == "Aggregate") {
            qdata <- aggregate(qdata[years], by = qdata[nms], FUN = func)
            qdata[[ag]] <- "All"
        } else {
            qdata <- qdata[qdata[[name]] == filters[[name]], ]
        }
    }

    return(qdata)
}
