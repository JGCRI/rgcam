#' Extract GCAM data from GCAM databases and import into R
#'
#' The rgcam package provides functions for extracting GCAM data from GCAM
#' output databases and importing it into R for analysis. The central concept in
#' rgcam is the "project data file", which contains an R-native representation
#' of selected queries for one or more scenarios. The package provides functions
#' to run the GCAM Model Interface to extract data and add the results to a new
#' or existing project data file, as well as to manage previously created
#' project data.
#'
#' @section Usage:
#'
#' You can create or add to a project file using the `addScenario`
#'function.  You will need a GCAM output database and an XML file
#'containing the queries that you want to extract from it.  You could
#'then get started by running, for example:
#'\verb{
#'
#'project.data <- addScenario('gcamdb_basexdb', 'project.dat', 'scenario-name'
#'                            'queries.xml')
#'
#'}
#'This command would run the queries in `queries.xml` against the
#'database `gcamdb_basexdb`, extract the results for a scenario called
#'"scenario-name", and write the results to a file called "project.dat".
#'If you have other scenarios, whether in the same database or a
#'different one, you can make additional calls to `addScenario` to add
#'them to the project data.  The results are also returned and assigned
#'to `project.data` so that you can begin working with them.
#'
#'If you already have a project data file that you want to work with,
#'you can load it using `loadProject`:
#'\verb{
#'
#'project.data <- loadProject('project.dat')
#'
#'}
#'
#'There are also functions to list scenarios and queries and to pull
#'queries from the data set:
#'\verb{
#'
#'## List scenarios and queries
#'scenarios <- listScenarios(project.data)
#'queries <- listQueries(project.data, 'scenario-name')
#'## Retrieve a query for all scenarios in the dataset,
#'## formatted as a single table
#'co2 <- getQuery(project.data, 'query-name')
#'
#'}
#'
"_PACKAGE"
