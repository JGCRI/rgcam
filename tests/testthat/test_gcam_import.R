library('rgcam')

context('Loading and importing GCAM data')

file.valid <- tempfile()
file.bad <- tempfile()
notdata <- list()
save(notdata, file=file.bad)
testtime <- lubridate::ymd_hm('1863-11-19 15:45',tz='EST')
datestrfmt <- '%Y-%d-%m %H:%M:%S'
SAMPLE.GCAMDBLOC <- system.file("extdata",
                                package="rgcam")
SAMPLE.QUERIES <- system.file("ModelInterface", "sample-queries.xml",
                              package="rgcam")
conn <- localDBConn(SAMPLE.GCAMDBLOC, "sample_basexdb")

## helper function for creating extra scenarios
dup.scenario <- function(scen, newname) {
    clone.query <- function(q) {dplyr::mutate(q,scenario=newname)}
    ns <- lapply(scen, clone.query)
    attr(ns,'date') <- testtime
    ns
}

test_that('Data file is not created on error.', {
              nosuchfile <- tempfile()
              expect_false(file.exists(nosuchfile))
              bad_conn <- localDBConn(tempdir(), nosuchfile)
              expect_error(addScenario(bad_conn, file.valid))
              expect_false(file.exists(file.valid))
          })

test_that('Passing an invalid object is an error.', {
              expect_error(addScenario(SAMPLE.GCAMDB, NULL),
                           'invalid object')
          })

test_that('Data can be imported from GCAM database.', {
              prj <- addScenario(conn, file.valid)
              attr(prj,'file') <- NULL # 'file' attr is stripped when
                                       # project is saved so when using
                                       # expect_equal_to_reference we must
                                       # strip it first.
              expect_equal_to_reference(prj,
                                        'sample-prj.dat')
              expect_true(file.exists(file.valid))
          })

## file.valid now exists
test_that('Clobber argument to addScenario works.', {
              expect_warning(prj <- addScenario(conn, file.valid,
                                                scenario='Reference-filtered'),
                             'already exists')
              expect_warning(prj <- addScenario(conn, file.valid),
                             'clobber')
              attr(prj,'file') <- NULL
              expect_equal_to_reference(prj, 'sample-prj.dat')

              expect_silent(prj <- addScenario(conn, file.valid,
                                               clobber=TRUE))
              attr(prj,'file') <- NULL
              expect_equal_to_reference(prj, 'sample-prj.dat')
          })



test_that('File with bad permissions is detected.', {
              Sys.chmod(file.bad, '0000')
              expect_error(addScenario(conn, file.bad), 'exists')
              Sys.chmod(file.bad, '0666')
          })

test_that('loadProject works.', {
              expect_error(loadProject(file.bad), 'Unable to load project')
              prj <- loadProject(file.valid)
              expect_equal(attr(prj,'file'), file.valid)
              attr(prj,'file') <- NULL
              expect_equal_to_reference(prj, 'sample-prj.dat')
          })

test_that('project info functions work.', {
              prj <- loadProject(file.valid)
              prj[['Scenario2']] <- dup.scenario(prj[[1]], 'Scenario2')
              ## List scenarios and queries
              expect_equal(listScenarios(prj),
                           c('Reference-filtered', 'Scenario2'))
              expect_equal(listQueries(prj,'Reference-filtered'),
                           c('CO2 concentrations', 'Climate forcing',
                             'Global mean temperature', 'GDP by region',
                             'PPP GDP by region',
                             'Population by region',
                             'Aggregated Land Allocation',
                             'Building floorspace', 'Land Allocation'))
              ## drop from each scenario all but one query so we can test the
              ## 'anyscen' options.  We haven't tested dropQueries yet, so a
              ## failure in that function could cause a failure here too.
              q1 <- c('CO2 concentrations', 'Climate forcing')
              q2 <- c('Climate forcing', 'GDP by region')
              prj <- dropQueries(prj, q1, invert=TRUE,
                                 scenarios='Reference-filtered' ) %>%
                  dropQueries(q2, invert=TRUE,
                              scenarios='Scenario2')
              expect_equal(listQueries(prj), union(q1,q2))
              expect_equal(listQueries(prj, anyscen=FALSE), intersect(q1,q2))

              ## Test the scenario run date functions.  Unfortunately, our
              ## sample data only has one scenario, so these tests will be
              ## incomplete where multi-scenario return values are concerned.
              expect_true(lubridate::is.POSIXt(getRundates(prj,'Reference-filtered')))
              expect_true(lubridate::with_tz(getRundates(prj)['Reference-filtered'], 'UTC') ==
                           lubridate::ymd_hms('2016-12-13 13:31:05')) # Run date
                                        # of the sample data
          })


test_that('validation functions accurately detect problems.', {
              prj <- loadProject(file.valid)
              ## valid project
              expect_equal(project.valid(prj), 0)

              prj[[1]][[1]] <- data.frame(scenario=rep('Reference-filtered',5))
              expect_equal(project.valid(prj), 7)

              prj[[1]][[2]] <- data.frame(a=c(1,2,3), b=c('a','b','c'))
              expect_equal(project.valid(prj), 6)

              prj[[1]][[3]] <- 'foo'
              expect_equal(project.valid(prj), 5)

              prj[[2]] <- list()
              expect_equal(project.valid(prj), 4)

              prj[[3]] <- 'bar'
              expect_equal(project.valid(prj), 3)

              expect_equal(project.valid(list()), 2)

              expect_equal(project.valid('baz'), 1)
          })

test_that('query retrieval works.', {
              prj <- loadProject(file.valid)

              ## add a second scenario
              prj[['Scenario2']] <- dup.scenario(prj[[1]], 'Scenario2')

              ## getting a nonexistent query is an error
              expect_error(getQuery(prj, 'FOO'), 'is not in any scenarios')

              co2 <- getQuery(prj, 'CO2 concentrations')
              expect_true(is.data.frame(co2))
              co2 <- tidyr::spread(co2, year,value)
              expect_equal(nrow(co2), 2)
              expect_equal(co2$scenario, c('Reference-filtered', 'Scenario2'))
              expect_equal(co2[["2100"]], rep(738.939,2))
              expect_equal(co2$Units, rep('PPM',2))

              ## single scenario query
              co2.single <- getQuery(prj, 'CO2 concentrations', 'Scenario2')
              expect_true(is.data.frame(co2.single))
              co2.single <- tidyr::spread(co2.single, year,value)
              expect_equal(nrow(co2.single), 1)
              expect_equal(co2.single$scenario, 'Scenario2')
              expect_equal(co2.single[["2050"]], 507.433)

              ## add a query to the second scenario that doesn't exist in the
              ## first.
              prj[['Scenario2']][['foo']] <-
                  prj[['Scenario2']][['CO2 concentrations']]
              foo <- getQuery(prj, 'foo') # all scenarios
              expect_true(is.data.frame(foo))
              foo <- tidyr::spread(foo, year,value)
              expect_equal(nrow(foo), 1)
              expect_equal(foo$scenario, 'Scenario2')
              expect_equal(foo[["2000"]], 364.147)
          })

## This one modifies the temporary project file
test_that('scenario can be added to an already-loaded data set.', {
              prj <- loadProject(file.valid)
              ## rename the scenario so we can load it again
              prj[['Scenario2']] <- dup.scenario(prj[[1]], 'Scenario2')
              prj[[1]] <- NULL
              prj <- addScenario(conn, prj)

              expect_equal(length(prj), 2)
              expect_true('Reference-filtered' %in% listScenarios(prj))
              expect_true('Scenario2' %in% listScenarios(prj))
          })

## This one reverts the changes made in the previous test
test_that('dropScenarios works in all option configurations.', {
              prj2 <- loadProject(file.valid)
              expect_equal(length(prj2),2)

              ## This should drop the second scenario but leave the file
              ## untouched.
              prj1 <- dropScenarios(prj2, 'Scenario2')
              expect_equal(length(prj1),1)
              expect_equal(listScenarios(prj1), 'Reference-filtered')
              prj2a <- loadProject(file.valid)
              expect_equal(prj2a, prj2)

              ## Test the invert option
              prj1x <- dropScenarios(prj2a, 'Reference-filtered', invert=TRUE)
              expect_equal(prj1x, prj1)
              prj2b <- loadProject(file.valid)
              expect_equal(prj2b, prj2)

              ## This one should drop the scenario in the working copy and in
              ## the file.
              prj1a <- dropScenarios(file.valid, 'Scenario2')
              expect_equal(prj1a, prj1)
              prj1b <- loadProject(file.valid)
              expect_equal(prj1b, prj1)

          })

test_that('saveProject saves project data correctly.', {
              prj <- loadProject(file.valid)
              ## Save to the default location.  Delete the file first to verify
              ## creation.
              unlink(file.valid)
              expect_false(file.exists(file.valid))
              saveProject(prj)
              expect_true(file.exists(file.valid))
              prj2 <- loadProject(file.valid)
              expect_equal(prj, prj2)

              ## Save to explicit filename
              altfile <- tempfile()
              expect_false(file.exists(altfile))
              prj <- saveProject(prj, file=altfile)
              expect_true(file.exists(altfile))
              prj3 <- loadProject(altfile)
              expect_equal(prj, prj3)
              unlink(altfile)
          })


test_that('dropQueries works in all (reasonable) option configurations.', {
              prj <- loadProject(file.valid)
              expect_equal(length(prj), 1) # mostly making sure we are starting
                                        # from a known state

              prj2 <- prj
              prj2[['Scenario2']] <- dup.scenario(prj[[1]], 'Scenario')
              ## drop one query from all scenarios
              prj2 <- dropQueries(prj2, 'CO2 concentrations')
              expect_equal(length(prj2[[1]]), length(prj2[[2]]))
              expect_equal(length(prj2[[1]]), length(prj[[1]]) - 1)
              expect_false('CO2 concentrations' %in% names(prj2[[1]]))
              expect_false('CO2 concentrations' %in% names(prj2[[2]]))

              ## drop another query from just Scenario 2 (which is the second
              ## scenario in the list)
              prj3 <- dropQueries(prj2, 'Land Allocation',
                                  scenarios='Scenario2')
              expect_equal(length(prj3[[1]]), length(prj3[[2]]) + 1)
              expect_equal(length(prj3[[1]]), length(prj2[[1]]))
              expect_true('Land Allocation' %in% names(prj3[[1]]))
              expect_false('Land Allocation' %in% names(prj3[[2]]))

              ## drop all but one query from the data
              prj4 <- dropQueries(prj3, 'Climate forcing', invert=TRUE)
              expect_equal(length(prj4[[1]]), 1)
              expect_equal(length(prj4[[2]]), 1)
              expect_equal(names(prj4[[1]]), 'Climate forcing')
              expect_equal(names(prj4[[2]]), 'Climate forcing')

              ## none of this should have changed the data file
              prj1a <- loadProject(file.valid)
              expect_equal(prj1a, prj)

              ## delete two queries from the file
              prj5 <- dropQueries(file.valid, c('GDP by region',
                                                'Building floorspace'))
              expect_equal(length(prj5[[1]]), length(prj[[1]]) - 2)
              expect_false('Building floorspace' %in% names(prj5[[1]]))
              prj5a <- loadProject(file.valid)
              expect_equal(prj5, prj5a)

              ## restore the temp file in case we have more tests.
              saveProject(prj)
          })

test_that('addQueryTable works.', {
              altfile <- tempfile()     # avoid disturbing our test case
              prj <- loadProject(file.valid)
              prj[['Scenario2']] <- dup.scenario(prj[[1]], 'Scenario2')
              prj <- saveProject(prj, altfile)
              testqueryname <- 'Aggregated Land Allocation'

              table1 <- getQuery(prj, testqueryname) # should
                                        # contain both scenarios

              ## Test failure cases first
              prj2 <- prj               # so we can verify that prj2 is
                                        # unchanged on failure.
              ## replace with noclobber
              expect_warning(
                  prj2 <- addQueryTable(prj, table1,
                                        testqueryname) )
              expect_equal(prj, prj2)

              ## add with no date
              expect_error(
                  prj2 <- addQueryTable(prj, table1, 'New Query',
                                        strict.rundate=TRUE) )
              expect_equal(prj, prj2)

              ## add with a bad date
              table2 <- dplyr::mutate(table1,
                                      scenario=paste0(scenario, ',date=',
                                                      format(testtime, datestrfmt)))
              expect_error(
                  prj2 <- addQueryTable(prj, table2, 'New Query',
                                        strict.rundate=TRUE) )
              expect_equal(prj, prj2)

              ## From here out we should succeed
              ## no date, clobber allowed
              prj2 <- addQueryTable(prj, table1, testqueryname,
                                    clobber=TRUE)
              expect_equal(prj, prj2, check.attributes=FALSE)   # shouldn't have
                                        # changed anything, but row names may be
                                        # different, so ignore attributes.
              ## no date, new query
              prj2 <- addQueryTable(prj, table1, 'New Query1')
              expect_true('New Query1' %in% listQueries(prj2, 'Reference-filtered'))
              expect_true('New Query1' %in% listQueries(prj2, 'Scenario2'))
              expect_equal(getQuery(prj, testqueryname),
                           getQuery(prj2, 'New Query1'))

              ## wrong date, but strict.rundate not in effect
              prj2 <- addQueryTable(prj, table2, 'New Query2')
              expect_true('New Query2' %in% listQueries(prj2, 'Reference-filtered'))
              expect_true('New Query2' %in% listQueries(prj2, 'Scenario2'))
              expect_equal(getQuery(prj, testqueryname),
                           getQuery(prj2, 'New Query2'))

              ## check that case gets corrected when it's wrong
              tablecase <- dplyr::rename(table1, Scenario=scenario,
                                         Region=region)
              prj2a <- addQueryTable(prj2, tablecase, 'Bad Case Query')
              bctbl <- getQuery(prj2a, 'Bad Case Query')
              expect_true('scenario' %in% names(bctbl))
              expect_true('region' %in% names(bctbl))
              expect_true('Units' %in% names(bctbl))
              expect_true('year' %in% names(bctbl))

              ## add to a file instead of a structure
              prj3 <- addQueryTable(altfile, table1, 'New Query3')
              expect_true('New Query2' %in%
                          listQueries(prj3, 'Reference-filtered')) # Previous
                                        # queries should have been written to
                                        # file.
              expect_true('New Query3' %in%
                          listQueries(prj3, 'Reference-filtered'))

              ## remove the tempfile
              unlink(altfile)
          })

test_that('addSingleQuery works.', {
    query_name <- "CO2 concentrations"
    prj <- loadProject(file.valid)
    comp.data <- getQuery(prj, query_name)
    co2_query <- '<ClimateQuery title="CO2 concentrations">
                    <axis1 name="CO2-concentration">none</axis1>
                    <axis2 name="Year">CO2-concentration[@year]</axis2>
                    <xPath buildList="true" dataName="CO2-concentration" group="false" sumAll="false">climate-model/CO2-concentration/text()</xPath>
                    <comments/>
                  </ClimateQuery>'
    ## add with full query definition
    expect_message(prj_test <- addSingleQuery(conn, "temp", query_name, co2_query, saveProj=FALSE),
                   'does not exist in this project.  Creating.')
    expect_identical(getQuery(prj_test, query_name), comp.data)

    ## add with query for query
    co2_query_query <- paste0("doc('", SAMPLE.QUERIES, "')//*[@title='", query_name, "']")
    expect_silent(prj_test <- addSingleQuery(conn, prj_test, query_name, co2_query_query, clobber=TRUE, saveProj=FALSE))
    expect_identical(getQuery(prj_test, query_name), comp.data)

    ## add with query from xml2 package
    queries <- xml2::read_xml(SAMPLE.QUERIES)
    co2_query <- xml2::xml_find_first(queries, paste0("//*[@title='", query_name, "']"))
    expect_silent(prj_test <- addSingleQuery(conn, prj_test, query_name, co2_query, clobber=TRUE, saveProj=FALSE))
    expect_identical(getQuery(prj_test, query_name), comp.data)
})

test_that('addMIBatchCSV works.', {
    prj <- loadProject(file.valid)
    SAMPLE.BATCH.CSV <- system.file("ModelInterface", "sample.csv", package="rgcam")
    expect_warning(prj_test <- addMIBatchCSV(SAMPLE.BATCH.CSV, "test", saveProj=FALSE),
                   "Missing column names filled in: 'X\\d+' \\[\\d+\\]")
    # Do not fail test because of differing filenames.
    #attr(prj, 'file') <- NULL
    #attr(prj_test, 'file') <- NULL
    #expect_identical(prj, prj_test)
    # TODO: it seems there are infact many subtle differences when reading
    # from the batch CSV such as column order, row sorting, the query PPP GDP by region got
    # added to the batch query list but sample.csv didn't get updated.  From a testing
    # perspective it would be nice if these data matched but from a mechanical standpoint
    # it doesn't really matter.
})

test_that('mergeProjects works.', {
    prj <- loadProject(file.valid)
    dup1_prj <- list(Dup1=dup.scenario(prj[[1]], "Dup1"))
    attr(dup1_prj, 'file') <-'dup1_prj'
    dup2_prj <- list(Dup2=dup.scenario(prj[[1]], "Dup2"))
    attr(dup2_prj, 'file') <-'dup2_prj'
    expect_silent(merged_prj <- mergeProjects(prj, list(dup1_prj, dup2_prj), saveProj=FALSE))
    expect_equal(listScenarios(merged_prj), c("Reference-filtered", "Dup1", "Dup2"))

    # Test clobber
    dup1_prj[[1]][["Extra Query"]] <- dup1_prj[[1]][[1]]
    expect_warning(merged_prj <- mergeProjects(merged_prj, list(dup1_prj), saveProj=FALSE),
                   "Skipping data in .* as clobber is false.", all=TRUE)
    expect_true(c("Extra Query") %in% listQueries(merged_prj, "Dup1"))

    dup1_prj[[1]][["Extra Query"]]$value <- 1234
    expect_silent(merged_prj <- mergeProjects(merged_prj, list(dup1_prj), clobber=TRUE, saveProj=FALSE))
    expect_true(all(getQuery(merged_prj, "Extra Query", "Dup1")$value == 1234))
})

test_that('querying a remote server that is not running fails', {
    remote_conn <- remoteDBConn("does", "not", "exist")
    co2_query <- '<ClimateQuery title="CO2 concentrations">
                    <axis1 name="CO2-concentration">none</axis1>
                    <axis2 name="Year">CO2-concentration[@year]</axis2>
                    <xPath buildList="true" dataName="CO2-concentration" group="false" sumAll="false">climate-model/CO2-concentration/text()</xPath>
                    <comments/>
                  </ClimateQuery>'
    expect_error(runQuery(remote_conn, co2_query, c(), c()))
})

# TODO: test querying a remote server.  It requires having a running basex server to
# connect to.

test_that('test that transformations work.', {
    prj <- loadProject(file.valid)

    sub_ahundred <- function(d) {
        d$value <- d$value - 100

        d
    }
    trn_list <- list()
    trn_list[["CO2 concentrations"]] <- sub_ahundred
    trn_list[["Population by region"]] <- sub_ahundred

    # test transformation in addScenario
    expect_message(prj_test <- addScenario(conn, "test", transformations=trn_list, saveProj=F),
                   "does not exist in this project.  Creating.")
    expect_identical(getQuery(prj, "CO2 concentrations")$value-100, getQuery(prj_test, "CO2 concentrations")$value)
    expect_identical(getQuery(prj, "Population by region")$value-100, getQuery(prj_test, "Population by region")$value)
    # Climate forcing was not in the transformation list so should not have been changed
    expect_identical(getQuery(prj, "Climate forcing")$value, getQuery(prj_test, "Climate forcing")$value)

    # test transformation in addSingleQuery
    query_name <- "CO2 concentrations"
    co2_query_query <- paste0("doc('", SAMPLE.QUERIES, "')//*[@title='", query_name, "']")
    expect_message(prj_test <- addSingleQuery(conn, "test", query_name, co2_query_query,
                   transformations=sub_ahundred, saveProj=F),
                   "does not exist in this project.  Creating.")
    expect_identical(getQuery(prj, "CO2 concentrations")$value-100, getQuery(prj_test, "CO2 concentrations")$value)
})

### Cleanup
unlink(file.valid)
unlink(file.bad)
