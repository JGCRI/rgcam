library('rgcam')

context('Loading and importing GCAM data')

file.valid <- tempfile()
file.bad <- tempfile()
notdata <- list()
save(notdata, file=file.bad)
testtime <- lubridate::ymd_hm('1863-11-19 15:45',tz='EST')
datestrfmt <- '%Y-%d-%m %H:%M:%S'

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
              expect_error(addScenario(nosuchfile, file.valid))
              expect_false(file.exists(file.valid))
          })

test_that('Data can be imported from GCAM database.', {
              prj <- addScenario(SAMPLE.GCAMDB, file.valid)
              attr(prj,'file') <- 'TEST' # because this is a tempfile, suppress
                                        # the filename.
              expect_equal_to_reference(prj,
                                        'sample-prj.dat')
              expect_true(file.exists(file.valid))
          })

## file.valid now exists
test_that('Clobber argument to addScenario works.', {
              expect_message(prj <- addScenario(SAMPLE.GCAMDB, file.valid),
                             'clobber')
              attr(prj,'file') <- 'TEST'
              expect_equal_to_reference(prj, 'sample-prj.dat')

              expect_silent(prj <- addScenario(SAMPLE.GCAMDB, file.valid,
                                               clobber=TRUE))
              attr(prj,'file') <- 'TEST'
              expect_equal_to_reference(prj, 'sample-prj.dat')
          })



test_that('File with bad permissions is detected.', {
              Sys.chmod(file.bad, '0000')
              expect_error(addScenario(SAMPLE.GCAMDB, file.bad), 'exists')
              Sys.chmod(file.bad, '0666')
          })

## TODO:  test that transformations work.

test_that('loadProject works.', {
              expect_error(loadProject(file.bad), 'Unable to load project')
              prj <- loadProject(file.valid)
              expect_equal(attr(prj,'file'), file.valid)
              attr(prj,'file') <- 'TEST'
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
              expect_equal(nrow(co2), 2)
              expect_equal(co2$scenario, c('Reference-filtered', 'Scenario2'))
              expect_equal(co2$X2100, rep(738.939,2))
              expect_equal(co2$Units, rep('PPM',2))

              ## single scenario query
              co2.single <- getQuery(prj, 'CO2 concentrations', 'Scenario2')
              expect_true(is.data.frame(co2))
              expect_equal(nrow(co2.single), 1)
              expect_equal(co2.single$scenario, 'Scenario2')
              expect_equal(co2.single$X2050, 507.433)

              ## add a query to the second scenario that doesn't exist in the
              ## first.
              prj[['Scenario2']][['foo']] <-
                  prj[['Scenario2']][['CO2 concentrations']]
              foo <- getQuery(prj, 'foo') # all scenarios
              expect_true(is.data.frame(foo))
              expect_equal(nrow(foo), 1)
              expect_equal(foo$scenario, 'Scenario2')
              expect_equal(foo$X2000, 364.147)
          })

## This one modifies the temporary project file
test_that('scenario can be added to an already-loaded data set.', {
              prj <- loadProject(file.valid)
              ## rename the scenario so we can load it again
              prj[['Scenario2']] <- dup.scenario(prj[[1]], 'Scenario2')
              prj[[1]] <- NULL
              prj <- addScenario(SAMPLE.GCAMDB, prj)

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
              expect_error(
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
              expect_true('X2050' %in% names(bctbl))

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


### Cleanup
unlink(file.valid)
unlink(file.bad)
