library('rgcam')

context('Loading and importing GCAM data')

file.valid <- tempfile()
file.bad <- tempfile()
notdata <- list()
save(notdata, file=file.bad)


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
              expect_null(prj)
              prj <- addScenario(SAMPLE.GCAMDB, file.valid,
                                 clobber=TRUE)
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
              expect_equal(listScenarios(prj), 'Reference-filtered')
              expect_equal(listQueries(prj,'Reference-filtered'),
                           c('CO2 concentrations', 'Climate forcing',
                             'Global mean temperature', 'GDP by region',
                             'Population by region',
                             'Aggregated Land Allocation',
                             'Building floorspace', 'Land Allocation'))
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
              clone_query <- function(q) {dplyr::mutate(q,scenario='Scenario2')}
              prj[['Scenario2']] <- lapply(prj[[1]], clone_query)

              co2 <- getQuery(prj, 'CO2 concentrations')
              expect_true(is.data.frame(co2))
              expect_equal(nrow(co2), 2)
              expect_equal(co2$scenario, c('Reference-filtered', 'Scenario2'))
              expect_equal(co2$X2100, rep(738.939,2))

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

### Cleanup
unlink(file.valid)
unlink(file.bad)
