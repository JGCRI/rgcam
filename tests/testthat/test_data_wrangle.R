library('rgcam')
library('dplyr')

context('GCAM data wrangling')

## set up test data
rgnlist <- rep(c('a','b','c'),2)
sectorlist <- c(rep('s1',3), rep('s2',3))
x2050 <- rep((1:3),2)
x2075 <- rep(1,6)
scenx <- tibble(scenario='x', region=rgnlist, sector=sectorlist,
                X2050=x2050, X2075=x2075)

y2050 <- c(rep(1,3), rep(0,3))
y2075 <- c(rep(0,3), 1:3)
sceny <- tibble(scenario='y', region=rgnlist, sector=sectorlist,
                X2050=y2050, X2075=y2075)
## remove a row to test what happens when a row is not in both sets
sceny <- filter(sceny, region != 'c' | sector != 's2')

scenboth <- rbind(scenx, sceny)

test_that('scenarioDiff: works with single table and default arguments.', {
              rslt <- diffScenarios(scenboth, c('y','x'))
              expect_equal(names(rslt), c('region','X2050','X2075'))
              expect_equal(rslt$X2050, c(-1,-3,-5))
              expect_equal(rslt$X2075, c(-1,0,-2))
              ## reverse the order of the scenarios
              rslt <- diffScenarios(scenboth, c('x','y'))
              expect_equal(rslt$X2050, c(1,3,5))
              expect_equal(rslt$X2075, c(1,0,2))
          })

test_that('scenarioDiff: aggregation by another column works.', {
              rslt <- diffScenarios(scenboth, c('y','x'), keycols='sector')
              expect_equal(names(rslt), c('sector','X2050','X2075'))
              expect_equal(rslt$X2050, c(-3,-6))
              expect_equal(rslt$X2075, c(-3,0))
          })

test_that('scenarioDiff: no-aggregation mode works.', {
              rslt <- diffScenarios(scenboth, c('y','x'), keycols=NULL)
              expect_equal(names(rslt), c('region','sector','X2050','X2075'))
              expect_equal(rslt$X2050, c(0,-1,-1,-2,-2,NA))
              expect_equal(rslt$X2075, c(-1,0,-1,1,-1,NA))
          })

test_that('scenarioDiff: two-table mode works.', {
              rslt <- diffScenarios(scenx, data2=sceny)
              expect_equal(names(rslt), c('region','X2050','X2075'))
              expect_equal(rslt$X2050, c(1,3,5))
              expect_equal(rslt$X2075, c(1,0,2))
              ## reverse the order and test again
              rslt <- diffScenarios(sceny, data2=scenx)
              expect_equal(rslt$X2050, c(-1,-3,-5))
              expect_equal(rslt$X2075, c(-1,0,-2))
          })

test_that('scenarioDiff: throws errors for bad arguments.', {
              expect_error(diffScenarios(scenboth),
                           'argument is required if only one')
              expect_error(diffScenarios(scenboth, c('x','y','z')),
                           'You must specify exactly two scenarios')
              expect_error(diffScenarios(scenboth, data2=sceny),
                           'each must contain exactly one')
          })
