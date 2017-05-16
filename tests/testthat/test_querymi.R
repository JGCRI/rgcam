context('Running model interface queries')

SAMPLE.GCAMDBLOC <- system.file("extdata",
                                package="rgcam")
SAMPLE.QUERIES <- system.file("ModelInterface", "sample-queries.xml",
                              package="rgcam")
conn <- localDBConn(SAMPLE.GCAMDBLOC, "sample_basexdb")
queries <- parse_batch_query(SAMPLE.QUERIES)


test_that('queries can be parsed', {
              expect_silent({qt <- parse_batch_query(SAMPLE.QUERIES)})
              expect_equal(length(qt), 9)
              expect_equal(names(qt),
                           c("CO2 concentrations", "Climate forcing",
                             "Global mean temperature",  "GDP by region",
                             "PPP GDP by region", "Population by region",
                             "Aggregated Land Allocation",
                             "Building floorspace", "Land Allocation")
                           )
              expect_equal(qt[[3]]$query,
                           "<ClimateQuery title=\"Global mean temperature\">\n  <axis1 name=\"temperature\">none</axis1>\n  <axis2 name=\"Year\">global-mean-temperature[@year]</axis2>\n  <xPath buildList=\"true\" dataName=\"global-mean-temperature\" group=\"false\" sumAll=\"false\">climate-model/global-mean-temperature/text()</xPath>\n  <comments/>\n</ClimateQuery>")
          })


test_that('runQuery works with default arguments and local db', {
              expect_silent({rslt <- runQuery(conn, queries[[4]]$query)})
              expect_true(is.data.frame(rslt))
              expect_equal(nrow(rslt), 22)
              expect_equal(names(rslt), c("Units", "scenario", "region", "Year",
                                          "value"))
          })

test_that('runQuery works with explicit arguments and local db', {
              queries <- parse_batch_query(SAMPLE.QUERIES)
              expect_silent({rslt <- runQuery(conn, queries[[4]]$query,
                                              scenarios='Reference-filtered',
                                              regions='USA')})
              expect_true(is.data.frame(rslt))
              expect_equal(nrow(rslt), 22)
              expect_equal(names(rslt), c("Units", "scenario", "region", "Year",
                                          "value"))
          })


### TODO: test remote db queries, test queries on DB with a wider selection of
### regions and scenarios.
