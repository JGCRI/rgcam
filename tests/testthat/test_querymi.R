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

test_that('empty region queries all region for XQuery style queries', {
    bld_xquery_query <- '<supplyDemandQuery title="Building Floorspace per capita">
        <axis1 name="Floorspace">input[@name]</axis1>
        <axis2 name="Year">demand-physical[@vintage]</axis2>
        <xPath buildList="true" dataName="percapita floorspace" group="false" sumAll="false">
        <![CDATA[
            declare function local:append-heirarchy($parent as node(), $append as node()) as node() {
                let $scn := $parent/ancestor::scenario,
                $rgn := $parent/ancestor::region,
                $consumer := $parent/ancestor::gcam-consumer
                return
                document { element scenario {
                    $scn/@*,
                    element region {
                        $rgn/@*,
                        element gcam-consumer {
                            $consumer/@*,
                            $append
                        }
                    }
                }
                }
            };
            declare function local:get-percapita($inputs as node()*) as node()* {
                unordered {
                    for $input in $inputs
                    let $new_input :=
                        element input {
                            attribute type {\'input\'},
                            attribute name { $input/@name },
                            for $demand in $input/floorspace
                            return
                            element demand-physical {
                                attribute vintage {$demand/@year},
                                (: TODO: hard coding units :)
                                attribute unit { \'m^2/person\'},
                                (: floorspace is billion m^2 and population is 1000 people and we want m^2/persion :)
                                text { $demand/text() div $input/ancestor::gcam-consumer/subregional-population[@year=$demand/@year] * 1000000 }
                            }
                        },
                    $new_root := local:append-heirarchy($input/parent::*/parent::*, $new_input)
                    return $new_root//text()
                }
            };
            declare function local:run-get-percapita($scenarios as xs:string*, $regions as xs:string*, $collection as xs:string) as node()* {
                let $regionsG := if(not($regions[1] = \'Global\'))
                    then $regions
                else distinct-values(collection($collection)/scenario/world/*[@type=\'region\']/@name)
                return
                for $scenario in $scenarios,
                $region in $regionsG
                let $scenario_split := tokenize($scenario, \' \'),
                $scenario_name := string-join($scenario_split[position() < last()], \' \'),
                $scenario_date := $scenario_split[last()],
                $currTree := collection($collection)/scenario[@name = $scenario_name and @date = $scenario_date]/world/*[@type = \'region\' and @name=$region]
                return
                local:get-percapita($currTree/gcam-consumer//building-node-input)

            };
            local:run-get-percapita((:scenarios:), (:regions:), (:collection:))
            ]]>
        </xPath>
        <comments/>
        </supplyDemandQuery>'

    expect_silent({bld_data <- runQuery(conn, bld_xquery_query,
                                    scenarios='Reference-filtered',
                                    regions=c())})
    # Note the sample DB has been filtered to only include just the region "USA"
    # which is not ideal for the sake of this test since we would want to ensure
    # for instance the model interface wasn't just returning the first region's
    # results instead of all.  This is the best we can do for now.
    expect_equal(unique(bld_data$region), c("USA"))
    })


### TODO: test remote db queries, test queries on DB with a wider selection of
### regions and scenarios.
