[![Travis-CI Build Status](https://travis-ci.org/JGCRI/rgcam.svg?branch=master)](https://travis-ci.org/JGCRI/rgcam)
[![Coverage Status](https://img.shields.io/codecov/c/github/JGCRI/rgcam/master.svg)](https://codecov.io/github/JGCRI/rgcam?branch=master)

# rgcam: An R Package for Extracting and Importing GCAM Data

## Overview

The rgcam package provides functions for extracting
[GCAM](https://github.com/JGCRI/gcam-core) data from GCAM output
databases and importing it into R for analysis.  The central concept
in rgcam is the "project data file", which contains an R-native
representation of selected queries for one or more scenarios.  The
package provides functions to run the GCAM Model Interface to extract
data and add the results to a new or existing project data file, as
well as to manage previously created project data.

## Installation

To install rgcam, open an R session and run
```R
install_github('JGCRI/rgcam')
```
The package includes a copy of the GCAM Model Interface and BaseX
library (with a version TODO), so it is not necessary to have
it otherwise installed or configured.  You will need a version of Java
that can run the Model Interface which now require a minimum of Java
version of 1.7*.  Note you do not need Java installed if you do not
need to run queries on a "local" database, see TODO for more details.

## Usage

You can create or add to a project file using the `addScenario`
function.  You will need a GCAM output database and an XML file
containing the queries that you want to extract from it.  You could
then get started by running, for example:
```R
conn <- localDBConn('/path/to/dbs', 'my-gcamdb_basexdb')
prj <- addScenario(conn, 'my-project-name.dat', 'my-scenario-name'
                   'my-batch-queries.xml')
```  
This command would run the queries in `my-batch-queries.xml` against the
database `my-gcamdb_basexdb`, extract the results for a scenario called
"my-scenario-name", and write the results to a file called "my-project-name.dat".
If you have other scenarios, whether in the same database or a
different one, you can make additional calls to `addScenario` to add
them to the project data.  The results are also returned and assigned
to `prj` so that you can begin working with them.  

If you already have a project data file that you want to work with,
you can load it using `loadProject`:
```R
prj <- loadProject('my-project-name.dat')
```

There are also functions to list scenarios and queries and to pull
queries from the data set:
```R
## List scenarios and queries
scenarios <- listScenarios(prj)
queries <- listQueries(project.data, 'scenario-name')
## Retrieve query named "GDP by region" for all scenarios in the dataset,
## formatted as a single table
gdp.rgn <- getQuery(prj, 'GDP by region')
```

The `rgcam` package now supports a number of advanced usage modes that
are described in a vignette included with the package.  Once you have
the `rgcam` installed, run
```R
devtools::build_vignettes('rgcam')
browseVignettes('rgcam')
```

## Getting Help

rgcam is under active development.  If you have trouble installing it,
or if you notice a problem while running,
[check the issues](https://github.com/JGCRI/rgcam/issues?utf8=%E2%9C%93&q=is%3Aissue)
list; we may have a solution for you.  If we don't, then open a new
issue.  Describe what you did, what you thought should have happened,
and what happened instead, and we'll help you figure out what went
wrong.
