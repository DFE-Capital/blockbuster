
[![Build Status](https://travis-ci.org/mammykins/blockbuster.svg?branch=master)](https://travis-ci.org/mammykins/blockbuster)
[![codecov.io](http://codecov.io/github/mammykins/blockbuster/coverage.svg?branch=master)](http://codecov.io/github/mammykins/blockbuster?branch=master)
[![GitHub tag](https://img.shields.io/github/tag/mammykins/blockbuster.svg)]()

# blockbuster


This R package allows you to simulate the deterioration of School buildings through time using a Discrete Time Markov Chain. The data were collected during the Property Data Survey Programme (PDSP) of 2012-2014. Approximately 2.7 million rows of data were collected. This provides the initial state of the School Estate at time zero. The deterioration of the School Estate is then modelled by using deterioration rates associated with each Construction Elements-Sub-element-construction-type. Here we provide an anonymised ten percent sample with identifying features removed. During the development period we have some simulated data available to aid code development, real data may be uploaded later.



|Data inputs|Description|
|---|---|
|building condition|Input data for time zero. A sample simulated from the PDS condition data table.|
|transition matrices|Deterioration rates based on estimates of expected lifetime of a building element-sub-element-construction-type|

## Installing the package

The package can be installed with the `devtools` package with `devtools::install_github('mammykins/blockbuster')`.

If you cannot use this function (due to firewalls for instance) you can download the package as a `.zip` file from the main repository page, and run `devtools::install_local('path_to_zip_file')`.

## Using the package

### From the terminal


### From an R session

Launch an R session as normal and run the following (again setting the arguments as required):

```
library(blockbuster)
```

Some documentation and vignettes will be added during package development to aid the user.

```
