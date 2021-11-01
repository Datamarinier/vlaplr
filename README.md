
# vlaplr

Vlaplr is an interface to the API of the Flemish parliament.

## Installation

You can install the released version of vlaplr from this repo with:

``` r
require(devtools)
install_github("datamarinier/vlaplr")
```

## Usage

This is only the initial setup. The package contains one unuseful toy
function which gives you the start of the week (Monday) of a certain
date.

``` r
library(vlaplr)
get_start_week("2021-11-03")

#returns "2021-11-01 
```
