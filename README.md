
# GreenExp

<!-- badges: start -->
<!-- badges: end -->

The goal of GreenExp is to ...

## Installation

You can install the development version of GreenExp from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Spatial-Data-Science-and-GEO-AI-Lab/GreenEx_R")
```

## Examples

### Calc NDVI 

``` r
library(GreenExp)
## basic example code
address_test <- sf::st_read("Data/Test_multiple_home_locations.gpkg")
ndvi_test <- terra::rast("data/NDVI_data_test.tif")
a <- calc_ndvi(address_test, ndvi_test, speed=5, time = 10)
a

```

