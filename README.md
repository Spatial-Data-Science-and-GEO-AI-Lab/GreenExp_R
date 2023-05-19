
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

This is a simple feature collection with 3 features and 2 fields. The details of the collection are as follows:

- Active geometry column: `geometry`
- Geometry type: `POINT`
- Dimension: `XY`
- Bounding box: `xmin: 385981.9`, `ymin: 392861.6`, `xmax: 388644.2`, `ymax: 395322.2`
- Projected CRS: `OSGB36 / British National Grid`

The feature collection includes the following features:

| UID | mean_NDVI   | geometry                | buffer                                                             |
|-----|-------------|-------------------------|--------------------------------------------------------------------|
| 1   | 0.3771171   | POINT (388644.2 392861.6) | POLYGON ((388727 391348, ...))                                     |
| 2   | 0.3775469   | POINT (385981.9 393805.5) | POLYGON ((385566 391671, ...))                                     |
| 3   | 0.3773992   | POINT (388631.2 395322.2) | POLYGON ((388523 392904, ...))                                     |

Please note that the information provided is for illustration purposes and may not reflect actual data.


