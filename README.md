
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

## Examples availability functions

### Calc NDVI 

``` r
library(GreenExp)
## basic example code
# Read the home address
address_test <- sf::st_read("Data/Test_multiple_home_locations.gpkg")
# Read the NDVI dataset
ndvi_test <- terra::rast("data/NDVI_data_test.tif")
# the funciton 'calc_ndvi' returns a sf dataset with the mean ndvi within a buffer from the home address 
ndvi_scores <- GreenExp::calc_ndvi(address_test, ndvi_test, speed=5, time = 10)
ndvi_scores

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



### Green Cover

``` r
landcover_test <- terra::rast("Data/Landcover_data_test.tif")
greencover_values <- GreenExp::land_cover(address_test, landcover_test, speed=5, time = 10)
greencover_values

```

This is a simple feature collection with 3 features and 37 fields. The details of the collection are as follows:

- Active geometry column: `geometry`
- Geometry type: `POINT`
- Dimension: `XY`
- Bounding box: `xmin: 385981.9`, `ymin: 392861.6`, `xmax: 388644.2`, `ymax: 395322.2`
- Projected CRS: `OSGB36 / British National Grid`

The feature collection includes the following fields:

| UID | 1001 | 1002 | 1003 | 1004 | 1005 | 1101 | 1102 | 1103 | 1104 | 1105 | 1201 | 1202 | 1203 | 1204 | 1205 | 1301 | 1302 | 1303 | 1304 | 1305 | 1401 | 1402 | 1403 | 1404 | 1405 | 1501 | 1502 | 1503 | 1504 | 1505 | 2001 | 2002 | 2003 | 2004 | 2005 | <NA> | geometry                | buffer                 |
|-----|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|-------------------------|------------------------|
| 1   | 0.26 | 0    | 0.03 | 0.02 | 0.05 | 0.01 | 0    | 0.04 | 0.07 | 0.06 | 0.02 | 0    | 0.01 | 0.02 | 0.03 | 0.15 | 0    | 0.06 | 0.05 | 0.09 | 0.01 | 0    | 0.01 | 0.01 | 0.01 | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | POINT (388644.2 392861.6) | POLYGON ((388727 391348, ...
| 2   | 0.24 | 0    | 0.03 | 0.01 | 0.05 | 0.02 | 0    | 0.04 | 0.05 | 0.06 | 0.01 | 0    | 0    | 0.01 | 0.03 | 0.16 | 0    | 0.06 | 0.05 | 0.10 | 0.02 | 0    | 0.01 | 0.01 | 0.02 | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0.01 | 0    | POINT (385981.9 393805.5) | POLYGON ((385566 391671, ...
| 3   | 0.27 | 0    | 0.02 | 0.02 | 0.04 | 0.01 | 0    | 0.05 | 0.07 | 0.07 | 0.01 | 0    | 0.01 | 0.03 | 0.03 | 0.16 | 0    | 0.04 | 0.05 | 0.06 | 0.01 | 0    | 0    | 0.01 | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | POINT (388631.2 395322.2) | POLYGON ((388523 392904, ...



