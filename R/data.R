#' an sf data frame of neighborhoods in Amsterdam
#'
#' Contains 518 polygons of neighborhoods in Amsterdam
#'
#' @format A data frame with 518 rows and 3 variables:
#'  \describe{
#'        \item{Buurtcode}{unique ID for each neighborhood}
#'        \item{Buurt}{The name of each neighborhood}
#'        \item{geom}{The geometry of the polygons of the neighborhoods}
#'        }
#'
#' @source {https://maps.amsterdam.nl/open_geodata/}
#'
#' @examples
#' data(Ams_Neighborhoods)     # Lazy loading
#'
"Ams_Neighborhoods"

#' an sf data frame of houses in Amsterdam
#' Houses are the centroids of the neighborhoods in Amsterdam
#'
#' Contains 518 points of houses in Amsterdam
#'
#' @format A data frame with 518 rows and 1 variables:
#'  \describe{
#'        \item{geom}{The geometry of the points of the houses}
#'        }
#'
#' @source {https://maps.amsterdam.nl/open_geodata/}
#'
#' @examples
#' data(Ams_Houses)     # Lazy loading
#'
"Ams_Houses"


#' an sf data frame of parks in Amsterdam
#' Parks are multi polygons
#'
#' Contains 123 polygons of parks in Amsterdam
#'
#' @format A data frame with 123 rows and 2 variables:
#'  \describe{
#'        \item{name}{The name of the park}
#'        \item{geom}{The geometry of the points of the houses}
#'        }
#'
#' @source {https://maps.amsterdam.nl/open_geodata/}
#'
#' @examples
#' data(Ams_Parks)     # Lazy loading
#'
"Ams_Parks"



#' an sf data frame of a small network in Amsterdam
#' network is represented with linestrings
#'
#' Contains 3899 linestring of roads in Amsterdam
#'
#' @format A data frame with 3899 rows and 3 variables:
#'  \describe{
#'        \item{osm_id}{The ID of the street}
#'        \item{name}{The name of the street}
#'        \item{geom}{The geometry of the LINESTRING}
#'        }
#'
#' @source {retrieved from osmextract, https://cran.r-project.org/web/packages/osmextract/vignettes/osmextract.html}
#'
#' @examples
#' data(network_file)     # Lazy loading
#'
"network_file"



