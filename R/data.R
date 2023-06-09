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

