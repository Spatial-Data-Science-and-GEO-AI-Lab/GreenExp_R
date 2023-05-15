#' @title Visibility Proportion
#' @description Function that calculates the proportion of visible area within a viewshed
#'
#' @param observer object of class \code{sf} with one point; Starting location
#' @param max_distance numeric; Buffer distance to calculate the viewshed
#' @param dsm_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the DSM
#' @param dtm_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the DTM
#' @param observer_height numeric > 0; Height of the observer (e.g. 1.7 meters)
#' @param raster_res optional; NULL or numeric > 0; Resolution that the viewshed raster should be aggregated to. Must be a multible of the dsm_rast resolution
#' @param plot optional; Plot the intersect of the buffer around the observer location and the DSM (left DSM; right visibility raster)
#'
#' @return numeric; Proportion of visible area of the viewshed. Values range from 0 to 1
#' @export
#'
visibility_proportion <- function(observer, max_distance = 800,
                                  dsm_rast, dtm_rast, observer_height = 1.7,
                                  raster_res = NULL, plot = FALSE){
  # Viewshed
  vs <- GreenExp::viewshed(observer = observer, raster_res = raster_res,
                      dsm_rast = dsm_rast, dtm_rast = dtm_rast,
                      max_distance = max_distance, observer_height = observer_height, plot = plot)

  vs_vals <- na.omit(terra::values(vs, mat = FALSE))

  # Calculate proportion of visible cells to all cells within max_distance buffer
  return(sum(vs_vals == 1) / length(vs_vals))
}
