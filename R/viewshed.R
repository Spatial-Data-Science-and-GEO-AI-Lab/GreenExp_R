#' Viewshed
#' @description Computes a binary viewshed of a point on a Digital Surface Model (DSM). The observer height is based on the heigth of a Digital Terrain Model (DTM).
#'
#' @param observer object of class \code{sf} with one point; Starting location
#' @param max_distance numeric; Buffer distance to calculate the viewshed
#' @param dsm_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the DSM
#' @param dtm_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the DTM
#' @param observer_height numeric > 0; Height of the observer (e.g. 1.7 meters)
#' @param raster_res optional; NULL or numeric > 0; Resolution that the viewshed raster should be aggregated to. Must be a multible of the dsm_rast resolution
#' @param plot optional; Plot the intersect of the buffer around the observer location and the DSM (left DSM; right visibility raster)
#'
#' @return object of class \code{\link[terra]{rast}}
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_buffer
#' @importFrom sf st_coordinates
#' @importFrom sf st_crs
#' @importFrom sf st_geometry_type
#' @importFrom terra crs
#' @importFrom terra extract
#' @importFrom terra res
#' @importFrom terra crop
#' @importFrom terra mask
#' @importFrom terra vect
#' @importFrom terra aggregate
#' @importFrom terra rowFromY
#' @importFrom terra colFromX
#' @importFrom terra values
#' @importFrom terra ncol
#' @importFrom terra boundaries
#' @importFrom terra xyFromCell
#' @importFrom terra plot
#' @importFrom terra rast
#' @importFrom raster raster
#' @importFrom graphics par
#' @importFrom graphics points

viewshed <- function(observer, dsm_rast, dtm_rast,
                     max_distance = 800, observer_height = 1.7,
                     raster_res = NULL, plot = FALSE) {
  #### 1. Check input ####
  # observer
  if (!is(observer, "sf")) {
    stop("observer must be a sf object")
  } else if (sf::st_crs(observer)$units_gdal == "degree") {
    stop("observer CRS unit must not be degree")
  } else if (!as.character(sf::st_geometry_type(observer, by_geometry = FALSE)) == "POINT") {
    stop("observer has no valid geometry")
  } else if (nrow(observer) > 1) {
    observer <- observer[1,]
    warning("Only the fist point of observer will be used. Please look into the vgvi_from_sf function")
  }

  # dsm_rast
  if (!is(dsm_rast, "SpatRaster")) {
    stop("dsm_rast needs to be a SpatRaster object")
  } else if (sf::st_crs(terra::crs(dsm_rast))$epsg != sf::st_crs(observer)$epsg) {
    stop("dsm_rast needs to have the same CRS as observer")
  } else if(dsm_rast@ptr$res[1] != dsm_rast@ptr$res[2]) {
    stop("dsm_rast: x and y resolution must be equal.\nSee https://github.com/STBrinkmann/GVI/issues/1")
  }

  # dtm_rast
  if (!is(dtm_rast, "SpatRaster")) {
    stop("dtm_rast needs to be a SpatRaster object")
  } else if (sf::st_crs(terra::crs(dtm_rast))$epsg != sf::st_crs(observer)$epsg) {
    stop("dtm_rast needs to have the same CRS as observer")
  }

  # max_distance
  max_distance <- round(max_distance, digits = 0)

  # raster_res
  dsm_res <- min(terra::res(dsm_rast))
  if (is.null(raster_res)) {
    raster_res = dsm_res
  } else if (raster_res < min(terra::res(dsm_rast))) {
    stop("raster_res must be higher than the resolution of dsm_rast")
  } else if ((raster_res %% dsm_res) != 0) {
    stop(paste0("raster_res must be a multible of the dsm_rast resolution. Try raster_res = ", raster_res - (raster_res %% dsm_res)))
  }
  rm(dsm_res)

  # observer inside DSM
  if(is.na(terra::cellFromXY(object = dsm_rast, xy = sf::st_coordinates(observer)))) {
    stop("observer outside dsm_rast")
  }

  #### 2. Prepare Data for viewshed analysis ####
  # Coordinates of start point
  x0 <- sf::st_coordinates(observer)[1]
  y0 <- sf::st_coordinates(observer)[2]

  # AOI
  output <- terra::rast(crs = terra::crs(dsm_rast),
                        xmin = (x0 - raster_res/2 - max_distance),
                        xmax = (x0 + raster_res/2 + max_distance),
                        ymin = (y0 - raster_res/2 - max_distance),
                        ymax = (y0 + raster_res/2 + max_distance),
                        resolution = raster_res, vals = 0) %>%
    terra::crop(dsm_rast)

  # Observer height
  height0 <- as.numeric(terra::extract(dtm_rast, cbind(x0, y0))) + observer_height

  # If the resolution parameter differs from the input-DSM resolution,
  # resample the DSM to the lower resolution.
  if (raster_res == min(terra::res(dsm_rast))) {
    dsm_rast_masked <- terra::crop(dsm_rast, output)
  } else {
    terra::terraOptions(progress = 0)
    dsm_rast_masked <- terra::crop(dsm_rast, output) %>%
      terra::aggregate(fact = raster_res/terra::res(.))
    terra::terraOptions(progress = 3)
  }

  #### 3. Compute viewshed ####
  # Start row/col
  r0 <- terra::rowFromY(output, y0)
  c0 <- terra::colFromX(output, x0)

  # Convert output raster to vector
  dsm_vec <- terra::values(dsm_rast_masked, mat = FALSE)
  dsm_cpp_rast <- terra::rast(dsm_rast_masked) %>% raster::raster()

  # Apply viewshed (C++) function
  viewshed <- viewshed_cpp(dsm = dsm_cpp_rast, dsm_values = dsm_vec,
                           x0 = c0, y0 = r0, h0 = height0,
                           radius = max_distance)

  # Copy result of lineOfSight to the output raster
  output[viewshed] <- 1

  # Remove cells outside buffer
  r <- round(max_distance / terra::res(dsm_rast_masked)[1])

  cells_in_vs <- LoS_reference(x0_ref = r, y0_ref = r, r = r, nc_ref = (2*r)+1) %>%
    na.omit() %>%
    unique()
  cell_0 <- terra::cellFromXY(output, sf::st_coordinates(observer))

  output[which(!(1:ncell(output)) %in% c(cell_0, (cells_in_vs+1)))] <- NA


  #### 4. Compare DSM with Visibility ####
  if (plot) {
    dsm_rast_masked[which(!(1:ncell(output)) %in% c(cell_0, (cells_in_vs+1)))] <- NA
    graphics::par(mfrow=c(1,2))
    terra::plot(dsm_rast_masked, legend = F); graphics::points(x0, y0, col = "red", pch = 20, cex = 2)
    terra::plot(output, legend = F); graphics::points(x0, y0, col = "red", pch = 20, cex = 2)
    graphics::par(mfrow=c(1,1))
  }
  message('This code is retrieved from the GVI function, for more information look at https://github.com/STBrinkmann/GVI)')
  return(output)
}
