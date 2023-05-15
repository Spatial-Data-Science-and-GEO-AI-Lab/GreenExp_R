#' @title Viewshed Resolution Analysis
#' @description Aggregating the input DSM reduces computiation time on cost of accuracy.
#' The \code{resolution_analysis} function computes viewsheds at different resolutions and compares computaion time and accuracy to the original viewshed 
#'
#' @param observer object of class \code{sf} with POINT geometries; Observer location(s) from where the resolution analysis should be computed.
#' @param max_distance numeric; Buffer distance to calculate the viewsheds
#' @param dsm_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the DSM
#' @param dtm_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the DTM
#' @param observer_height numeric > 0; Height of the observer (e.g. 1.7 meters)
#' @param raster_res integer vector; Resolution values that the viewshed raster should be aggregated to. All values must be a multible of the dsm_rast resolution
#'
#' @return object of class \code{\link[tibble]{tibble}}. Resolution, Similarity, Time [ms]
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

resolution_analysis <- function(observer, dsm_rast, dtm_rast, greenspace_rast = NULL, 
                     max_distance = 800, observer_height = 1.7, 
                     raster_res = NULL, progress = FALSE) {
  #### 1. Check input ####
  # observer
  if (!is(observer, "sf")) {
    stop("observer must be a sf object")
  } else if (sf::st_crs(observer)$units_gdal == "degree") {
    stop("observer CRS unit must not be degree")
  } else if (!as.character(sf::st_geometry_type(observer, by_geometry = FALSE)) == "POINT") {
    stop("observer has no valid geometry")
  }
  
  # dsm_rast
  if (!is(dsm_rast, "SpatRaster")) {
    stop("dsm_rast needs to be a SpatRaster object")
  } else if (sf::st_crs(terra::crs(dsm_rast))$epsg != sf::st_crs(observer)$epsg) {
    stop("dsm_rast needs to have the same CRS as observer")
  } else if(dsm_rast@pnt$res[1] != dsm_rast@pnt$res[2]) {
    stop("dsm_rast: x and y resolution must be equal.\nSee https://github.com/STBrinkmann/GVI/issues/1")
  }
  
  # dtm_rast
  if (!is(dtm_rast, "SpatRaster")) {
    stop("dtm_rast needs to be a SpatRaster object")
  } else if (sf::st_crs(terra::crs(dtm_rast))$epsg != sf::st_crs(observer)$epsg) {
    stop("dtm_rast needs to have the same CRS as observer")
  }
  
  
  
  # greenspace_rast
  if(!is.null(greenspace_rast)){
    if (!is(greenspace_rast, "SpatRaster")) {
      stop("greenspace_rast needs to be a SpatRaster object!")
    } else if (sf::st_crs(terra::crs(greenspace_rast))$epsg != sf::st_crs(observer)$epsg) {
      stop("greenspace_rast needs to have the same CRS as observer")
    } else if(greenspace_rast@pnt$res[1] != greenspace_rast@pnt$res[2]) {
      stop("greenspace_rast: x and y resolution must be equal.\nSee https://github.com/STBrinkmann/GVI/issues/1")
    }
  }
  
  # max_distance
  max_distance <- round(max_distance, digits = 0)
  
  # raster_res
  raster_res <- sort(raster_res)
  dsm_res <- min(terra::res(dsm_rast))
  if (is.null(raster_res)) {
    raster_res = dsm_res
  } else if (any(raster_res < min(terra::res(dsm_rast)))) {
    stop("raster_res must be higher than the resolution of dsm_rast")
  } else if (any((raster_res %% dsm_res) != 0)) {
    stop(paste0("raster_res must be a multible of the dsm_rast resolution"))
  }
  rm(dsm_res)
  
  
  # Main Loop
  output <- dplyr::tibble(
    Resolution = as.numeric(),
    Similarity = as.numeric(),
    Time = as.numeric()
  )
  if(progress){
    pb = txtProgressBar(min = 0, max = nrow(observer), initial = 0) 
  }
  for (i in 1:nrow(observer)) {
    distance_tbl <- dplyr::tibble(
      Resolution = as.numeric(),
      Similarity = as.numeric(),
      Time = as.numeric()
    )
    
    if(progress){
      setTxtProgressBar(pb,i) 
    }
    
    for(r in raster_res) {
      #### 2. Prepare Data for viewshed analysis ####
        
      # Coordinates of start point
      x0 <- sf::st_coordinates(observer[i,])[1]
      y0 <- sf::st_coordinates(observer[i,])[2]
      
      # AOI
      aoi <- terra::rast(crs = terra::crs(dsm_rast),
                         xmin = floor(x0 - r/2 - max_distance),
                         xmax = ceiling(x0 + r/2 + max_distance),
                         ymin = floor(y0 - r/2 - max_distance),
                         ymax = ceiling(y0 + r/2 + max_distance),
                         resolution = r, vals = 0) %>%
        terra::crop(dsm_rast)
      
      # Observer height
      height0 <- as.numeric(terra::extract(dtm_rast, cbind(x0, y0))) + observer_height
      
      # If the resolution parameter differs from the input-DSM resolution,
      # resample the DSM to the lower resolution.
      if (r == min(terra::res(dsm_rast))) {
        dsm_rast_masked <- terra::crop(dsm_rast, aoi)
      } else {
        terra::terraOptions(progress = 0)
        dsm_rast_masked <- terra::crop(dsm_rast, aoi) %>% 
          terra::aggregate(fact = r/terra::res(.))
        terra::terraOptions(progress = 3)
      }
      
      
      #### 3. Compute viewshed ####
      # Start row/col
      r0 <- terra::rowFromY(aoi, y0)
      c0 <- terra::colFromX(aoi, x0)
      
      # Convert raster to vector
      dsm_vec <- terra::values(dsm_rast_masked, mat = FALSE)
      dsm_cpp_rast <- terra::rast(dsm_rast_masked) %>% raster::raster()
      
      if(!is.null(greenspace_rast)){
        this_greenspace_rast <- greenspace_rast %>% 
          terra::crop(aoi)
        
        greenspace_vec <- terra::values(this_greenspace_rast, mat = FALSE)
        greenspace_cpp_rast <- this_greenspace_rast %>% terra::rast() %>% raster::raster()
      }
    
      
      # Apply viewshed (C++) function
      if(is.null(greenspace_rast)){
        time_a <- Sys.time()
        viewshed <- viewshed_cpp(dsm_cpp_rast, dsm_vec, c0, r0, max_distance, height0)
        time_b <- Sys.time()
      } else {
        viewshed <- viewshed_cpp(dsm_cpp_rast, dsm_vec, c0, r0, max_distance, height0)
        
        time_a <- Sys.time()
        vgvi <- VGVI_cpp(dsm = dsm_cpp_rast, dsm_values = dsm_vec,
                         greenspace = greenspace_cpp_rast, greenspace_values = greenspace_vec, 
                         x0 = c0, y0 = r0, radius = max_distance, h0 = height0,
                         fun = 1, m = 0.5, b = 8)
        time_b <- Sys.time()
      }
      
      
      
      # Copy result of lineOfSight to the aoi raster
      aoi[viewshed] <- 1
      aoi_vals <- terra::values(aoi) %>%
        na.omit()
      
      distance_tbl <- dplyr::tibble(
        Resolution = r,
        Similarity = length(which(aoi_vals == 1)) / length(aoi_vals),
        Time = as.numeric(difftime(time_b, time_a, units = "secs"))*1000
      ) %>% 
        dplyr::add_row(distance_tbl, .)
    }
    output <- distance_tbl %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(Similarity = min(distance_tbl[1,2], Similarity) / max(distance_tbl[1,2], Similarity)) %>% 
      dplyr::add_row(output, .)
  }
  
  return(output)
}