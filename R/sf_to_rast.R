#' sf to raster interpolation
#' @description Function for computing a continuous raster map from a sf object by interpolating the sf values. For the interpolation Inverse Distance Weighting (IDW) is beeing used.
#'
#' @param observer object of class \code{sf}; Observer location(s) used for the interpolation. See ‘Details’ for valid sf geometry types
#' @param v character; Name of a variable in \code{observer} which values are used for the interpolation
#' @param aoi optional; object of class \code{sf}; Area of interest the raster map should me masked to. If NULL, the bounding box of \code{observer} will be used instead
#' @param max_distance numeric; Only observer locations within the buffer distance from the prediction location are used for interpolation 
#' @param n numeric; Number of nearest observer locations that should be used for the interpolation
#' @param beta numeric; Inverse istance power, β, determines the degree to which the nearer observer locations are preferred over more distant points
#' @param raster_res numeric; Resolution of the interpolatet raster map that is returned
#' @param spacing optional; numeric; Only if \code{observer} is a linestring (or polygon), points on the line (or on a grid) will be generated.
#' The \code{spacing} parameter sets the distance in between the points on the line/grid
#' @param cores numeric; The number of cores to use, i.e. at most how many child processes will be run simultaneously
#' @param progress logical; Show progress bar?
#'
#' @details 
#' \code{observer} needs to be a geometry of type POINT, LINESTRING, MULTILINESTRING, POLYGON or MULTIPOLYGON. If \code{observer} is a LINESTRING or MULTILINESTRING, 
#' points will be generated along the line(s) every \code{raster_res} meters. If \code{observer} is a POLYGON or MULTIPOLYGON, a grid with resolution = \code{raster_res} 
#' will be generated as the obersver locations.
#' The CRS (\code{\link[sf]{st_crs}}) needs to have a metric unit! 
#'
#' @return object of class \code{\link[terra]{rast}} 
#' @export
#' @importFrom sf st_crs
#' @importFrom sf st_geometry_type
#' @importFrom sf st_transform
#' @importFrom sf st_union
#' @importFrom sf st_cast
#' @importFrom sf st_line_sample
#' @importFrom sf st_as_sf
#' @importFrom sf st_bbox
#' @importFrom sf st_intersection
#' @importFrom sf st_make_grid
#' 
#' @importFrom dplyr rename
#' 
#' @importFrom raster raster
#' @importFrom terra rast
#' @importFrom terra vect
#' @importFrom terra rasterize
#' @importFrom terra values
#' 



sf_to_rast <- function(observer, v, aoi = NULL, max_distance = Inf, n = Inf, beta = 2,
                       raster_res = NULL, spacing = raster_res, na_only = FALSE, cores = 1, progress = FALSE) {
  #### 1. Check input ####
  # observer
  valid_sf_types <- c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")
  if (!is(observer, "sf")) {
    stop("observer must be a sf object")
  } else if (is.null(sf::st_crs(observer)$units) || sf::st_crs(observer)$units_gdal == "degree") {
    stop("observer CRS unit must not be degree")
  } else if (!as.character(sf::st_geometry_type(observer, by_geometry = FALSE)) %in% valid_sf_types) {
    stop("observer has no valid geometry. See Details")
  } else if (as.character(sf::st_geometry_type(observer, by_geometry = FALSE)) == "MULTIPOINT") {
    observer <- sf::st_cast(observer, "POINT")
  }
  rm(valid_sf_types)
  
  # v
  if(!v %in% colnames(observer)) {
    stop("v must be a variable of the observer object")
  }
  
  # aoi
  if(!is.null(aoi)){
    if (!is(aoi, "sf")) {
      stop("aoi must be a sf object")
    } else if (!as.character(sf::st_geometry_type(aoi, by_geometry = FALSE)) %in% c("POLYGON", "MULTIPOLYGON")) {
      stop("observer must be POLYGON or MULTIPOLYGON")
    } else if (sf::st_crs(aoi) != sf::st_crs(observer) ) {
      aoi <- sf::st_transform(aoi, sf::st_crs(observer))
    }
  }
  
  
  # max_distance
  mode = 1
  if(max_distance <= 0){
    stop("max_distance must be greater 0")
  } else if (is.infinite(max_distance)) {
    mode = 0
  }
  
  # n
  if(n <= 0){
    stop("n must be greater 0")
  }
  
  # raster_res
  if(is.null(raster_res) || raster_res <= 0){
    stop("raster_res must be greater 0")
  }
  
  # spacing
  if (is.null(spacing)) {
    spacing <- raster_res
  }
  
  # cores
  cores <- as.integer(cores)
  
  
  #### 2. Convert observer to points ####
  if(progress) {
    message("Preprocessing:")
    pb = txtProgressBar(min = 0, max = 3, initial = 0, style = 3)
  }
  
  if (as.character(sf::st_geometry_type(observer, by_geometry = FALSE)) %in% c("LINESTRING", "MULTILINESTRING")) {
    observer <- observer %>%
      sf::st_union() %>%
      sf::st_cast("LINESTRING") %>%
      sf::st_line_sample(density = 1/spacing) %>%
      sf::st_cast("POINT") %>%
      sf::st_as_sf() %>% 
      dplyr::rename(geom = x) %>% 
      sf::st_intersection(observer)
  } else if (as.character(sf::st_geometry_type(observer, by_geometry = FALSE)) %in% c("POLYGON", "MULTIPOLYGON")) {
    observer_bbox <- sf::st_bbox(observer)
    observer <- sf::st_make_grid(x = observer_bbox, cellsize = spacing,
                                 crs = sf::st_crs(observer), what = "centers") %>% 
      sf::st_as_sf() %>% 
      dplyr::rename(geom = x) %>% 
      sf::st_intersection(observer)
    rm(observer_bbox)
  }
  
  if (progress) setTxtProgressBar(pb, 1)
  
  #### 3. Prepare data for interpolation analysis ####
  # observer
  if(is.null(aoi)){
    aoi <- st_as_sfc(sf::st_bbox(observer))
  }
  
  # n
  if(is.infinite(n)){
    n = nrow(observer)
  }
  
  # mode
  if(mode == 1) {
    box_size <- (as.integer(max_distance/raster_res)) * (as.integer(max_distance/raster_res))
    mode <- ifelse(box_size > n, 1, 0)
    rm(box_size)
  }
  
  # IWD raster
  iwd_rast <- terra::rast(xmin = sf::st_bbox(aoi)[1],
                          xmax = sf::st_bbox(aoi)[3],
                          ymin = sf::st_bbox(aoi)[2],
                          ymax = sf::st_bbox(aoi)[4],
                          crs = sf::st_crs(aoi)$proj4string,
                          resolution = raster_res)
  obs_cells <- terra::cellFromXY(iwd_rast, sf::st_coordinates(observer))
  
  # Remove observer outside aoi
  observer <- observer[which(!is.na(obs_cells)), ]
  obs_cells <- na.omit(obs_cells)
  
  iwd_rast[obs_cells] <- dplyr::pull(observer, v)
  
  if (progress) setTxtProgressBar(pb, 2)
  
  iwd_cpp_rast <- iwd_rast %>% terra::rast() %>% raster::raster()
  iwd_raster_vec <- terra::values(iwd_rast, mat = FALSE)
  
  if (progress) setTxtProgressBar(pb, 3)
  
  #### 4. IWD ####
  if (progress) {
    message("\nComputing IWD interpolation:")
  }
  
  iwd_vals <- IDW_cpp(rast = iwd_cpp_rast, x = iwd_raster_vec,
                      sf_x = sf::st_coordinates(observer)[,1],
                      sf_y = sf::st_coordinates(observer)[,2],
                      sf_z = dplyr::pull(observer, v),
                      n = n, b = beta, radius = max_distance, mode = 0, #mode,
                      na_only = na_only, ncores = cores, display_progress = progress)
  
  iwd_rast[] <- iwd_vals
  
  iwd_rast <- iwd_rast %>% 
    terra::crop(terra::vect(aoi)) %>%
    terra::mask(terra::vect(aoi))
  
  return(iwd_rast)
}