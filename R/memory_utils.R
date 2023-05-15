getFreeMemoryKB <- function() {
  osName <- Sys.info()[["sysname"]]
  if (osName == "Windows") {
    x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    x <- gsub("\r", "", x, fixed = TRUE)
    return(as.integer(x))
  } else if (osName == 'Linux') {
    x <- system2('free', args='-k', stdout=TRUE)
    x <- strsplit(x[2], " +")[[1]][4]
    return(as.integer(x))
  } else {
    stop("Only supported on Windows and Linux")
  }
}

rast_fits_vect_fact <- function(max_aoi, max_distance, raster_res) {
  invisible(gc())
  available_ram = getFreeMemoryKB()
  
  fact = 0
  b = TRUE
  
  while (b) {
    fact = fact + 1
    
    nc = sf::st_make_grid(max_aoi, n = fact)[1] %>%
      sf::st_buffer(max_distance) %>%
      terra::vect() %>%
      terra::rast(resolution = raster_res) %>%
      terra::ncell()
    
    if( ((2^31-1) > nc) && (available_ram > (nc * 8 / 1000)) ){
      b = FALSE
    }
  }
  
  return(fact)
}