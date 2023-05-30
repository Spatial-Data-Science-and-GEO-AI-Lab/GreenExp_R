library(httr)

access_token <- "MLY|5946359745468210|66b4730cad535105b96da4fac2cf73f3" # create your access token at https://mapillary.com/developer

address_location <- sf::st_read("data/Test_multiple_home_locations.gpkg")
a <- sf::st_transform(address_location, 4326)

# Returns data frame containing detailed info for all zooms
deg2num<-function(lat_deg, lon_deg, zoom){
  lat_rad <- lat_deg * pi /180
  n <- 2.0 ^ zoom
  xtile <- floor((lon_deg + 180.0) / 360.0 * n)
  ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
  return( data.frame(x=xtile, y=ytile,z=zoom))
  #  return(paste(paste("https://a.tile.openstreetmap.org", zoom, xtile, ytile, sep="/"),".png",sep=""))
}

coords <-sf::st_coordinates(a)
xtiles <- coords[,1]
ytiles <- coords[,2]

tiles <- deg2num(xtiles, ytiles, zoom=14)
tile_url = paste0("https://tiles.mapillary.com/maps/vtp/mly1_public/2/",tiles$z,'/',tiles$x,'/',tiles$y,'?access_token=',access_token)
response <- httr::GET(tile_url[2])
