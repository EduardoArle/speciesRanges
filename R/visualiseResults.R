#' plotData
#'
#' Plot the species occurrences with map background for visualisation
#'
#' @importFrom graphics points
#' @importFrom raster extent plot
#' @importFrom rgeos gIntersection gBuffer
#' @importFrom rworldmap getMap
#' @importFrom sp over proj4string coordinates CRS
#' @param occ dataFrame of the species occurrence. Output of function occData.
#' @param regional logical, whether the whole world should be plotted as the 
#' background or only the region adjacent to the species countries of 
#' occurrence.
#' @param range shapefile, the user can provide a specialist range map, or a 
#' convex hull to be visualised with the point occurrence.
#' @return This function plots the species occurrence
#' @export
visualiseResults <- function(occ, regional = TRUE, range, updated_range) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  world <- getMap(resolution = "low")
  world <- suppressWarnings(gBuffer(world, byid = TRUE, width = 0))
  
  #create spatial points from point data coordinates
  occ_sp <- occ
  sp::coordinates(occ_sp) <- ~Longitude + Latitude
  sp::proj4string(occ_sp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

  if(regional == TRUE){
    countries <- unique(over(occ_sp,world)$NAME)
    countries <- countries[-is.na(countries)]
    countries <- world[world$NAME %in% countries,]
    CP <- as(extent(countries), "SpatialPolygons")
    sp::proj4string(CP) <- CRS(proj4string(world))
    map <- suppressWarnings(gIntersection(world,
                                          CP,
                                          byid = TRUE, 
                                          checkValidity = 2))
  } else {
    map <- world
  }
  par(mfrow = c(1, 3), mar = c(1, 1, 1, 1))
  
  plot(map, col = "khaki", bg = "azure2",
       main = unique(occ_sp$species), font.main = 3)
  
  points(occ_sp[which(occ_sp$occurrence == TRUE),], pch = 21, cex = 1, 
         bg = "blue")
  points(occ_sp[which(occ_sp$occurrence == FALSE),], pch = 21, cex = 1, 
         bg = "red")

  plot(map, col = "khaki", bg = "azure2",
       main = unique(occ_sp$species), font.main = 3)
  
  plot(range,add = T,col = "lightgreen")

  plot(map, col = "khaki", bg = "azure2",
       main = unique(occ_sp$species), font.main = 3)
  
  plot(updated_range,add = T,col = "lightblue")

}