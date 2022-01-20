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
#' @examples
#' 
#' # Load exemplary data
#' 
#' data(Euchloe_ausonia)
#' 
#' sps_occurrence <- occData(Euchloe_ausonia)
#' 
#' plotData(sps_occurrence)
#' 
#' # Create a data.frame containing species names and coordinates
#'
#' test_data <- data.frame(sps=rep("Equus acephalus",10),
#'              lon=c(-43.2,-58.4,-56,-44,-54.5,-57.4,-60.1,-68.5,-71.3,-47.5),
#'              lat=c(-22.9,-34.6,-34.8,-20,-25.5,-25.2,-3,-32.5,-41.1,-15.5),
#'              gender=rep("female",10),head_size=rep("headless individual"),
#'              occ=c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,TRUE,TRUE))
#'
#' sps_occurrence <- occData(test_data,"sps","lon","lat","occ")
#' 
#' plotData(sps_occurrence)
#' 
#' # Plot occurrences with the whole world as background
#' 
#' plotData(sps_occurrence,regional=FALSE)
#'
#' 
#' @export
plotData <- function(sps_occurrence, regional = TRUE, range = NULL) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  world <- getMap(resolution = "low")
  world <- suppressWarnings(gBuffer(world, byid = TRUE, width = 0))
  
  #create spatial points from point data coordinates
  occ_sp <- sps_occurrence
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
  par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
  plot(map, col = "khaki", bg = "azure2",
       main = unique(occ_sp$species), font.main = 3)
  if(class(range) != "NULL"){
    plot(range,add = T,col = "lightgreen")
  }
  points(occ_sp[which(occ_sp$occurrence == TRUE),], pch = 21, cex = 1, 
         bg = "blue")
  points(occ_sp[which(occ_sp$occurrence == FALSE),], pch = 21, cex = 1, 
         bg = "red")
}