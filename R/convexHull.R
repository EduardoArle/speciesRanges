#' Convex hull
#'
#' Creates a convex hull with a buffer using all the occurrence points available
#' for a species. This convex hull is used as the basis to calculate the species
#' range when a specialist range map is not available.
#'
#' @importFrom grDevices chull
#' @importFrom rgeos gBuffer gIntersection gDifference
#' @importFrom rworldmap getMap
#' @importFrom sp SpatialPolygons Polygons Polygon SpatialPolygonsDataFrame
#' @importFrom sp proj4string
#' @param occ_data output of function *occData* indicating the species 
#' occurrence
#' @param buffer numeric, indicates the size of the buffer in km. Default is 
#' 0.5 degree.
#' @param realm character, "terrestrial" or "marine". NULL will considere both.
#' @return This function standardises the user provided georeferenced 
#' biological data to be fed into the models.
#' @examples
#' 
#' # Create a data.frame containing species names and coordinates
#'
#' test_data <- data.frame(sps=rep("Equus acephalus",10),
#'              lon=c(-43.2,-58.4,-56,-44,-54.5,-57.4,-60.1,-68.5,-71.3,-47.5),
#'              lat=c(-22.9,-34.6,-34.8,-20,-25.5,-25.2,-3,-32.5,-41.1,-15.5),
#'              gender=rep("female",10),head_size=rep("headless individual"),
#'              occ=c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,TRUE,TRUE))
#'
#' occ_data <- occData(test_data,"sps","lon","lat","occ")
#' 
#' convex_hull <- convexHull(occ_data,realm = "terrestrial")
#' 
#' @export
convexHull <- function(occ_data, buffer = 0.5, realm = NULL) {
  presence <- occ_data[which(occ_data$occurrence == T),]
  dat <- cbind(presence$Longitude,presence$Latitude)
  ch <- chull(dat)
  coords <- dat[c(ch, ch[1]), ]  # closed polygon
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)),)
  sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
  sp_poly_buffer <- suppressWarnings(gBuffer(sp_poly_df, byid = TRUE, 
                                             width = buffer))
  sp_poly_buffer2 <- sp_poly_buffer
  
  world <- getMap(resolution = "low")
  world <- suppressWarnings(gBuffer(world, width = 0))
  if(realm == "terrestrial"){
    sp_poly_buffer2 <- suppressWarnings(gIntersection(world,
                                          sp_poly_buffer2,
                                          checkValidity = 2))
  }
  if(realm == "marine"){
    sp_poly_buffer2 <- suppressWarnings(gDifference(sp_poly_buffer2,
                                          world,
                                          checkValidity = 2))
  }
  return(sp_poly_buffer2)
}