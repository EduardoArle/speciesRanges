#' Input occurrence data
#'
#' Prepares user provided georeferenced biological data for the models
#'
#' @param  occ_table table containing latitude and longitude
#' @param  species col.name containing the species information
#' @param  longitude col.name containing the longitude information
#' @param  latitude col.name containing the latitude information
#' @param  occurrence col.name informing whether each entry is a presence or 
#' absence. Values must be TRUE or FALSE.
#' @return This function standardises the user provided georeferenced 
#' biological data to be fed into the models.
#' @examples
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
#' @export
occData <- function(occ_table, 
                    species = "Species", 
                    longitude = "Longitude", 
                    latitude = "Latitude",
                    occurrence = "occurrence") {
  table2 <- occ_table
  names(table2)[which(names(table2) == species)] <- "Species"
  names(table2)[which(names(table2) == longitude)] <- "Longitude"
  names(table2)[which(names(table2) == latitude)] <- "Latitude"
  names(table2)[which(names(table2) == occurrence)] <- "occurrence"
  return(table2)
}
