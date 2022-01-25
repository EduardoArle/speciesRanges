#' updateRange
#'
#' Update the species range map based on point-occurrence presence and absence
#' data. The algorithm uses either a specialist range map or a convex hull as
#' basis for the calculations
#'
#' @importFrom sp coordinates proj4string CRS
#' @importFrom raster stack extract rasterToPolygons
#' @param occ dataFrame of the species occurrence. Output of function occData.
#' @param range shapefile, the user can provide a specialist range map, or a 
#' convex hull to be visualised with the point occurrence.
#' @param res numeric, spatial resolution in degrees. Default is 1.
#' @param range_bonus numeric, percentage of bonus to be given to cells within 
#' the basis range.
#' @param occ_bonus numeric, value of bonus given by presences.
#' @param abs_penalty numeric, value of penalty given by absences.
#' @param red2 numeric, distance decay for *occ_bunus* and *abs_penalty*.
#' @return An updated range map based on presences and absences.
#' @examples
#' 
#' @export
updateRange <- function(occ, range, res = 1, range_bonus = 75, occ_bonus = 20,
                        abs_pen = 40, red2 = 0.5){

  r_bonus <- rangeBonus(range, range_bonus, res)
  occ_bonus <- occurrenceBonus(occ, occ_bonus, res, red2)
  abs_penalty <- absencePenalty(occ, abs_pen, res, red2)
  
  # combine three parts of the equation
  rasters <- list(r_bonus,occ_bonus,abs_penalty)
  p_occ <- sum(stack(rasters))
    
  p_occ[p_occ[] >= 0.5] <- 1
  p_occ[p_occ[] < 0.5] <- 0
    
  presence <- occ[which(occ$occurrence == "TRUE"),]
  coordinates(presence) <- ~Longitude+Latitude
  proj4string(presence) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +no_defs")
  
  ID_raster <- raster(res = res) 
  ID_raster[] <- c(1:length(ID_raster))
  
  IDs_presence <- extract(ID_raster,presence) #get the IDs of points where there is a presence
  
  if(length(which(is.na(IDs_presence)))>0){   #eliminate possible NAs
    IDs_presence <- IDs_presence[-which(is.na(IDs_presence))] 
  }
  
  p_occ[IDs_presence] <- 1
  p_occ[which(p_occ[] == 0)] <- NA
  
  new_range <- rasterToPolygons(p_occ, dissolve = T)
  
  return(new_range)
}
