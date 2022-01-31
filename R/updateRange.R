#' updateRange
#'
#' Update the species range map based on point-occurrence presence and absence
#' data. The algorithm uses either a specialist range map or a convex hull as
#' basis for the calculations
#'
#' @importFrom sp coordinates proj4string CRS
#' @importFrom raster rasterize extract raster stack rasterToPolygons
#' @importFrom rgeos gBuffer
#' @param occ dataFrame of the species occurrence. Output of function occData.
#' @param range shapefile, the user can provide a specialist range map, or a 
#' convex hull to be visualised with the point occurrence.
#' @param res numeric, spatial resolution in degrees. Default is 1.
#' @param range_bonus numeric, percentage of bonus to be given to cells within 
#' the basis range.
#' @param occ_bonus numeric, value of bonus given by presences.
#' @param abs_penalty numeric, value of penalty given by absences.
#' @param red2 numeric, distance decay for *occ_bunus* and *abs_penalty*.
#' @param study_area, spatial object defining the extention of the study area.
#' The default is the *range* buffered by 1 degree.
#' @return An updated range map based on presences and absences.
#' @examples
#' 
#' @export
updateRange <- function(occ, range, res = 1, range_bonus = 75, occ_bonus = 20,
                        abs_pen = 40, red2 = 0.5, study_area = "range"){

  if(study_area == "range"){
    area_ext <- range
  }
  
  if(study_area == "presence"){
    occ_sp <- occ
    sp::coordinates(occ_sp) <- ~Longitude + Latitude
    sp::proj4string(occ_sp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    area_ext <- occ_sp
  }
  
  #range bonus
  empty_raster <- raster(ext = extent(area_ext) + c(-1,1,-1,1), 
                         res = res) 
  empty_raster[] <- 0
  
  ID_raster <- empty_raster
  ID_raster[] <- c(1:length(ID_raster))
  
  r_bonus <- rasterize(range,empty_raster) #rasterise range
  r_bonus[!is.na(r_bonus[])] <- range_bonus/100
  r_bonus[is.na(r_bonus[])] <- 0
  
  #occurrence bonus
  presence <- occ[which(occ$occurrence == "TRUE"),] 
  sp::coordinates(presence) <- ~Longitude+Latitude
  sp::proj4string(presence) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +no_defs")
  
  bonus_points <- list()
  for(i in 1:nrow(presence))
  {
    bonus_layer <- empty_raster
    acc_cells <- numeric()
    
    bonus2 <- occ_bonus/100
    j = 1
    while(bonus2 >= 0.01)
    {
      buf_pt <- gBuffer(presence[i,], width = j*res) #make a buffer around point
      cells <- extract(ID_raster,buf_pt)[[1]] #get ID of cells within the buffer
      
      rep <- which(cells %in% acc_cells)
      if(length(rep) != 0){
        cells <- cells[-which(cells %in% acc_cells)]
      }
      
      bonus2 <- bonus2/j^red2
      bonus_layer[cells] <- bonus2
      
      j <- j+1
      acc_cells <- c(acc_cells,cells) #cells that have already been computed
    }
    
    bonus_points[[i]] <- bonus_layer
  }
  
  occ_bonus <- sum(stack(bonus_points))
  
  #absence penalty
  absence <- occ[which(occ$occurrence == "FALSE"),] 
  sp::coordinates(absence) <- ~Longitude+Latitude
  sp::proj4string(absence) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +no_defs")
  
  penalty_points <- list()
  for(i in 1:nrow(absence))
  {
    penalty_layer <- empty_raster
    acc_cells <- numeric()
    
    penalty2 <- abs_pen/100
    j = 1
    while(penalty2 >= 0.01)
    {
      buf_pt <- gBuffer(absence[i,], width = j*res) #make a buffer around point
      cells <- extract(ID_raster,buf_pt)[[1]] #get ID of cells within the buffer
      
      rep <- which(cells %in% acc_cells)
      if(length(rep) != 0){
        cells <- cells[-which(cells %in% acc_cells)]
      }
      
      penalty2 <- penalty2/j^red2
      penalty_layer[cells] <- penalty2
      
      j <- j+1
      acc_cells <- c(acc_cells,cells) #cells that have already been computed
    }
    
    penalty_points[[i]] <- penalty_layer
  }
  
  abs_penalty <- sum(stack(penalty_points))*-1

  # combine three parts of the equation
  rasters <- list(r_bonus,occ_bonus,abs_penalty)
  p_occ <- sum(stack(rasters))
    
  p_occ[p_occ[] >= 0.5] <- 1
  p_occ[p_occ[] < 0.5] <- 0
    
  presence <- occ[which(occ$occurrence == "TRUE"),]
  sp::coordinates(presence) <- ~Longitude+Latitude
  sp::proj4string(presence) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +no_defs")
  
  IDs_presence <- extract(ID_raster,presence) #get the IDs of points where there is a presence
  
  if(length(which(is.na(IDs_presence)))>0){   #eliminate possible NAs
    IDs_presence <- IDs_presence[-which(is.na(IDs_presence))] 
  }
  
  p_occ[IDs_presence] <- 1
  p_occ[which(p_occ[] == 0)] <- NA
  
  new_range <- rasterToPolygons(p_occ, dissolve = T)
  
  return(new_range)
}
