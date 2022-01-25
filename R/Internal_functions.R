#' @importFrom rgeos gBuffer
#' @importFrom raster rasterize extract raster
#' @importFrom sp coordinates proj4string
#' 
rangeBonus <- function(range, range_bonus, res){
  
  empty_raster <- raster(res = res) 
  empty_raster[] <- c(1:length(empty_raster))
  
  range_raster <- rasterize(range,empty_raster) #rasterise range
  range_raster[!is.na(range_raster[])] <- range_bonus/100
  range_raster[is.na(range_raster[])] <- 0
  
  return(range_raster)
}

occurrenceBonus <- function(occ, occ_bonus, res, red2){
  
  presence <- occ[which(occ$occurrence == "TRUE"),] 
  coordinates(presence) <- ~Longitude+Latitude
  proj4string(presence) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +no_defs")
  
  empty_raster <- raster(res = res) 
  empty_raster[] <- 0
  
  ID_raster <- empty_raster
  ID_raster[] <- c(1:length(ID_raster))
    
  bonus_points <- list()
  for(i in 1:nrow(presence))
  {
    bonus_layer <- empty_raster
    acc_cells <- numeric()
    
    bonus2 <- occ_bonus/100
    j = 1
    while(bonus2 >= 0.01)
    {
      buf_pt <- gBuffer(presence[i,], width = j) #make a buffer around point
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
  
  bonus_points <- sum(stack(bonus_points))
  return(bonus_points)
}

absencePenalty <- function(occ, abs_pen, res, red2){
  
  absence <- occ[which(occ$occurrence == "FALSE"),] 
  coordinates(absence) <- ~Longitude+Latitude
  proj4string(absence) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +no_defs")
  
  empty_raster <- raster(res = res) 
  empty_raster[] <- 0
  
  ID_raster <- empty_raster
  ID_raster[] <- c(1:length(ID_raster))
  
  penalty_points <- list()
  for(i in 1:nrow(absence))
  {
    penalty_layer <- empty_raster
    acc_cells <- numeric()
    
    penalty2 <- abs_pen/100
    j = 1
    while(penalty2 >= 0.01)
    {
      buf_pt <- gBuffer(absence[i,], width = j) #make a buffer around point
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
  
  penalty_points <- sum(stack(penalty_points))*-1
  return(penalty_points)
}
