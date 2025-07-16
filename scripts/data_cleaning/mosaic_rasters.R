# mosaic prediction chunks from random forest 
# Wesley Rancher


library(terra)
library(dplyr)
#library(foreach)
#library(doParallel)
library(future)

# directories
file_dir <- "data/processed/raster/tiles/"
output_dir <- "data/processed/raster/mosaics/"
#setwd(file_dir)

# read in the rasters 
list_of_files <- list.files(file_dir, pattern = "prediction_tile_.*\\.tif$", full.names = TRUE)
bounds <- vect("F:/Rancher_Sims_Full_Landscape/input/FullLandscapeV3_082722.shp")

# iterate over a sequence of years and pull out files specific to the year in the sequence
years <- 2000:2024
years_string <- as.character(years)


#foreach(year = years_string, .packages = c("terra", "dplyr")) %dopar% {
species <- c("Blackspruce", "Resinbirch", "Quakingaspen", "Whitespruce")
plan(multisession, workers = availableCores() - 1)
for (i in seq_along(years_string)) {
  year <- years_string[[i]]
  files_one_year <- list_of_files[grepl(year, list_of_files)]
  
  for (spp in species){
    #pull out unique year
    files_one_spp <- files_one_year[grepl(spp, files_one_year)]

    #convert to list of rasters
    list_of_rasters <- lapply(files_one_spp, function(file) {
      r <- rast(file)
      r
    })

    # get band names for retention
    band_names <- names(list_of_rasters[[i]])
    #flattened_band_names <- unlist(band_names)
    
    # turn list in sprc and mosaic
    rsrc <- terra::sprc(list_of_rasters)
    m <- mosaic(rsrc)
    cropped <- mask(m, bounds)
    print(paste0(year, " start: ", Sys.time()))
    names(m) <- band_names
    
    
    #save it
    output_filename <- paste0(output_dir, "Biomass_", spp, "_", year, ".tif")
    print(output_filename)
    writeRaster(cropped, filename = output_filename, filetype = "GTiff", overwrite = TRUE)
    print(paste0(year, " finish: ", Sys.time()))
    rm(files_one_spp, list_of_rasters, m, cropped)
    gc()
  }
  rm(files_one_year)
}
stopCluster(cl)

