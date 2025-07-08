library(sf)
library(dplyr)
library(terra)

#files split by year
cafi_dir <- "data/raw/csv/cafi_plot_measurements_1994_2017/"
files <- list.files(cafi_dir, pattern = "biomass_*", full.names = TRUE)
bounds <- st_read("data/raw/shp/USGS_AK_GageBasinDA_SFR2020/USGS_AK_GageBasinDA_SFR2020.shp")
bounds <- bounds %>% filter(StationNm == "TANANA R AT NENANA AK")
bounds_vector <- vect(bounds)

#bind and remove duplicates for plotting
file_to_df <- function(file){
  df <- read.csv(file)
  
  #convert to sf and then vector for proper clipping
  sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  sf <- st_transform(sf, st_crs(bounds))
  sf_vector <- vect(sf)
  
  #crop to aoi and visualize
  cropped <- crop(sf_vector, bounds_vector)
  plot(bounds_vector)
  
  if (nrow(cropped) > 0) {
    plot(cropped, add = TRUE)
    
    #back to sf in WGS 84 to get lat/long
    cropped_sf <- st_as_sf(cropped)
    cropped_sf_wgs <- st_transform(cropped_sf, crs = 4326)
    coords <- st_coordinates(cropped_sf_wgs)
    cropped_sf_wgs$longitude <- coords[,1]
    cropped_sf_wgs$latitude <- coords[,2]
    cropped_df_wgs <- as.data.frame(cropped_sf_wgs) %>% select(-geometry, -SPP_count)
    write.csv(cropped_df_wgs, paste0("data/processed/csv/",basename(file)),row.names = FALSE)
    return(cropped_df_wgs)
  } else {
    print(basename(file))
    print("has zero geometries")
    return()
  }
}

#apply function and bind for visualization
empirical_sf_objs <- lapply(files, file_to_df)
all_pts_in_aoi <- bind_rows(empirical_sf_objs)%>%
  distinct(latitude, longitude, .keep_all = TRUE)

#save the new aoi
writeVector(bounds_vector, "data/processed/shp/tanana_rb.shp")
writeVector(vect(all_pts_in_aoi, geom = c("longitude", "latitude"), crs="EPSG:4326"), "data/processed/shp/tanana_rb_pts.shp")
