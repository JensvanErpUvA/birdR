## code to prepare `DATASET` dataset goes here
files <- list.files('/Users/jedgroev/surfdrive/Shared/Methodology Paper data',full.names = T)
files <- files[!grepl('.Rmd',files)]
# shps
turbines <- sf::st_read(files[grepl('.shp',files)])
usethis::use_data(turbines, overwrite = TRUE)

# era5
era <- fread(files[grepl('ERA5',files)])
usethis::use_data(era, overwrite = TRUE)

# tracks
tracks_all <- fread(files[grepl('bird-tracks_bare_w1',files)][1])
usethis::use_data(tracks_all, overwrite = TRUE)

# mask
landmask <- fread(files[grepl('landmask',files)])
usethis::use_data(landmask, overwrite = TRUE)


