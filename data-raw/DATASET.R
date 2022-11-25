## code to prepare `DATASET` dataset goes here
files <- list.files('/Users/jedgroev/surfdrive/Shared/Methodology Paper data',full.names = T)
files <- files[!grepl('.Rmd',files)]

# NOTE THAT LARGE DATASETS CANNOT BE SUBMITTED TO CRAN
#### 1. shps ####
turbines <- as.data.table(sf::st_read(files[grepl('.shp',files)]))
usethis::use_data(turbines, overwrite = TRUE)

#### 2. era5 ####
era <- fread(files[grepl('ERA5',files)])
usethis::use_data(era, overwrite = TRUE)

#### 3. tracks ####
tracks_all <- fread(files[grepl('bird-tracks_bare_w1',files)][1])
usethis::use_data(tracks_all, overwrite = TRUE)

#### 4. mask ####
landmask <- fread(files[grepl('landmask',files)])
usethis::use_data(landmask, overwrite = TRUE)


