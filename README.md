# birdR
 
A R package to filter birdradar datasets which have as minimal input a track table with the following columns. Several of these columns might be stored alternatively for different radars and require additional data preparation.

| column          | description                                                            | 
|:---------------:|:----------------------------------------------------------------------:|
| id              | unique_identifier                                                      |               
| timestamp_start | UTC timestamp corresponding to the first location of a bird-track      | 
| timestamp_end   | UTC timestamp corresponding to the last location of a bird-track       | 
| trajectory_time | offset seconds from timestamp_start for each location in a bird-track  | 
| trajectory      | linestring geometry corresponding to a bird-track                      | 
: **Table:** Columns bird-trajectory table 


# Installation

## Clone of repository

### Direct installation of birdR through R

#### 1. Install gert (if not installed yet)
```R
install.packages('gert')
```

#### 2. Install birdR
```R
library(gert)
gert::git_clone('http://gitlabdeploytoken658893:z661w3sBPtdvSAHxLCLs@gitlab.com/uva_ibed_ame/robin_radar/birdar', path = d<-tempfile("birdar_"))
devtools::install_local(d, force=T)
```

### Using `remotes`

Once a new version of the remotes package is released (>2.4.1) the following command should work (also see this [issue](https://github.com/r-lib/remotes/issues/670)):


```r
remotes::install_git("http://gitlabdeploytoken658893:z661w3sBPtdvSAHxLCLs@gitlab.com/uva_ibed_ame/robin_radar/birdar.git", force=T, git = 'external')
```

# Usage 

For a typical analysis workflow, please check the [vignette](https://uva_ibed_ame.gitlab.io/robin_radar/birdar/articles/aa-processing.html).  

### data 

Four sample datasets are available within the R-package. 

```r
# tracks (robin radar database)
tracks

# wind turbines at sea of NL 
turbines 

# era5 weather data
era

# landmask (robin radar database)
landmask
```

### area of inclusion 

see more examples at [roi](https://uva_ibed_ame.gitlab.io/robin_radar/birdar/reference/roi.html)

```r
# without exclusion areas 
location <- c(4.185345, 52.42783)
area_of_inclusion <-  roi(location=location,
                         distance=c(2500,1000),
                         crs=23095,
                         excl_angles=c(287,30,115,135))

# with exclusion areas 
location <- c(4.185345, 52.42783)
area_of_inclusion <-  roi(location=location,
                         distance=c(2500,1000),
                         crs=23095,
                         excl_angles=c(287,30,115,135),
                         excl_geom=turbines)
```

### movement and weather annotation 

```r
# sample 
sample <- tracks_all[sample(1:nrow(tracks_all),1000),]

# annotate movement metrics
annotation <- movement(TRACKS=sample,
                       VAR=c('n','duration', 'length', 'groundspeed', 'displacement', 'direction','dot'), 
                       crs=23095)

# annotate weather 
annotation <- weather(x=annotation,
                      era5=era,
                      var=c("U10m","V10m"),
                      unit="hours")
                      
# annotate airspeed     
annotation[,airspeed := mapply(get_airspeed, groundspeed, direction, U10m, V10m)]
```
