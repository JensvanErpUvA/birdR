# birdar

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

There are a few options to install the package some examples below.

## Direct download

Download the zip file directly https://gitlab.com/uva_ibed_ame/swiss_radar/uvaSBRS/-/archive/main/uvaSBRS-main.zip

Then use this file to install

```R
remotes::install_local('~/Downloads/birdar-main.zip')
```

This can either be done directly, or by starting a new R project from version control.

Then if R is running the repository/project directory it can be directly installed.

```R
remotes::install_local()
```

## Clone of repository

### Direct installation of uvaSBRS through R

#### 1. Install gert (if not installed yet)
```R
install.packages('gert')
```

#### 2. Install uvaSBRS
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

### area of inclusion 

```r
# inclusion ring

# location as vector
location <- c(4.185345, 52.42783)
area_of_inclusion <-  roi(location=location,
                         distance=c(2500,1000),
                         crs=23095)

# location as geom
# 4326
location <- st_sfc(st_point(c(4.185345, 52.42783)),crs=4326)
area_of_inclusion <-  roi(location=location,
                         distance=c(2500,1000),
                         crs=23095)
# 3035
location <- st_sfc(st_point(c(3925960, 3273397)),crs=3035)
area_of_inclusion <-  roi(location=location,
                         distance=c(2500,1000),
                         crs=23095)
## distance

# one distance value: no exclusion of area near radar
location <- st_sfc(st_point(c(4.185345, 52.42783)),crs=4326)
area_of_inclusion <-  roi(location=location,
                         distance=2500,
                         crs=23095)
# two distance values: no exclusion of area near radar
location <- st_sfc(st_point(c(4.185345, 52.42783)),crs=4326)
area_of_inclusion <-  roi(location=location,
                         distance=c(2500,1000),
                         crs=23095)

## exclusion angles
location <- st_sfc(st_point(c(4.185345, 52.42783)),crs=4326)

# exclusion angles can be a list, vector or matrix

# list
area_of_inclusion <-  roi(location=location,
                         distance=c(2500,1000),
                         crs=23095,
                         excl_angles=list(c(287,30),c(115,135)))

# vector
area_of_inclusion <-  roi(location=location,
                         distance=c(2500,1000),
                         crs=23095,
                         excl_angles=c(287,30,115,135))

# matrix
area_of_inclusion <-  roi(location=location,
                         distance=c(2500,1000),
                         crs=23095,
                         excl_angles=matrix(c(287,30,115,135),nrow=2,ncol=2,byrow=T))

# do not exclude areas near the radar
area_of_inclusion <-  roi(location=location,
                         distance=c(2500),
                         crs=23095,
                         excl_angles=matrix(c(287,30,115,135),nrow=2,ncol=2,byrow=T))

## exclusion areas

# without buffer points will not be excluded
location <- st_sfc(st_point(c(4.185345, 52.42783)),crs=4326)

area_of_inclusion <-  roi(location=location,
                         distance=c(2500,1000),
                         crs=23095,
                         excl_angles=c(287,30,115,135),
                         excl_geom=turbines)

# buffer outside the function
polygons <- st_buffer(st_transform(turbines$geometry,23095),100)
area_of_inclusion <-  roi(location=location,
                         distance=c(2500,1000),
                         crs=23095,
                         excl_angles=c(287,30,115,135),
                         excl_geom=polygons)
# buffer within the function
area_of_inclusion <-  roi(location=location,
                         distance=c(2500,1000),
                         crs=23095,
                         excl_angles=c(287,30,115,135),
                         excl_geom=turbines,
                         excl_buffer=100
)

# vary the buffer distance based on distance from radar
dist <- st_distance(location,turbines$geometry)
weight <- as.vector(dist/max(dist) * 100)

area_of_inclusion <-  roi(location=location,
                         distance=c(2500,1000),
                         crs=23095,
                         excl_angles=c(287,30,115,135),
                         excl_geom=turbines,
                         excl_buffer=weight
)

# apply a buffer around multiple locations by using lists
area_of_inclusion <-  roi(location=location,
                         distance=2500,
                         crs=23095,
                         excl_angles=c(287,30,115,135),
                         excl_geom=list(turbines,location),
                         excl_buffer=list(100,1000)
)

# apply a buffer around multiple locations by using lists and varying buffer weight within a single dataset
dist <- st_distance(location,turbines$geometry)
weight <- as.vector(dist/max(dist) * 100)
area_of_inclusion <-  roi(location=location,
                         distance=2500,
                         crs=23095,
                         excl_angles=c(287,30,115,135),
                         excl_geom=list(turbines,location),
                         excl_buffer=list(weight,1000)
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

## Description
Let people know what your project can do specifically. Provide context and add a link to any reference visitors might be unfamiliar with. A list of Features or a Background subsection can also be added here. If there are alternatives to your project, this is a good place to list differentiating factors.

## Badges
On some READMEs, you may see small images that convey metadata, such as whether or not all the tests are passing for the project. You can use Shields to add some to your README. Many services also have instructions for adding a badge.

## Visuals
Depending on what you are making, it can be a good idea to include screenshots or even a video (you'll frequently see GIFs rather than actual videos). Tools like ttygif can help, but check out Asciinema for a more sophisticated method.

## Installation
Within a particular ecosystem, there may be a common way of installing things, such as using Yarn, NuGet, or Homebrew. However, consider the possibility that whoever is reading your README is a novice and would like more guidance. Listing specific steps helps remove ambiguity and gets people to using your project as quickly as possible. If it only runs in a specific context like a particular programming language version or operating system or has dependencies that have to be installed manually, also add a Requirements subsection.

## Usage
Use examples liberally, and show the expected output if you can. It's helpful to have inline the smallest example of usage that you can demonstrate, while providing links to more sophisticated examples if they are too long to reasonably include in the README.

## Support
Tell people where they can go to for help. It can be any combination of an issue tracker, a chat room, an email address, etc.

## Roadmap
If you have ideas for releases in the future, it is a good idea to list them in the README.

## Contributing
State if you are open to contributions and what your requirements are for accepting them.

For people who want to make changes to your project, it's helpful to have some documentation on how to get started. Perhaps there is a script that they should run or some environment variables that they need to set. Make these steps explicit. These instructions could also be useful to your future self.

You can also document commands to lint the code or run tests. These steps help to ensure high code quality and reduce the likelihood that the changes inadvertently break something. Having instructions for running tests is especially helpful if it requires external setup, such as starting a Selenium server for testing in a browser.

## Authors and acknowledgment
Show your appreciation to those who have contributed to the project.

## License
For open source projects, say how it is licensed.

## Project status
If you have run out of energy or time for your project, put a note at the top of the README saying that development has slowed down or stopped completely. Someone may choose to fork your project or volunteer to step in as a maintainer or owner, allowing your project to keep going. You can also make an explicit request for maintainers.
