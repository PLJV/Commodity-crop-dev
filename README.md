# Commodity Crop Development Suitability Model
This is a (relatively) mature, 'R'-based modeling workflow for [PLJV](http://pljv.org)'s commodity crop development suitability model.  It accepts a project-area delineation (e.g., as a shapefile) and fetches/generates/processes crop data, climate data, topographic data and soil data thought to be limiting factors for crop production.  It uses observations of these environmental variables in areas where crops are grown to learn (via Random Forests) what these conditions look like in your project area.  You can then explore what these relationships look like in model space, or make probablistic predictions about suitability for certain crop-types in other geographies.  

PLJV uses crop suitability predictions for landscape change modeling in our biological planning (e.g., for [Landscape Design](http://pljv.org/for-habitat-partners/landscape-design/)), but this model could be integrated into a number of planning frameworks, including Strategic Habitat Conservation, Structured Decision Making, and Systematic Conservation Planning. We are making it available publically in the hopes that we can solicit feedback from developers, analysts, and planners in order to improve its implementation. 

The workflow is designed to work on any Unix environment with at least 30 GB of RAM available (for large raster operations).  The more CPU cores, the better. If you don't have one in your closet, it could be deployed pretty easily on an Ubuntu instance on [AWS EC2](https://aws.amazon.com/ec2/ "I'm fairly cheap"). 

# Usage (tl;dr)
From bash:
``` bash
R --no-save --vanilla --slave /path/to/project_shapefile.shp < 01_Process_NASS_imagery.R
R --no-save --vanilla --slave /path/to/project_shapefile.shp < 02_Prepare_explanatory_data.R
R --no-save --vanilla --slave /path/to/project_shapefile.shp < 03_Train_Random_Forest.R
```
# Speed Tweaks
We have written the workflow to be as memory and CPU efficient as possible, taking advantage of as many cores and as much RAM as concievable when doing things like processing raster data or parallelizing certain operations.  That said, there are still things you can do to speed-up operation of the model-building workflow.

## 1) Pre-install 'R' package dependencies
The workflow depends on a number of common 'R' packages used for spatial modeling in order to fetch explanatory data and train a model for your project area. You should pre-install these manually to ensure they are available:
``` r
install.packages('devtools',repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('rgdal',repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('rgeos',repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('raster',repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('utils',repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('soilDB',repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('parallel',repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('FedData',repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))

require("devtools")
install_github('ktaylora/landscapeAnalysis')
```
## 2) Specify a pre-cropped DEM for your project area
Workflow (02) will automatically search for a DEM raster named elevation.tif in the current working directory to use to calculate topographic variables for your project area. If one isn't found, the workflow will attempt to use the 'fedData' package to fetch 30m DEM tiles from a (NED) USGS FTP server.  Fetching and mosaicing DEM tiles, even with tweaks in the 'landscapeAnalysis' package, can take time and is prone to problems (like USGS randomly taking their servers offline).  It's preferable to pre-crop your DEM to the extent of your project area so it's available for use at run-time, particularly if you are working across large geographies (e.g., multiple counties).
