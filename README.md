# Commodity Crop Developmnet Suitability Model
This is a (relatively) mature, 'R'-based modeling workflow for PLJV's commodity crop development suitability model.  It accepts a project-area delineation (e.g., as a shapefile) and fetches/generates/processes crop data, climate data, as well as data for topographic and soil conditions thought to be limiting for crop production.  It uses observations of these environmental variables in areas where crops are grown to learn (via Random Forests) what these conditions look like in your project area.  You can then explore what these relationships look like in model space, or make predictions about suitability for certain crop-types in other areas.

This is designed to work on any Unix environment with at least 30 GB of RAM available (for large raster operations).  The more CPU cores, the better.  

# Usage
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
install.packages('devtools', repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('rgdal', repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('rgeos', repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('raster', repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('utils', repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('soilDB', repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('parallel', repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
install.packages('FedData', repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"))
require("devtools")
install_github('ktaylora/landscapeAnalysis')
```
