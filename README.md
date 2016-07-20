# Commodity Crop Developmnet Suitability Model
This is a (relatively) mature, 'R'-based modeling workflow for a commodity crop development suitability model.  It accepts a project-area delineation (e.g., as a shapefile) and fetches/generates/processes crop data, climate data, as well as data for topographic and soil conditions thought to be limiting for crop production.  It uses observations of these environmental variables in areas where crops are grown to learn (via Random Forests) what these conditions look like in your project area.  You can then explore what these relationships look like in model space, or make predictions about suitability for certain crop-types in other areas.

This is designed to work on any Unix environment with at least 30 GB of RAM available (for large raster operations).  The more CPU cores, the better.  

# Usage
``` r
R --no-save --vanilla --slave /path/to/project_shapefile.shp < 01_Process_NASS_imagery.R
R --no-save --vanilla --slave /path/to/project_shapefile.shp < 02_Prepare_explanatory_data.R
R --no-save --vanilla --slave /path/to/project_shapefile.shp < 03_Train_Random_Forest.R
```
