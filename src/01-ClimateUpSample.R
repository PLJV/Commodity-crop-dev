#######################################################################################
#######################################################################################
# RUN THIS FIRST - CREATES A STATISTICAL UP-SAMPLE OF CLIMATE VARIABLES FOR EACH
# COUNTY. ASSUMES THAT THERE ARE CLIMATE RASTERS COVERING ENTIRE STUDY AREA AND
# DEM RASTERS SUB-SET TO EACH COUNTY. PLEASE SOURCE FUNCTIONS IN FUNCTION SECTION. 
#######################################################################################
#######################################################################################
source("src/FUNCTIONS.R")                  # ADDS REQUIRED FUNCTIONS
argv <- commandArgs(trailingOnly=T)

if(length(argv)<1){
  cat(" -- error : ClimateUpSample requires a single argument specifying a project workspace root folder.")                                 
} else {
  path <- argv[1] # default path to root (state-level) folder
}

if(!file.exists(path)){ cat(" -- error: couldn't find the root project directory") }

dirs <- list.dirs(path, recursive=F)                                
ylist <- c("adi", "dd5", "map", "mat", "ffp") 
xraster="elev.tif"
oldDir=getwd()

setwd(path)
for (d in dirs) { # iterate over each FIPS folder in the project workspace
    if(!is.na(as.numeric(substr(d,3,length(d))))) { cat(" -- error: directory", d, "breaks our FIPS project template format.  What's it doing here?\n"); stop(); } # sanity-check this directory                
    setwd(paste(d, "RASTER", sep="/"))
    x <- paste(getwd(),xraster, sep="/") 
    for(r in ylist) {
      RasterUpSample(x=x, y=paste(r, "tif", sep="."), p=0.02, sample.type="random", filename=paste(r, "tif", sep="."))
      gc(verbose=F);
    };
    setwd("../..");  
    gc(verbose=F) 
}
