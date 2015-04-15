#######################################################################################
#######################################################################################
# RUN THIS SECOND - CREATES A RANDOM SAMPLE FOR EACH SSURGO POLYGON IN EACH COUNTY. 
# THE SAMPLE IS THE USED TO PULL VALUES FROM THE NASS RASTERS AND CREATE A RESPONSE (y)
# VARIABLE FOR USE IN THE CROP MODEL. PLEASE SOURCE FUNCTIONS IN FUNCTION SECTION.
#######################################################################################
#######################################################################################
require(rgdal)
require(MASS)
require(raster)

source("src/FUNCTIONS.R") 

argv <- commandArgs(trailingOnly=T) # accepts a single argument; the root path to our project space, containing state-county up-sampled data (e.g., C:/myFiles; containing TX023... etc. )

if(!file.exists(argv[1])) {
  cat(" -- error: path to upsampled state-county data not found.\n"); 
  stop() ;
}

#######################################################
# PERCENT AREA RANDOM POINT SAMPLE OF SSURGO POLYGONS #
####################################################### 

# SHAPEFILE NAME
inshape="SSURGO"

# PERCENT SUBSAMPLE
p=0.02

setwd(argv[1])
dirs <- list.dirs(recursive=F)
for(d in dirs){
  setwd(d);
  spath=paste(getwd(), "VECTOR", sep="/");
  cat(" -- processing:", spath, "\n", sep=" ");
  shape <- try(readOGR(dsn=spath, layer=inshape, verbose=F));
  if(class(shape) == "try-error"){ 
    cat(" -- error:",spath,"didn't contain a SSURGO.shp file.\n",sep=" "); 
    stop(); 
  } else {
    csamp <- psample(shape, pct=p, join=FALSE, msamp=2, sf=4046.8, 
                     stype="random", iter=100);
    writeOGR(csamp, spath, "sample", driver="ESRI Shapefile");
  }
  setwd("..")
}
cat(" -- done.\n")
