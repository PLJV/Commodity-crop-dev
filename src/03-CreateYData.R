###############################################################
#   CREATE Y VARIABLE (TILLED/NOTTILLED) BASED ON FREQUENCY   
#   ACCEPTS TWO ARGUMENTS, THE FIRST IS A PATH TO THE PROJECT ROOT FOLDER (e.g., myFolder/TEXAS... containing TX023... etc),
#   THE SECOND IS THE FULL PATH TO A FOLDER CONTAINING THE NASS RASTERS YOU WISH TO USE FOR YOUR RUN. 
#
###############################################################

require(raster)
require(sp)
require(rgdal)

argv <- commandArgs(trailingOnly=T)

t=1 #TILLAGE THRESHOLD 

# sanity-check our project root folder and NASS imagery folder
if(length(argv) < 1){
  path      <- paste("~/PLJV/Workspace/Tillage_Run/TEXAS") # default path, if one isn't specified at runtime.
  nass.path <- paste("~/PLJV/NASS") 
} else {
  path      <- argv[1]
  nass.path <- argv[2]
}
if( (!file.exists(path)) || (!file.exists(nass.path)) ){
  cat(" -- error: couldn't find project root or NASS imagery paths, please double check input.\n",sep=" ");
  stop();
}

parentDir <- getwd()

# parse out our NASS imagery
nass <- list.files(nass.path, pattern="tif$", full.names=T);
  nass <- lapply(nass, FUN=raster);

dirs <- list.dirs(path, recursive=F, full.names=F) # assumes that the root of the directory tree only contains FIPS directories  
c <- as.list(dirs) # sanity-check our directories
  c <- sum(as.numeric(unlist(lapply(c, FUN=substr, start=3, stop=5)))) # anything without a numeric COUNTY FIPS code?
if(is.na(c)){ cat(" -- error: a directory in our project space breaks our FIPS project template format.  What's it doing here?\n"); stop(); }
for (d in dirs) {
  cat(" -- processing", d, ":", sep=" ");
  setwd(paste(path, d, sep="/"));
  spath <- paste(getwd(), "VECTOR", sep="/");
  shape <- readOGR(dsn=spath, layer="sample", verbose=F);

  if ( is.na(match("y", names(shape@data))) == "FALSE") 
    { shape@data <- shape@data[-match("y", names(shape@data))] }	 
  crop <- as.data.frame(array(0, dim=c( dim(shape)[1], 0 )))  
  # sample will contain extractions from each year of the NASS time-series.  If a site was
  # tilled at any point in this time-series, we going to call it "tilled".  There is some potential
  # economic information that we are smoothing over here.  We should change this in the future.  - Kyle T.
  for(i in nass) {
   shape <- spTransform(shape, CRS(projection(i)))
       r <- extract(i, shape);
    crop <- cbind(crop, r);
    cat(".");
  }  
  crop <- data.frame(crop, y=apply(crop, 1, function(x) 
          ifelse(sum(x) >= t, 1, 0) ) )
	shape@data <- data.frame(shape@data, y=crop$y)
    writeOGR(shape, spath, "sample", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
  setwd(oldDir)
  cat("\n");
}; cat("\n")
