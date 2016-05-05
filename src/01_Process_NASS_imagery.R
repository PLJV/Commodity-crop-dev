#
# Accepts two arguments: (1) is a directory path containing processed (GeoTIFF) NASS rasters
# and (2) is a focal_county_NNNNN shapefile layer in the CWD.  This script is designed to be called from 00_
#

argv <- commandArgs(trailingOnly=T)

include <- function(x,from="cran",repo=NULL){
  if(from == "cran"){
    if(!do.call(require,as.list(x))) install.packages(x, repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"));
    if(!do.call(require,as.list(x))) stop("auto installation of package ",x," failed.\n")
  } else if(from == "github"){
    if(!do.call(require,as.list(x))){
      if(!do.call(require,as.list('devtools'))) install.packages('devtools', repos=c("http://cran.revolutionanalytics.com","http://cran.us.r-project.org"));
      require('devtools');
      install_github(paste(repo,x,sep="/"));
    }
  } else{
    stop(paste("could find package:",x))
  }
}

include('rgdal')
include('raster')
include('utils')
include('landscapeAnalysis')

#
# getNASSValuesByCropName()
#
getNASSValuesByCropName <- function(y=NULL,t=NULL){
  if(is.null(t)) t <- read.csv(list.files(pattern="csv$")[1])
    return(as.numeric(t[,1][grepl(tolower(t[,2]),pattern=tolower(y))]))
}
#
# dropEmptyClasses()
#
dropEmptyClasses <- function(r=NULL,rebalance=T){
  if(!inherits(r,"Raster")) stop(" r= argument to dropEmptyClasses() was not a raster object.")
  cutoff  <- ceiling(raster::ncell(r)*0.001)  # determine a cut-off value consistent with a fraction of 1% of the landscape.
  classes <- sampleRandom(r,size=raster::ncell(r)*0.5,sp=T)
    names(classes) <- "response"

  keep <- which(table(classes@data[,1]) > cutoff)
    keep <- as.numeric(names(keep))
  classes <- classes[classes$response %in% keep,]
  if(rebalance){
    min <- table(classes@data[,1])
      min <- min(min[min>0])

    final <- list()
    for(i in unique(classes@data[,1])){
      focal <- classes[classes@data[, 1] == i,]
      final[[length(final)+1]] <- focal[sample(1:nrow(focal),size=min),]
    }

    return(do.call(rbind,final))
  }
}
#
# MAIN
#

cat(" -- processing NASS imagery for focal county:",argv[2],"\n")

if(sum(grepl(list.files(pattern="shp"),pattern=paste(argv[2],"_farmed_binary_pts",sep="")))==0){
  nassImagery <- lapply(list.files(argv[1],pattern="cdls.*tif$",full.names=T),FUN=raster)
            b <- spTransform(readOGR(".",argv[2],verbose=F),CRS(projection(nassImagery[[1]])))

  nassImagery <- lapply(nassImagery,FUN=raster::crop,b)

  # if we haven't built a farmed/not-farmed surface for the focal county, do it now. Eventually this will need to be parsed out
  # to handle individual crop types and class balancing
  if(!file.exists(paste(argv[2],"_farmed_binary.tif",sep=""))){
    cat(" -- building cropped / not-cropped surfaces for majority crops in nass time-series: ")
    grains <- c(29,24,27)          # lump our 'grains' -- winter wheat, millet, rye
    grasses <- c(176)              # actual 'grass'
    crop_grasses <- c(36,4)        # alfalfa and sorghum
    corn   <- 1
    cotton <- 2
    beans <- c(5,42)
    for(i in 1:length(nassImagery)){
      cat(".");r <- subs(nassImagery[[i]], y = data.frame(ori=grains,dst=c(1,1,1)), by = 1, which = 2, subsWithNA = F)
      cat(".");r <- subs(nassImagery[[i]], y = data.frame(ori=crop_grasses,dst=c(2,2)), by = 1, which = 2, subsWithNA = F)
      cat(".");r <- subs(nassImagery[[i]], y = data.frame(ori=beans,dst=c(3,3)), by = 1, which = 2, subsWithNA = F)
      cat(".");r <- subs(nassImagery[[i]], y = data.frame(ori=corn,dst=4), by = 1, which = 2, subsWithNA = F)
      cat(".");r <- subs(nassImagery[[i]], y = data.frame(ori=cotton,dst=5), by = 1, which = 2, subsWithNA = F)
      cat(".");r <- subs(nassImagery[[i]], y = data.frame(ori=grasses,dst=6), by = 1, which = 2, subsWithNA = F)
      cat(".");r[r>6] <- 0
      cat(".");writeRaster(r,filename=gsub(names(nassImagery[[i]]),pattern="X",replacement="farmed_binary_"),format="GTiff",overwrite=T)
      cat("+")
    }; cat("\n")

    r <- stack(list.files(pattern="^farmed_")) # take the mode crop type across the time-series as representative of crop type for the given area
      r <- raster::calc(r,fun=landscapeAnalysis::Mode)

    writeRaster(r,filename=paste(argv[2],"_farmed_binary",sep=""),format="GTiff",overwrite=T)
      file.remove(list.files(pattern="^farmed_")) # clean-up
  }

  # Generate sample of points with balanced class sizes
  pts <- dropEmptyClasses(r)
  rgdal::writeOGR(pts,".", paste(argv[2],"_farmed_binary_pts",sep=""), driver="ESRI Shapefile", overwrite=T)
}
