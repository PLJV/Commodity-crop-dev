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
# downsamplePtsToMinimum()
#
downsamplePtsToMinimum <- function(x,y){
  if(nrow(x@coords) > nrow(y@coords)){
    x<-x[sample(1:nrow(x@coords),size=nrow(y@coords)),]
  } else if(nrow(y@coords) > nrow(x@coords)){
    y<-y[sample(1:nrow(y@coords),size=nrow(x@coords)),]
  }
  names(x) <- "response"
  names(y) <- "response"
  return(rbind(x,y))
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
  cat(" -- building cropped / not-cropped surfaces for each year in nass time-series: ")
  values <- read.csv("2014_nass_cell_values_not_farmed.csv")
  for(i in 1:length(nassImagery)){
    cat(".");r <- subs(nassImagery[[i]], y = values, by = 1, which = 2, subsWithNA = F)
    cat(".");r[r>0] <- 1
    cat(".");writeRaster(r,filename=gsub(names(nassImagery[[i]]),pattern="X",replacement="farmed_binary_"),format="GTiff",overwrite=T)
    cat("+")
  }; cat("\n")

  r <- stackApply(stack(list.files(pattern="^farmed_")),indices=1,fun=sum)
    r[r>0] <- 1;

  writeRaster(r,filename=paste(argv[2],"_farmed_binary",sep=""),format="GTiff",overwrite=T)
    file.remove(list.files(pattern="^farmed_")) # clean-up
}
# Generate samples of presence/absence data
presences <- raster::sampleRandom(subs(r,data.frame(1,1),by=1,which=2,subsWithNA=T),size=round(ncell(r)*0.01),na.rm=T,sp=T) # randomly sample 1% of the raster surface for presence training data
 absences <- raster::sampleRandom(subs(r,data.frame(0,1),by=1,which=2,subsWithNA=T),size=round(ncell(r)*0.01),na.rm=T,sp=T)
  absences@data[,1] <- 0
pts <- downsamplePtsToMinimum(presences,absences); rm(presences,absences)
writeOGR(pts,".",paste(argv[2],"_farmed_binary_pts",sep=""), driver="ESRI Shapefile",overwrite=T)
}
