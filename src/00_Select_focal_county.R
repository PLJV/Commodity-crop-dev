#
# Accepts two arguments, the first is a zipfile containing county polygons and FIPS codes (as provided by census.gov), and the
# second argument is the 5-character State + FIPS code for our target county.  This is designed to be parallelized across a number of counties,
# one strategy may be to identify the target counties in a GIS and dump the corresponding FIPS for a parallel run into a text file.
#
# e.g., 02001, 52113
#

require(rgdal)
require(raster)
require(utils)

argv <- commandArgs(trailingOnly=T)

unpackCountiesZipfile <- function(x){
  # if x is a complete filename with extension, let's strip the extension out for our work
  if(grepl(x,pattern="[.]zip")) {
    o <- strsplit(x,"[.]")
      o <- as.vector(o[[1]][length(o[[1]])-1])
    o <- strsplit(o,"/")[[1]]
      o <- o[length(o)]

    suppressWarnings(dir.create("/tmp/counties"))
    utils::unzip(zipfile=x, exdir=paste("/tmp/counties/",o,sep=""),overwrite=F)
  }
  # by default, return as a layer to the user
  return(readOGR(paste("/tmp/counties/",o,sep=""),o,verbose=F))
}

processFocalCounty <- function(x){
  b <- unpackCountiesZipfile(x)
  # build a unique identifier field from a composite of STATE and COUNTY FPs.
  b$UID <- paste(sprintf("%02d",as.numeric(b$STATEFP)),sprintf("%03d",as.numeric(b$COUNTYFP)),sep="")
  # parse our shapefile by the UID passed by our user
  focal <- b[b$UID == argv[2],]
  if(nrow(focal)>1){
    warning("more than one county matched the UID specified at runtime -- this shouldn't happen.  Just using the first occurrence as our focal county")
    b <- b[1,]
  }
  return(b)
}

b <- processFocalCounty(argv[1])
