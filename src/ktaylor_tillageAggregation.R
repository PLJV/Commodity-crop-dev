require(raster,quietly=T)
require(snow,quietly=T)
require(rgdal,quietly=T)
require(rgeos,quietly=T)

endCluster()
beginCluster(n=6)

home <- Sys.getenv("HOME")
projectDir <- getwd()

# project folders
shapefiles <- paste(home,"/PLJV/Croplands",sep="")
tillageRaster <- raster(paste(home,"/PLJV/products/tillage_predictions/tillage_likelihood_gplcc_pilot_region.tif",sep=""))

# accepts two arguments at runtime : a two leter state code used to filter the agriculture shapefiles, and an (optional) parent region polygon shapefile that defines the boundaries of our extraction.
argv <- commandArgs(trailingOnly=T)

if(length(argv)>0){ 
  state <- argv[1] 
  if(file.exists(argv[2])){
    path <- unlist(strsplit(path, sep="/"))
      shape <- unlist(strsplit(path[length(path)], "[.]"))[1]
        path <- paste(path[1:(length(path)-1)],collapse="/")
    parentRegion <- c(path,shape)
  }
} else {
  stop("script requires a two-letter state code (e.g., CO) passed as an argument at runtime")
}

cat(" -- reading shapefiles\n")

setwd(shapefiles)

shapes <- unlist(lapply(as.list(list.files(pattern="[.]shp$")), FUN=strsplit, split="[.]"))
  shapes <- shapes[!grepl(shapes, pattern="shp")]
    shapes <- shapes[grepl(shapes,pattern=state)]
      shapes <- shapes[grepl(shapes,pattern="CLU")] # The CLU shapefiles contain the same stuff as CRP

cat(paste("[",shapes,"]",sep=""),"\n")

out <- list();
if(exists("parentRegion")) parentRegion <- spTransform(readOGR(parentRegion[1],parentRegion[2],verbose=F),CRS(projection(tillageRaster)))

for(shape in shapes){
  cat(" -- processing:",shape,"\n")
  cat(" -- ensuring consistent projections between raster and polygons\n")
  s<-readOGR(".",shape,verbose=F)
    if(projection(s) != projection(tillageRaster)) s<-spTransform(s,CRS(projection(tillageRaster)))
      if(exists("parentRegion")){ s<-s[!is.na(as.vector(sp::over(s,parentRegion))),] }
      
  cat(" -- calculating zonal statistics for:",shape,"\n") 

  focal <- extract(tillageRaster,s,fun=mean,na.rm=T)
    s@data <- data.frame(prob=focal)
  cat("\n -- rasterizing zonal polygons\n")
  out[[length(out)+1]] <- rasterize(s,crop(raster(tillageRaster),s),field="prob", progress="text")
}

# merge and save our output
out <- do.call(raster::merge, out)
  setwd(projectDir)
    writeRaster(out,paste(state,"field_aggregated","tif",sep="."),overwrite=T)

