
#
# Resample raster data to a focal DEM
#
# Author: Kyle Taylor (kyle.taylor@pljv.org), based on original work in GRASS by Jeff Evans, TNC.
#

require(rgdal, quietly=T)
require(raster, quietly=T)
require(topmodel, quietly=T)
require(gdata, quietly=T)

if(require(snow)) beginCluster(n=3)

argv <- commandArgs(trailingOnly=T)

# sanity-check our input

if(file.exists(argv[1])){
	workspaceRoot <- argv[1]
} else {
    cat(" -- error: ", workspaceRoot, " (path not found)\n");
    stop()
}

counties <- list.dirs(workspaceRoot, recursive=F, full.names=F)
  if(length(counties)<1){ stop("couldn't find any FIPS county folders in the root workspace directory") }

for(d in counties){
	# set-up our workspace
	                elev <- raster(paste(workspaceRoot,d,"RASTER","elev.tif", sep="/"))
	      climateRasters <- paste(c("adi", "dd5", "ffp", "map", "mat"), "tif", sep=".")
	climateRasters_paths <- paste(workspaceRoot, d, "RASTER", climateRasters, sep="/")
	      climateRasters <- lapply(as.list(climateRasters_paths), FUN=raster)

	## perform a bilinear interpolation of our climate rasters, writing output as we go
	cat(" -- resampling\n")
	for(i in 1:length(climateRasters)){
		cat(" -- ", climateRasters[[i]]@data@names, "\n", sep="")
		climateRasters[[i]] <- raster::resample(x=climateRasters[[i]], y=elev, method='bilinear', progress="text")
		writeRaster(climateRasters[[i]], filename=climateRasters_paths[i], overwrite=T)
	}

	## CTI
	cat(" -- calculating CTI\n")

	cti <- raster(topmodel::topidx(as.matrix(elev), res=30)[[1]], crs=CRS(projection(elev)))
	writeRaster(cti, filename=paste(workspaceRoot,d,"RASTER","cti.tif", sep="/"), overwrite=T)

	## Surface Relief Ratio

	# 3x3
	mean_r <- raster::focal(elev, w=matrix(1/9,nrow=3,ncol=3), fun=mean);
	 min_r <- raster::focal(elev, w=matrix(1/9,nrow=3,ncol=3), fun=min);
	 max_r <- raster::focal(elev, w=matrix(1/9,nrow=3,ncol=3), fun=max);
	  srr3 <- (mean_r - min_r)/(max_r - min_r);
	  writeRaster(srr3, filename=paste(workspaceRoot,d,"RASTER","srr3.tif", sep="/"), overwrite=T)

	# 27x27
	mean_r <- raster::focal(elev, w=matrix(1/729,nrow=27,ncol=27), fun=mean);
	 min_r <- raster::focal(elev, w=matrix(1/729,nrow=27,ncol=27), fun=min);
	 max_r <- raster::focal(elev, w=matrix(1/729,nrow=27,ncol=27), fun=max);
	 srr27 <- (mean_r - min_r)/(max_r - min_r);
	 writeRaster(srr27, filename=paste(workspaceRoot,d,"RASTER","srr27.tif", sep="/"), overwrite=T)

	rm(mean_r,min_r,max_r);
	gc(verbose=F);

	## Slope position

	mean_r <- raster::focal(elev, w=matrix(1/121,nrow=11,ncol=11), fun=mean);
	spost  <- elev - mean_r;
	writeRaster(spost, filename=paste(workspaceRoot,d,"RASTER","spost.tif", sep="/"), overwrite=T)

	## Topographic Roughness Index

	# 3x3 SCALE
	rough3   <- raster::focal(elev, w=matrix(1/9,nrow=3,ncol=3), fun=var);
	writeRaster(rough3, filename=paste(workspaceRoot,d,"RASTER","rough3.tif", sep="/"), overwrite=T)
	# 27x27 SCALE		
	rough27 <- raster::focal(elev, w=matrix(1/729,nrow=27,ncol=27), fun=var);
	writeRaster(rough27, filename=paste(workspaceRoot,d,"RASTER","rough27.tif", sep="/"), overwrite=T)

	## SLOPE, SLOPE*SIN(ASPECT), SLOPE*COS(ASPECT)

	# slope
	slope_r <- raster::terrain(elev, opt='slope', unit='degrees', neighbors=8)
	  slope_r <- tan(slope_r)*100 # Slope (deg) -> Slope (%)
	    writeRaster(slope_r, filename=paste(workspaceRoot,d,"RASTER","slope.tif", sep="/"), overwrite=T)

	asp_r <- raster::terrain(elev, opt='aspect', unit='degrees', neighbors=8)

	# COS
	scosa <- slope_r*cos(asp_r/57.296)
	  writeRaster(scosa, filename=paste(workspaceRoot,d,"RASTER","scosa.tif", sep="/"), overwrite=T)

	# SIN
	ssina <- slope_r*sin(asp_r/57.296)
	  writeRaster(ssina, filename=paste(workspaceRoot,d,"RASTER","ssina.tif", sep="/"), overwrite=T)

	## TRASP
	trasp <- (1.0-cos(0.01745*(asp_r-30.0))/2.0)
	  writeRaster(trasp, filename=paste(workspaceRoot,d,"RASTER","trasp.tif", sep="/"), overwrite=T)
}

endCluster()
