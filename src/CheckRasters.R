############################
#  WRITE RASTER SUMMARIES  #
############################

require(raster)  

argv <- commandArgs(trailingOnly=T)
path <- argv[1]
setwd(path)
dirs <- list.dirs(path)
       
for (d in 1:length(dirs) ) {                
  cdir=dirs[d]
  setwd(paste(paste(path, cdir, sep="/"), "RASTER", sep="/") )
   rlist <- list.files(pattern="tif$") 
      r.info <- as.data.frame(array(0, dim=c( 0, 12 )))
    names(r.info) <- c("AREA", "RASTER", "MIN", "MAX", "ROWS", "COLS", "NCELLS", "CELLSIZE", "
                        XMIN", "XMAX", "YMIN", "YMAX")
        for (i in rlist ) {            
            r <- raster(i) 
              e <- extent(r)
        	  rsum <- data.frame(AREA=cdir, RASTER=i, MIN=minValue(r), MAX=maxValue(r), 
        	                     ROWS=nrow(r), COLS=ncol(r), NCELLS=ncell(r), 
								 CELLSIZE=res(r)[1], XMIN=e@xmin, XMAX=e@xmax, 
								 YMIN=e@ymin, YMAX=e@ymax)
              r.info <- rbind(rsum, r.info)	  		
        }
		if ( ( d == 1) == TRUE) {
           write.table(r.info, file=paste(path, "RasterSummaries.csv", sep="/"), 		          
					 sep=",", row.names=FALSE) 
	     } else {
           write.table(r.info, file=paste(path, "RasterSummaries.csv", sep="/"), 
		               sep=",", row.names=FALSE, append=TRUE, col.names=FALSE)
	}		
}
