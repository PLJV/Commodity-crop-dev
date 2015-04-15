#######################################################################################
#######################################################################################
# 04 - RANDOM FORESTS TILLAGE MODEL WITH MODEL SELECTION AND VALIDATION. PRODUCES A 
#   RASTER OF PREDICTED PROBABILITIES. LOOPS THROUGH ALL COUNTIES. PIXEL LEVEL.
#######################################################################################
#######################################################################################
source("~/PLJV/Workspace/Scripts/TNC_Code/FUNCTIONS.R")
#state="TEXAS"
#state.abvr="TX"

#path=paste("D:/My_Projects/Tillage_Risk/CROP", state, sep="/")
#fpath=paste(paste("D:/My_Projects/Tillage_Risk/CROP", state, sep="/"), "RESULTS", sep="/")

argv <- commandArgs(trailingOnly=T)

#
# sanity-check our project root folder 
#

if(length(argv) < 1){
  path      <- paste("~/PLJV/Workspace/Tillage_Run/TEXAS") # default path, if one isn't specified at runtime.
} else {
  path      <- argv[1]
}
if(!file.exists(path)){
  cat(" -- error: couldn't find project root directory, please double check input.\n",sep=" ");
  stop();
}

dirs <- list.dirs(path, full.names=F, recursive=F)

t <- as.list(dirs) # sanity-check our directories
  t <- sum(as.numeric(unlist(lapply(t, FUN=substr, start=3, stop=5)))) # anything without a numeric COUNTY FIPS code?
    t <- t[is.na(as.numeric(unlist(lapply(t, FUN=substr, start=3, stop=5))))]

#
# iterate over our counties, processing with RF as we go
#

# default shapefile input and raster output designations
snapExtent <- rgdal::readOGR(path, "targetCounties", verbose=F)
  snapExtent <- spTransform(snapExtent, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    snapExtent <- extent(snapExtent)
      snapExtent <- raster(snapExtent, res=0.0002951271)

   inshape <- "sample"
     rtype <- "tif"
   praster <- "cprob"

# define the input raster variables we will treat as factors
flist <- c("drclassdcd", "drclasswet", "flodfreqdc", "flodfreqma", 
           "hydclprs", "hydgrpdcd", "pondfreqpr", "slopegradd", 
           "wtdepannmi", "wtdepaprju")

###### START LOOP FOR ALL MODELS ######      
     
for (d in dirs) {                
  setwd(paste(path, d, sep="/"))	
  
  fpath <- paste(getwd(), "RESULTS", sep="/")
    if(!file.exists(fpath)){ dir.create(fpath) }
  spath <- paste(getwd(), "VECTOR", sep="/") 
  rpath <- paste(getwd(), "RASTER", sep="/")
  
  cat(" -- processing county", getwd(), "\n", sep=" : ")
	
  # READ SHAPEFILE
  csamp <- readOGR(dsn=spath, layer=inshape, verbose=F)
  
  #############################################  			 
  #############################################
  # ASSIGN RASTER VALUES TO POINTS            #
  #############################################
  #############################################

  rlist <- list.files(rpath, pattern="tif$", full.names=TRUE); 

  chkProjections <- as.list(rlist)
    chkProjections <- lapply(chkProjections, FUN=raster)
      chkProjections <- unique(unlist(lapply(chkProjections, FUN=projection)))
        if(length(chkProjections)>1) { cat(" -- error: inconsistent projections in input rasters.\n"); stop(); }

  # make sure we are not using elevation or the FIPS raster template as explanatories...
  rlist <- rlist[!grepl(rlist,pattern="elev")]               
  rlist <- rlist[!grepl(rlist,pattern=paste(tolower(d), "img", sep="."))]
  rlist <- rlist[!grepl(rlist,pattern=paste(rpath, paste(tolower(d), "tif", sep=".")))]

  xvars <- stack(rlist);
  csamp <- spTransform(csamp, CRS(projection(xvars[[1]])))  
   v <- extract(xvars, csamp)
     v <- as.data.frame(v)
  csamp@data = data.frame(csamp@data, v[match(rownames(csamp@data), rownames(v)),])

  # writeOGR(csamp, dsn=spath, layer="Sample", driver="ESRI Shapefile")

  # FACTORIZE SSURGO VARIABLES IN POINT SAMPLE; keep other variables as continuous vectors
  for(i in flist) {
     csamp@data[,i] <- factor(csamp@data[,i])
  }
	
  #####################################################################
  #####################################################################
  # SCREEN VARIABES FOR MISSING DATA, UNUSABLE FACTORS AND INVARIANCE #
  #####################################################################
  #####################################################################
  # REMOVE VARIABLES WITH > 10% PERCENT MISSING VALUES
  scol=3
  vars <- names(csamp@data[,scol:ncol(csamp@data)])
   r <- as.vector(array(0, dim=c((0),(1)))) 
   pct <- 0.05
   for(i in 1:length(vars) ) {   
      p <- colSums(is.na(csamp@data[,scol:ncol(csamp@data)])) / 
            colSums(!is.na(csamp@data[,scol:ncol(csamp@data)]))   
  	  if ( (p[i] > pct) == TRUE) { r <- as.vector(append(r, vars[i], after=length(r))) }
   }
   
   if ( (length(r) > 0) == TRUE) { csamp@data <- csamp@data[-match(r, names(csamp@data))]  }
   	
  # REMOVE VARIABLES WITH INVARIANCE OR UNUSABLE FACTORS (x<2 OR x>30)
  vars <- names(csamp@data[,scol:ncol(csamp@data)])
    r <- as.vector(array(0, dim=c((0),(1)))) 	
    for(i in vars ) {        
  	  if (is.factor(csamp@data[,i]) == TRUE ) {
  	      if( (nlevels(csamp@data[,i]) > 30) == TRUE) { r <- as.vector(append(r, i, after=length(r))) }
  	      if( (nlevels(csamp@data[,i]) < 2) == TRUE) { r <- as.vector(append(r, i, after=length(r))) }   
  	   }	 	  
  	  if ( (is.numeric(csamp@data[,i]) ) == TRUE ) {
  	    if( ( range(csamp@data[,i], na.rm=TRUE)[1] == range(csamp@data[,i], na.rm=TRUE)[2] ) == TRUE)	
  	         { r <- as.vector(append(r, i, after=length(r))) }      
  	   }
      }
  
  cat(" -- explanatory variables:")
  cat(r, "\n");

   if ( (length(r) > 0) == TRUE) { csamp@data <- csamp@data[-match(r, names(csamp@data))]  }
   
  # REMOVE NA ROW OBSERVATIONS 
  csamp@data <- na.omit(csamp@data) 
   
  # RE-FACTORIZE SSURGO VARIABLES IN POINT SAMPLE 
  vars <- names(csamp@data[,scol:ncol(csamp@data)])
    for(i in vars) {
       if (is.factor(csamp@data[,i]) == TRUE ) { 
         csamp@data[,i] <- factor(csamp@data[,i])
     	  } 
      }

  ######################################################################## 
  ########################################################################
  #          RANDOM FOREST TILLAGE MODEL AND RASTER PREDICTION           #
  ########################################################################
  ########################################################################
  # NUMBER OF BOOTSTRAP REPLICATES
  b=501 
    
  # CREATE X,Y DATA
  ydata <- as.factor(csamp@data[,"y"])
  xdata <- csamp@data[,scol:ncol(csamp@data)]

  save.image("session.RData")

  ( rf.model <- rf.modelSel(x=xdata, y=ydata, imp.scale="se", ntree=b, 
                            strata=ydata, final=FALSE, na.action=na.omit,
  						  plot.imp=FALSE) )

  # IF ERROR IS WITHIN THRESHOLD SELECT MORE PARSIMONOUS MODEL						  
  thresh=0.03
  error <- apply(rf.model$TEST[,2:3], MARGIN=1, FUN=sum)
  nparm <- rf.model$TEST[,4] 
  x <- vector()
    for (i in 2:length(error)) {
       if( abs(error[1] - error[i]) <= thresh) {
  	   x <- append(x, "TRUE", after=length(x))
  		  } else {
  	   x <- append(x, "FALSE", after=length(x))
  	}
    }
  epost <- as.vector(1)
   epost <- append(epost, which(x == "TRUE")+1)
     nparm <- nparm[epost]
    if( length(epost) > 1 ) { 
  	 min.post <- which(nparm  == min(nparm))	 	 
      index=as.numeric(rownames(rf.model$TEST))[min.post]
      sel.vars=unlist(rf.model$PARAMETERS[index])	 	 
      } else {
        sel.vars <- rf.model$SELVARS
      }						  
    						  
  # RUN FINAL MODEL 
    rf.data <- data.frame(y=ydata, xdata[,sel.vars])
    rf.final <- randomForest(x=rf.data[,2:ncol(rf.data)], y=rf.data[,"y"], ntree=b, 
                             importance=TRUE, norm.votes=TRUE, na.action=na.omit)

  #( rf.balanced <- rfClassBalance(ydata=rf.data[,1], xdata=rf.data[,2:ncol(rf.data)], 
  #                                  ntree=b) )
    						   
  #################		   
  # PREDICT MODEL #
  #################
  xvars <- stack(paste(rpath, paste(rownames(rf.final$importance), rtype, sep="."), sep="/"))
    r <- predict(xvars, rf.final, type="prob", progress="text", index=2)
      r <- resample(r, snapExtent, method="ngb")
      writeRaster(r, filename=paste(fpath, paste(paste(d, "cprob", sep="_"), 
                 rtype, sep="."), sep="/"), overwrite=TRUE)				   
  ###################################################
  save.image( paste(getwd(), "Model.RData", sep="/") ) 
  setwd("..")
} # END OF ALL MODELS LOOP

