###############################
# createSSURGORasters
###############################
#
# An interface for generating SSURGO/gSSURGO rasters.  This project was originally crafted to generate SSURGO rasters 
# for use as input in a tillage likelihood model.  I anticipate eventually including these various functions in a formal package that we use
# to drive that model. In the meantime, this will exist as a loose collection of scripts that may or may not serve some purpose for you.  
# 
# Please report any bugs/questions to the author(s).
# Kyle Taylor (kyle.taylor@pljv.org) [2014]
#

require(RCurl)
require(raster)
require(rgdal)

#
# Quick and Dirty fetch from WSS.  Make this more robust, mm?
#
wssFetch <- function(STATE=NULL, FIPS=NULL){

  # format according to WSS:
  if(is.null(STATE)){
    cat(" -- error: no state specified for WSS download.\n");
    stop();
  }
  # if FIPS is undefined, download the whole state
  if(is.null(FIPS)){
    FIPS <- formatC(seq(1,153, 2), width=3, flag="0")
  }
  
  files <- paste("wss_SSA_",STATE,FIPS,"_soildb_US_2003_[2014-09-15].zip",sep="")
  links <- paste("http://websoilsurvey.sc.egov.usda.gov/DSD/Download/Cache/SSA/wss_SSA_",STATE, FIPS,
                "_soildb_US_2003_[2014-09-15].zip",sep="")
  #
  cat(" -- Downloading County Data:")
  for(i in 1:length(links)){
    cat("[",i,"/",length(links),"]")
    f <- CFILE(files[i], mode="wb");
      curlPerform(url = links[i], writedata = f@ref);
        close(f);
  }; cat("\n")

}

#
# populateMuaggattTable()
# Accepts a path to the root folder of a SSURGO county extraction (Ex: /home/ktaylora/SSURGO/TX/TX001) and parses the SSURGO data contained in the 
# ./soilsmu_a/tabular/muaggatt.txt file for a series of focal variables.  These variables are hard-coded at the moment, but I will include an option to allow the 
# user to specify them (by name) in the future. A data.frame containing the parsed table values and associated MUKEY's is returned to the user.
#
# Author: Kyle Taylor (kyle.taylor@pljv.org) [2014]
#

populateMuaggattTable <- function(path=NULL, targetVars=NULL){
  
  ## local variables and target output

  populatedTable <- data.frame()

  musym          <- character() # musym 
  mukey          <- character() # mukey
  aws0025wta     <- numeric()   # Available Water Storage 0-25 cm
  aws0050wta     <- numeric()   # Available Water Storage 0-50 cm
  aws0100wta     <- numeric()   # Available Water Storage 0-100 cm
  aws0150wta     <- numeric()   # Available Water Storage 0-150 cm
  brockdepmin    <- numeric()   # Bedrock Depth - Minimum
  drclassdcd     <- numeric()   # Drainage Class - Dominant Condition
  slopegraddcp   <- numeric()   # Slope Gradient - Dominant Component
  slopegradwta   <- numeric()   # Slope Gradient - Weighted Average
  wtdepannmin    <- numeric()   # Water Table Depth - Annual - Minimum
  wtdepaprjunmin <- numeric()   # Water Table Depth - April - June - Minimum
  flodfreqdcd    <- numeric()   # Flooding Frequency - Dominant Condition
  flodfreqmax    <- numeric()   # Flooding Frequency - Maximum
  pondfreqprs    <- numeric()   # Ponding Frequency - Presence
  hydgrpdcd      <- numeric()   # Hydrologic Group - Dominant Conditionsp
  iccdcd         <- numeric()   # Irrigated Capability Class - Dominant Condition
  iccdcdpct      <- numeric()   # Irrigated Capability Class  - Dominant Condition Aggregate Percent
  niccdcd        <- numeric()   # Non-Irrigated Capability Class - Dominant Condition
  hydclprs       <- numeric()   # Hydric Classification - Presence

  ## built-in methods
  parseMuaggattLn <- function(x=NULL){
    if(!is.null(x)){
      x<-gsub("[^A-Za-z0-9. :punct:]","",x) # return the meat, leave the potatoes portion of the CL. (verified working with SSURGO 2.2)
    }
    return(x)
  }

  # sanity-check the path provided by the user
  t <- paste(path,'tabular','muaggatt.txt',sep="/")
    cat(" -- parsing:",t,"\n",sep=" ")
    t <- strsplit(readLines(t), split="\\|")

  for(ln in t){ # iterate over each line of the muaggatt file, parsing as we go
    ln <- parseMuaggattLn(ln)
    #cat(" -- ln: ", j, "\n")
    #print(ln)
    if(length(musym <- ln[01]) == 0 || ln[01] == ""){ cat(" -- error: encountered an invalide musym value (this should never happen).\n"); stop(); } 
    if(length(muname <- ln[02]) == 0 || ln[02] == ""){ cat(" -- error: encountered an invalide mukey value (this should never happen).\n"); stop(); }
    if(length(slopegraddcp <- ln[04]) == 0 || ln[04] == ""){ slopegraddcp <- NA } 
    if(length(slopegradwta <- ln[05]) == 0 || ln[05] == ""){ slopegradwta <- NA }
    if(length(brockdepmin <- ln[06]) == 0 || ln[06] == ""){ brockdepmin <- NA } 
    if(length(wtdepannmin  <- ln[07]) == 0 || ln[07] == ""){ wtdepannmin <- NA } 
    if(length(wtdepaprjunmin <- ln[08]) == 0 || ln[08] == ""){ wtdepaprjunmin <- NA } 
    if(length(flodfreqdcd <- ln[09]) == 0 || ln[09] == ""){ 
      flodfreqdcd  <- NA 
    } else {
        if(flodfreqdcd == "None"){ flodfreqdcd <- 1 
        } else if(flodfreqdcd == "Very rare"){ flodfreqdcd <- 2 
        } else if(flodfreqdcd == "Rare"){ flodfreqdcd <- 3 
        } else if(flodfreqdcd == "Occasional"){ flodfreqdcd <- 4 
        } else if(flodfreqdcd == "Common"){ flodfreqdcd <- 5 
        } else if(flodfreqdcd == "Frequent"){ flodfreqdcd <- 6 
        } else if(flodfreqdcd == "Very frequent"){ flodfreqdcd <- 7 
        } else { flodfreqdcd <- 99;
               cat(" -- encountered an unknown flood frequency class parsing muaggatt table.\n"); }
    }
    if(length(flodfreqmax<- ln[10]) == 0 || ln[10] == ""){ flodfreqdmax  <- NA 
    } else {
         if (flodfreqmax == "None"){flodfreqmax <- 1
         } else if(flodfreqmax == 'Very rare'){ flodfreqmax <- 2 
         } else if(flodfreqmax == 'Rare'){ flodfreqmax <- 3 
         } else if(flodfreqmax == 'Occasional'){ flodfreqmax <- 4 
         } else if(flodfreqmax == 'Common'){ flodfreqmax <- 5 
         } else if(flodfreqmax == 'Frequent'){ flodfreqmax <- 6 
         } else if(flodfreqmax == 'Very frequent'){ flodfreqmax <- 7 
         } else if(flodfreqmax == 'Not rated'){ flodfreqmax <- NA 
         } else{ flodfreqmax <- 99;
               cat(" -- encountered an unknown flood frequency max class parsing muaggatt table.\n"); }
    }
    if(length(pondfreqprs<- ln[11]) == 0 || ln[11] == "")  pondfreqprs <- NA 
    if(length(aws0025wta <- ln[12]) == 0 || ln[12] == "")  aws0025wta  <- NA 
    if(length(aws0050wta <- ln[13]) == 0 || ln[13] == "")  aws0050wta  <- NA 
    if(length(aws0100wta <- ln[14]) == 0 || ln[14] == "")  aws0100wta  <- NA 
    if(length(aws0150wta <- ln[15]) == 0 || ln[15] == "")  aws0150wta  <- NA 
    if(length(drclassdcd <- ln[16]) == 0 || ln[16] == ""){ drclassdcd  <- NA 
    } else { 
        if(drclassdcd == "Excessively drained"){ drclassdcd <- 1 
        } else if(drclassdcd == "Somewhat excessively drained"){ drclassdcd <- 2 
        } else if(drclassdcd == "Well drained"){ drclassdcd <- 3 
        } else if(drclassdcd == "Moderately well drained"){ drclassdcd <- 4 
        } else if(drclassdcd == "Somewhat poorly drained"){ drclassdcd <- 5 
        } else if(drclassdcd == "Poorly drained"){ drclassdcd <- 6 
        } else if(drclassdcd == "Very poorly drained"){ drclassdcd <- 7 
        } else if(drclassdcd == "Not rated"){ drclassdcd <- NA 
        } else { drclassdcd <- 99; 
               cat(" -- encountered an unknown dry drainage class parsing muaggatt table.\n"); }
    }
    if(length(drclasswettest <- ln[17]) == 0 || ln[17] == ""){ drclasswettest  <- NA 
    } else {
        if(drclasswettest == "Excessively drained"){ drclasswettest <- 1 
        } else if(drclasswettest == "Somewhat excessively drained"){ drclasswettest <- 2 
        } else if(drclasswettest == "Well drained"){ drclasswettest <- 3 
        } else if(drclasswettest == "Moderately well drained"){ drclasswettest <- 4 
        } else if(drclasswettest == "Somewhat poorly drained"){ drclasswettest <- 5 
        } else if(drclasswettest == "Poorly drained"){ drclasswettest <- 6 
        } else if(drclasswettest == "Very poorly drained"){ drclasswettest <- 7
        } else if(drclasswettest == "Not rated"){ drclasswettest <- NA 
        } else { drclasswettest <- 99; 
               cat(" -- encountered an unknown wet drainage class (",drclasswettest,")parsing muaggatt table.\n", sep=""); }
    }
    if(length(hydgrpdcd <- ln[18]) == 0 || ln[18] == ""){ hydgrpdcd  <- NA 
    } else {
        if(hydgrpdcd == "A" ){ hydgrpdcd <- 1 
        } else if(hydgrpdcd == "B"){ hydgrpdcd <- 2 
        } else if(hydgrpdcd == "C"){ hydgrpdcd <- 3 
        } else if(hydgrpdcd == "D"){ hydgrpdcd <- 4 
        } else if(hydgrpdcd == "AD"){ hydgrpdcd <- 5 
        } else if(hydgrpdcd == "BD"){ hydgrpdcd <- 6 
        } else if(hydgrpdcd == "CD"){ hydgrpdcd <- 7
        } else if(hydgrpdcd == "Not rated"){ hydgrpdcd <- 7  
        } else if(suppressWarnings(!is.na(as.numeric(hydgrpdcd)))){ hydgrpdcd <- hydgrpdcd 
        } else { cat(" -- encountered an unknown hydrologic group (",hydgrpdcd,") parsing muaggatt table.\n",sep=""); 
                 hydgrpdcd <- 99;
        }
    }
    if(length(iccdcd <- ln[19]) == 0 || ln[19] == ""){ iccdcd  <- NA }
    if(length(iccdcdpct <- ln[20]) == 0 || ln[20] == "" ){ iccdcdpct  <- NA }
    if(length(niccdcd <- ln[21]) == 0 || ln[21] == "" ){ niccdcd  <- NA }
    if(length(niccdcdpct <- ln[22]) == 0 || ln[22] == "" ){ niccdcdpct  <- NA }
    if(length(hydclprs <- ln[38]) == 0 || ln[38] == "" ){ hydclprs  <- NA 
    } else {
        # note: these are now numeric 0-10, as of SSURGO v9
        if(hydclprs == "All hydric"){ hydclprs <- 1 
        } else if(hydclprs == "Not hydric"){ hydclprs <- 2 
        } else if(hydclprs == "Partially hydric"){ hydclprs <- 3 
        } else if(hydclprs == "Unknown"){ hydclprs <- 4 
        } else if(suppressWarnings(!is.na(as.numeric(hydclprs)))) { hydclprs <- hydclprs
        } else {
          cat(" -- encountered an unknown hydrologic category (",hydclprs,") parsing muaggatt table.\n",sep="");
        }
    }
  
  # build and sanity check our parsed output and return a data.frame to the user
  row <- try(data.frame(muname=muname, musym=musym, aws0025wta=aws0025wta, aws0050wta=aws0050wta, aws0100wta=aws0100wta,
                        aws0150wta=aws0150wta,brockdepmin=brockdepmin,drclassdcd=drclassdcd,slopegraddcp=slopegraddcp,slopegradwta=slopegradwta,
                        wtdepannmin=wtdepannmin,wtdepaprjunmin=wtdepaprjunmin,flodfreqdcd=flodfreqdcd,flodfreqmax=flodfreqmax,
                        pondfreqprs=pondfreqprs,hydgrpdcd=hydgrpdcd,iccdcd=iccdcd,iccdcdpct=iccdcdpct,niccdcd=niccdcd,hydclprs=hydclprs))
  
  if(nrow(populatedTable)>0){ populatedTable <- rbind(populatedTable,row) } else { populatedTable <- row }
    
  }
    if(sum(is.na(populatedTable))> 0) { cat(" -- warning: there are NA values in the populated muaggatt table.\n") }
      return(populatedTable)
}

#
# populateNccpiTable()
# Accepts a path to a stat NCCPI table (either a csv or xls document).  Returns table as a data.frame to the user.
#
# author : Kyle Taylor (kyle.taylor@pljv.org)
#

populateNccpiTable <- function(path=NULL){
  ## default includes
  require(tools)

  ## local variables and target output
  nccpiTable <- data.frame() # houses our NCCPI
  
  if(is.null(path)){ # did the user fail to provide an NCCPI table?  
    nccpiTable <- ifelse(file.exists("nccpi.csv"), read.csv(nccpi.csv), NA)
    if(is.na(nccpiTable)) { cat(" -- failed to parse NCCPI table at path=\n"); stop()}
  } else {           # default handling of file path information
    if(tools::file_ext(path) == "csv"){ nccpiTable <- read.csv(path) 
    } else if(tools::file_ext(path) == "xls") {
      if(!require(gdata)){ cat("[not found] attempting install.\n"); install.packages(pkgs='gdata',repo='http://cran.us.r-project.org'); require(gdata) }
      nccpiTable <- read.xls(path)
    } else {
      cat(" -- NCCPI path= argument should be a csv or xls file")
      stop()
    }
  }

  return(nccpiTable)
}

#
# ssurgoVectorToRaster()
#
# accepts a SSURGO map unit polygon (containing a MUKEY) and generates a gridded raster indicating values 
# for field(s) passed by the user
# 

ssurgoVectorToRaster <- function(s=NULL,fields=NULL,muaggattTable=NULL,nccpiTable=NULL,res=75){
  ## default includes
  if(require(snow)) beginCluster(n=3)

  ## default functions
  nToMUSYM <- function(id=NULL) return(levels(s$MUSYM)[id]) 

  if(is.null(fields[1])){ # did the user pass fields for us to use?
    fields <- colnames(muaggattTable) # dump all of the ssurgo data we can get our hands on in the muaggat table for this county
      fields <- fields[!grepl(fields,pattern="mu")] # ignore mukey and musym fields 
  }
  
  rasters <- list() # product rasters

  if(is.na(raster::projection(s))){ stop(" -- could not find projection information for the sp data provided.")} # sanity-check our input polygon data

  # generate a template raster surface and prepare for a substitution using the appropriate musym values

  cat(" -- generating template (MUKEY) raster surface from polygons.\n")
  if(grepl(projection(s), pattern="longlat")) s<-spTransform(s, CRS(projection("+init=epsg:2163"))) # fix : use a metric coordinate system to initialize our raster surface
  
  r <- raster(res=res, ext=extent(s), crs=CRS(projection("+init=epsg:2163"))) 
    r <- raster::rasterize(s,r,field="MUSYM", progress='text') # populate raster cells with corresponding MUSYM values 
      r <- projectRaster(r, crs=CRS(projection("+init=epsg:4326")), method='ngb') # transform back into +longlat and the WGS84 datum
  
  cellValueSubs <- unique(r)
    cellValueSubs <- data.frame(values=cellValueSubs,mukeys=as.vector(nToMUSYM(cellValueSubs)))

  # sanity check our data
  if(sum(cellValueSubs$mukeys %in% muaggattTable$musym) != length(cellValueSubs$mukeys)){
     noMusym <- which(!(cellValueSubs$mukeys %in% muaggattTable$musym))
     cat(" -- warning: mukey '", as.vector(cellValueSubs$mukeys)[noMusym], "' for focal county not found in the muaggat table.\n",sep="")
  }

  # add variables from the SSURGO list

  for(f in fields){
      # add a column to our table for field values needed for our substitution
      cellValueSubs[cellValueSubs$mukeys %in% as.vector(muaggattTable$musym),f] <- as.factor(muaggattTable[as.vector(muaggattTable$musym) %in% cellValueSubs$mukeys,f])
      # generate a raster surface consistent with our polygon using the MUKEY for this area
      cat(" -- rasterizing:",f,"\n")
      r_focal <- r;
        r_focal <- raster::subs(x=r_focal,y=cellValueSubs,by='values',which=f)
          r_focal <- reclassify(r_focal,r_focal@data@attributes[[1]], subsWithNA=T) 
            r_focal@data@names <- f

      rasters[[length(rasters)+1]] <- r_focal
  }

  # finally, add rasterized NCCPI productivity ratings 
  
  nccpiTable <- nccpiTable[nccpiTable$Area.Symbol == as.vector(unique(s$AREASYMBOL)),] #subset the nccpi table to the focal FIPS
  cellValueSubs[,"nccpi"] <- nccpiTable[as.vector(nccpiTable$Map.Symbol) %in% cellValueSubs$mukeys,'NCCPI.Rating']
  
  r <-raster::subs(x=r,y=cellValueSubs,by='values',which="nccpi", subsWithNA=T) 
  rasters[[length(rasters)+1]] <- r
    rasters[[length(rasters)]]@data@names <- "nccpi"

  gc() 
  return(rasters)
}

ssurgoUnpackCounty <- function(path=NULL){
  # default includes
  require(utils)
  # check input zip file to see if it's in a format we can understand
  l<-try(unzip(path,list=T))
  if(class(l) == "try-error"){ stop(" -- is the path= argument not a zipfile?")}
    if(!(sum(grepl(l$Name, pattern="tabular")) > 1 && sum(grepl(l$Name, pattern="spatial")) > 1)) # these values are consistent with 
      { stop(" -- inconsistent county-level SSURGO zipfile structure. please contact author (kyle.taylor@pljv.org).") }
  # unpack spatial and tabular data
  spatial <- l$Name[grepl(l$Name, pattern="soilmu_a_")]
  tabular <- l$Name[grepl(l$Name, pattern="tabular")]
  unzip(zipfile=path, files=c(spatial,tabular))
}

#
# ssurgoUnpackState()
# Unpack a state SSURGO zip, as downloaded from the NRCS Geospatial Data Gateway
# http://datagateway.nrcs.usda.gov/GDGOrder.aspx
#

ssurgoUnpackState <- function(path=NULL){
  # default includes
  require(utils)
  # check input zip file to see if it's in a format we can understand
  l<-try(unzip(path,list=T))
  if(class(l) == "try-error"){ stop(" -- is the path= argument not a zipfile?")}
    if( sum(grepl(l$Name, pattern="wss_SSA_"))/sum(grepl(l$Name, pattern="zip"))  == 1 ) # these values are consistent with 
      { stop(" -- inconsistent state-level SSURGO zipfile structure. please contact author (kyle.taylor@pljv.org).") }
  
  unzip(path)
  return(l$Name[grepl(l$Name, pattern="zip")])
}

#
# gssurgoUnpackStateZip
# Unpacks a gSSURGO zipfile for a state and creates a raster stack which is returned to the user.  Each layer of the stack
# can be processed by the user using gssurgoToRaster()
#

gssurgoUnpackStateZip <- function(s=NULL){
  # default includes
  require(utils) 
  # check input zip file to see if it's in a format we can understand
  l<-try(unzip(s,list=T))
    if(class(l) == "try-error"){ stop(" -- is the s= argument not a zipfile?")}
      if( (sum(grepl(l$Name, pattern="valu_")) + sum(grepl(l$Name, pattern="gssurgo_g"))) == 0 ) # these values are consistent with gSSURGO v
  	    { stop(" -- inconsistent gssurgo zipfile structure. please contact author (kyle.taylor@pljv.org).") }
  # 

}


#
# gssurgoToRaster()
# accepts a gssurgo raster and 
#

gssurgoToRaster <- function(r=NULL,fields=NULL){

}
