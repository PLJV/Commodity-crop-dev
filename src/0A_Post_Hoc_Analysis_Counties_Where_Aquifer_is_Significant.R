#
# Post-hoc analysis: Identify counties where saturated thickness was an important explanatory variable and
# determine the degree of importance (in terms of %RMSE explained).
#

require(parallel)
require(raster)
require(rgdal)

zipHasModelObject <- function(x){
  c <- paste("7za l ",x,sep="")
    c <- system(command=c,intern=T)
  return(sum(grepl(c,pattern="_model.rdata"))>0)
}

unpackModelSpace <- function(x,rootPath="/tmp",flushBigSpace=T){
  dirName <- strsplit(x,split="[.]")[[1]][1]
    fullPath <- paste(rootPath,dirName,sep="/")
      unlink(fullPath,recursive=T,force=T)
      dir.create(fullPath)
  wd <- getwd() # keep track of our CWD so we can move back to it when we are done
  if(!file.copy(from=x,to=fullPath,overwrite=T)){
    stop("failed to copy x= to /tmp file space")
  } else {
    setwd(fullPath)
      system(command=paste("7za x ",x," ",paste(dirName,"_model.rdata",sep=""),sep=""),ignore.stdout=T)
        setwd(wd)
    if(flushBigSpace){
      # there is a lot of data stuffed in these zips. Get rid of it to free-up some space
      unlink(list.files(fullPath,pattern="7z$",full.names=T),force=T)
    }
  }
  return(fullPath)
}

getFinalVariablesUsed <- function(x){
  require(randomForest)
  current_model_data <- new.env()
  # parse out our random forest model from the model session in the x= project directory
  m <- list.files(x,pattern="_model.rdata",full.names=T)
    load(m,envir=current_model_data)
      m <- get("rf.final",envir=current_model_data)
  # parse a clean list of our list of final variables (I apologize for the magic below)
  predictors <- attributes(m$terms)$predvars
    predictors <- as.vector(na.omit(gsub(predictors,pattern=",|)|list|as.factor|response|\\(",replacement=NA)))
      return(predictors)
}

getImportance <- function(m,metric="MDA",cutoff=0.35,plot=NULL){
  n <- names(importance(m)[,3])
  if(grepl(tolower(metric),pattern="mda")){ # Mean Decrease Accuracy
    cutoff <- quantile(importance(m)[,3],p=cutoff) # convert our cut-off to a quantile value consistent with the distribution of our metric
      cutoff <- cutoff/max(as.vector(importance(m)[,3]))
    importance <- as.vector(importance(m)[,3])/max(as.vector(importance(m)[,3]))
  } else if(grepl(tolower(metric),pattern="positive-class")){ # Importance for predicting 1
    cutoff <- quantile(importance(m)[,2],p=cutoff) # convert our cut-off to a quantile value consistent with the distribution of our metric
      cutoff <- cutoff/max(as.vector(importance(m)[,2]))
    importance <- as.vector(importance(m)[,2])/max(as.vector(importance(m)[,2]))
  } else if(grepl(tolower(metric),pattern="neg-class")){ # Importance for predicting 0
    cutoff <- quantile(importance(m)[,1],p=cutoff) # convert our cut-off to a quantile value consistent with the distribution of our metric
      cutoff <- cutoff/max(as.vector(importance(m)[,1]))
    importance <- as.vector(importance(m)[,1])/max(as.vector(importance(m)[,1]))
  }
  # plot a distribution of our scaled variable importance values? (useful for estimating cutoffs)
  if(!is.null(plot)){
    dev.new()
    plot(density(importance),main=paste("metric=",metric,"; cut-off=",cutoff,sep=""),cex.main=0.8,cex=1.3,xlab="scaled importance value", ylab="density")
    grid();grid();
  }
  # return the most important variables based on metric/cutoff value?
  if(!is.null(cutoff)){
    return(data.frame(var=n[importance>=cutoff],importance=importance[importance>=cutoff]))
  }
  return(data.frame(var=n,importance=importance))
}
applyToModelWorkspace <- function(x=NULL,fun=NULL,m="rf.initial"){
  require(randomForest)
  current_model_data <- new.env()
  # parse out our random forest model from the model session in the x= project directory
  M <- list.files(x,pattern="_model.rdata",full.names=T)
    load(M,envir=current_model_data)
      m <- get(m,envir=current_model_data)

  return(fun(m))
}

hasSaturatedThickness <- function(x){
  return(sum(grepl(x,pattern="satThick"))>0)
}

#
# MAIN
#

# find zips that have a valid random forest model object
zips <- list.files(pattern="7z$")
 zips <- zips[unlist(lapply(zips,zipHasModelObject))]
   zips <- lapply(zips,unpackModelSpace)

counties <- unlist(strsplit(unlist(zips),split="/"))
 counties <- counties[grepl(counties,pattern="focal")]

# determine which models have saturated thickness in the final variable selection
m_vars_used <- lapply(zips,getFinalVariablesUsed)
  m_vars_used <- unlist(lapply(m_vars_used,hasSaturatedThickness))

cat("counties where saturated thickness was important:",gsub(x=unlist(zips),pattern="/tmp/",replacement="")[m_vars_used],"\n")

# determine relative importance and attribute to county shapefiles
cl <- makeCluster(detectCores()-1)
m_initial_importance <- list()
m_satThick_importance <- list()
  for(i in 1:length(zips)){ m_initial_importance[[i]] <- try(applyToModelWorkspace(zips[[i]],getImportance)) }
    for(i in 1:length(m_initial_importance)){
      m_satThick_importance[[i]] <- try(m_initial_importance[[i]][grepl(tolower(m_initial_importance[[i]]$var),pattern="satthick"),2]);
      if(class(m_satThick_importance[[i]])=='try-error'){
        m_satThick_importance[[i]] <- NA
      }
    }

setwd("source_counties")
s <- lapply(counties,FUN=readOGR,dsn=".",verbose=F)
o <- s[[1]]
  for(i in 2:length(s)){ o <- raster::union(o,s[[i]]) }
    o$sThickImp <- unlist(m_satThick_importance) # relative importance of saturated thickness as a variable
       o$usedST <- m_vars_used                   # saturated thickness was used in the final model
      writeOGR(o,"/home/ktaylora/Incoming","satThicknessImportance",driver="ESRI Shapefile",overwrite=T)

# clean-up
unlink(list.files("/tmp",pattern="focal_county",full.names=T),recursive=T,force=T)
