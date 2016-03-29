#
# Post-hoc analysis: Identify counties where saturated thickness was an important explanatory variable and
# determine the degree of importance (in terms of %RMSE explained).
#

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
  predictors <- as.vector(na.omit(gsub(predictors,pattern=",|)|list|as.factor|response|\\(",replacement=NA)))
  return(predictors)
}

hasSaturatedThickness <- function(x){
  return(sum(grepl(x,pattern="satThick"))>0)
}

# find zips that have a valid random forest model object
zips <- list.files(pattern="7z$")
 zips <- zips[unlist(lapply(zips,zipHasModelObject))]
   zips <- lapply(zips,unpackModelSpace)

# determine which models have saturated thickness in the final variable selection
m_vars_used <- lapply(zips,getFinalVariablesUsed)
  m_vars_used <- unlist(lapply(m_vars_used,hasSaturatedThickness))

cat("counties where saturated thickness was important:",gsub(x=unlist(zips),pattern="/tmp/",replacement="")[m_vars_used],"\n")
# clean-up
unlink(list.files("/tmp",pattern="focal_county",full.names=T),recursive=T,force=T)
