#
# Post-hoc analysis: Identify counties where saturated thickness was an important explanatory variable and
# determine the degree of importance (in terms of %RMSE explained).
#

require(parallel)
require(raster)
require(rgdal)
require(rfUtilities)

zipHasModelObject <- function(x){
  c <- paste("7za l ",  x,  sep  =  "")
    c <- system(command = c, intern = T)
  return(sum(grepl(c, pattern = "_model.rdata"))>0)
}

unpackModelSpace <- function(x, rootPath = "/tmp", flushBigSpace = T){
  dirName <- strsplit(x, split = "[.]")[[1]][1]
    fullPath <- paste(rootPath, dirName, sep = "/")
      unlink(fullPath, recursive = T, force = T)
      dir.create(fullPath)
  wd <- getwd() # keep track of our CWD so we can move back to it when we are done
  if(!file.copy(from = x, to = fullPath, overwrite = T)){
    stop("failed to copy x =  to /tmp file space")
  } else {
    setwd(fullPath)
      system(command = paste("7za x ", x, " ", paste(dirName, "_model.rdata", sep = ""), sep = ""), ignore.stdout = T)
        setwd(wd)
    if(flushBigSpace){
      # there is a lot of data stuffed in these zips. Get rid of it to free-up some space
      unlink(list.files(fullPath, pattern = "7z$", full.names = T), force = T)
    }
  }
  return(fullPath)
}

getFinalVariablesUsed <- function(x){
  require(randomForest)
  current_model_data <- new.env()
  # parse out our random forest model from the model session in the x =  project directory
  m <- list.files(x, pattern = "_model.rdata", full.names = T)
    load(m, envir = current_model_data)
      m <- get("rf.final", envir = current_model_data)
  # parse a clean list of our list of final variables (I apologize for the magic below)
  predictors <- attributes(m$terms)$predvars
    predictors <- as.vector(na.omit(gsub(predictors, pattern = ", |)|list|as.factor|response|\\(", replacement = NA)))
      return(predictors)
}

getImportance <- function(m, metric = "MDA", cutoff = 0.35, plot = NULL){
  n <- names(importance(m)[, 3])
  if(grepl(tolower(metric), pattern = "mda")){ # Mean Decrease Accuracy
    cutoff <- quantile(importance(m)[, 3], p = cutoff) # convert our cut-off to a quantile value consistent with the distribution of our metric
      cutoff <- cutoff/max(as.vector(importance(m)[, 3]))
    importance <- as.vector(importance(m)[, 3])/max(as.vector(importance(m)[, 3]))
  } else if(grepl(tolower(metric), pattern = "positive-class")){ # Importance for predicting 1
    cutoff <- quantile(importance(m)[, 2], p = cutoff) # convert our cut-off to a quantile value consistent with the distribution of our metric
      cutoff <- cutoff/max(as.vector(importance(m)[, 2]))
    importance <- as.vector(importance(m)[, 2])/max(as.vector(importance(m)[, 2]))
  } else if(grepl(tolower(metric), pattern = "neg-class")){ # Importance for predicting 0
    cutoff <- quantile(importance(m)[, 1], p = cutoff) # convert our cut-off to a quantile value consistent with the distribution of our metric
      cutoff <- cutoff/max(as.vector(importance(m)[, 1]))
    importance <- as.vector(importance(m)[, 1])/max(as.vector(importance(m)[, 1]))
  }
  # plot a distribution of our scaled variable importance values? (useful for estimating cutoffs)
  if(!is.null(plot)){
    dev.new()
    plot(density(importance), main = paste("metric = ", metric, "; cut-off = ", cutoff, sep = ""), cex.main = 0.8, cex = 1.3, xlab = "scaled importance value",  ylab = "density")
    grid();grid();
  }
  # return the most important variables based on metric/cutoff value?
  if(!is.null(cutoff)){
    return(data.frame(var = n[importance> = cutoff], importance = importance[importance> = cutoff]))
  }
  return(data.frame(var = n, importance = importance))
}

applyToModelWorkspace <- function(x = NULL, fun = NULL, m = "rf.initial"){
  require(randomForest)
  current_model_data <- new.env()
  # parse out our random forest model from the model session in the x =  project directory
  M <- list.files(x, pattern = "_model.rdata", full.names = T)
    load(M, envir = current_model_data)
      m <- get(m, envir = current_model_data)

  return(fun(m))
}

loadModelWorkspace <- function(x = NULL){
  current_model_data <- new.env()
  # parse out our random forest model from the model session in the x =  project directory
  M <- list.files(x, pattern = "_model.rdata", full.names = T)
    load(M, envir = current_model_data)
      return(current_model_data)
}


hasSaturatedThickness <- function(x){
  return(sum(grepl(x, pattern = "satThick")) > 0)
}

#
# MAIN
#



z <- list.files(pattern="7z$")
  z <- z[unlist(lapply(z,zipHasModelObject))]

counties <- list()

for(county in z){
  county <- unpackModelSpace(county)
  mod_importance <-applyToModelWorkspace(county,fun=importance,m="rf.final")
  if(hasSaturatedThickness(rownames(mod_importance))){
    i <- mod_importance['satThick',2]/max(mod_importance[,2]) # importance relative to other variables considered in final model
    county_id  <- as.numeric(unlist(strsplit(county,split = "_"))[3])
    counties[[length(counties)+1]] <- data.frame(id=county_id,importance=i)
  }
}

counties <- do.call(rbind,counties)

# clean-up
unlink(list.files("/tmp",pattern="focal_county",full.names=T),recursive=T,force=T)
