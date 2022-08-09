# LL-FP-SAfxns_v.1.3.R
# Author: Shana Terai Snarrenberg
# Dec 2020

library(openxlsx)
library(zoo)

loadData <- function(file, rmvNaNs){
  FPdata <- read.csv(file, header=FALSE,
                     skip = 2,
                     colClasses = c(rep('numeric',3), rep('NULL',4)),
                     col.names = c("time","f465","f405",'d1','d2','d3','d4'))
  if(rmvNaNs){
    FPdata <- na.omit(FPdata)
  }
  return(FPdata)
}

loadData_social <- function(file, rmvNaNs){
  FPdata <- read.csv(file, header=FALSE,
                     skip = 2,
                     colClasses = c(rep('numeric',7), rep('NULL',5)),
                     col.names = c("time","f1","f2","f3","f4","f5","f6",'d1','d2','d3','d4',"d5"))
  if(rmvNaNs){
    FPdata <- na.omit(FPdata)
  }
  return(FPdata)
}

loadData_2f <- function(file, rmvNaNs){
  FPdata <- read.csv(file, header=FALSE,
                     skip = 2,
                     colClasses = c(rep('numeric',5), rep('NULL',4)),
                     col.names = c("time","f465","f405","raw1","fred","raw2",'d1','d2','d3','d4'))
  FPdata <- FPdata[c("time","f465","f405","fred")]
  if(rmvNaNs){
    FPdata <- na.omit(FPdata)
  }
  return(FPdata)
}


loadDatainfo <- function(file){
  return(read.xlsx(file))
}

# Decimate
decimate <- function(FPdata,deciN){
  decilocs <- seq(from=1, to=nrow(FPdata), by=deciN)
  return(FPdata[decilocs,])
}

# Get sampling frequency
getFs <- function(FPdata){
  Fs <- 1/mean(diff(FPdata$time[1:10]))
  return(Fs)
}

# Recording endpoint locations
get_recEnds <- function(FPdata) {
  timedifs <- 1:(dim(FPdata)[1]-1)
  recEnds <- timedifs[diff(FPdata$time) > 10]
  recEnds <- c(recEnds, dim(FPdata)[1])
  return(recEnds)
}

# Trim interval edges
trimEdges <- function(FPdata,dropMS) {
  Fs <- getFs(FPdata)
  dropN <- round(dropMS/1000*Fs)
  cat(dropMS,'ms corresponds to ',dropN,' data point(s) dropped.')
  if (dropN > 0) {
    recEnds <- get_recEnds(FPdata)
    dropLocs <- c(1:dropN, (replicate(length(recEnds),c(-dropN:dropN)) + t(replicate((dropN*2+1),recEnds))))
    cat('\n',1/Fs*dropN*2*length(recEnds),'s total dropped.')
    return(FPdata[!(1:dim(FPdata)[1] %in% dropLocs),])
  }
  else {
    cat('\n0 ms total dropped.')
    return(FPdata)
  }
}

# Simple moving average
simpleRoll <- function(FPdata, win, rec_type, twof) {
  if(rec_type == "int"){
    recEnds <- get_recEnds(FPdata)
    starting = 1
    if(twof){
      for (ending in recEnds) {
        FPdata[starting:ending, 'f465'] <- rollmean(FPdata[starting:ending,'f465'], win, 
                                                    align='center', fill=NA)
        FPdata[starting:ending, 'f405'] <- rollmean(FPdata[starting:ending,'f405'], win, 
                                                    align='center', fill = NA)
        FPdata[starting:ending, 'fred'] <- rollmean(FPdata[starting:ending,'fred'], win, 
                                                    align='center', fill = NA)
        starting = ending + 1
      }
    } else {
      for (ending in recEnds) {
        FPdata[starting:ending, 'f465'] <- rollmean(FPdata[starting:ending,'f465'], win, 
                                                    align='center', fill=NA)
        FPdata[starting:ending, 'f405'] <- rollmean(FPdata[starting:ending,'f405'], win, 
                                                    align='center', fill = NA)
        starting = ending + 1
      }
    }
  } else {
    if(twof){
      FPdata['f465'] <- rollmean(FPdata['f465'], win, align='center', fill=NA)
      FPdata['f405'] <- rollmean(FPdata['f405'], win, align='center', fill=NA)
      FPdata['fred'] <- rollmean(FPdata['fred'], win, align='center', fill=NA)
    } else{
      FPdata['f465'] <- rollmean(FPdata['f465'], win, align='center', fill=NA)
      FPdata['f405'] <- rollmean(FPdata['f405'], win, align='center', fill=NA)
    }
  }
  
  return(FPdata)
}

# Binning
binds <- function(FPdata,bin_val, rec_type, twof) {
  if(rec_type == "int"){
    recEnds <- get_recEnds(FPdata)
    starting = 1
    FP_binds_465 <- vector(mode="list", length=length(recEnds))
    FP_binds_405 <- vector(mode="list", length=length(recEnds))
    binlocs_list <- vector(mode="list", length=length(recEnds))
    if(twof){
      for (e in c(1:length(recEnds))) {
        ending = recEnds[e]
        FP_binds_465[[e]] <- rollapply(FPdata[starting:ending, 'f465'], bin_val, mean, by=bin_val, align='left')
        FP_binds_405[[e]] <- rollapply(FPdata[starting:ending, 'f405'], bin_val, mean, by=bin_val, align='left')
        FP_binds_red[[e]] <- rollapply(FPdata[starting:ending, 'fred'], bin_val, mean, by=bin_val, align='left')
        binlocs_list[[e]] <- seq(from=starting, to=ending, by=bin_val)[1:length(FP_binds_405[[e]])]
        starting = ending + 1
      }
      binlocs = unlist(binlocs_list)
      FPdata[binlocs, 'f465'] <- unlist(FP_binds_465)
      FPdata[binlocs, 'f405'] <- unlist(FP_binds_405)
      FPdata[binlocs, 'fred'] <- unlist(FP_binds_red)
    } else{
      for (e in c(1:length(recEnds))) {
        ending = recEnds[e]
        FP_binds_465[[e]] <- rollapply(FPdata[starting:ending, 'f465'], bin_val, mean, by=bin_val, align='left')
        FP_binds_405[[e]] <- rollapply(FPdata[starting:ending, 'f405'], bin_val, mean, by=bin_val, align='left')
        binlocs_list[[e]] <- seq(from=starting, to=ending, by=bin_val)[1:length(FP_binds_405[[e]])]
        starting = ending + 1
      }
      binlocs = unlist(binlocs_list)
      FPdata[binlocs, 'f465'] <- unlist(FP_binds_465)
      FPdata[binlocs, 'f405'] <- unlist(FP_binds_405)
    }
  } else {
    if(twof){
      FP_binds <- rollapply(FPdata[c('f465','f405','fred')], bin_val, mean, by=bin_val, align='left')
      binlocs <- seq(from=1, to=dim(FP_binds)[1]*bin_val, by=bin_val)
      FPdata[binlocs, c('f465','f405','fred')] <- FP_binds
    } else{
      FP_binds <- rollapply(FPdata[c('f465','f405')], bin_val, mean, by=bin_val, align='left')
      binlocs <- seq(from=1, to=dim(FP_binds)[1]*bin_val, by=bin_val)
      FPdata[binlocs, c('f465','f405')] <- FP_binds
    }
  }
  return(FPdata[binlocs,])
}

# Normalize 0 - 1
norm01 <- function(data) {
  data_noNa <- na.omit(data)
  return ((data - min(data_noNa))/(max(data_noNa) - min(data_noNa)))
}

# Linear regression fit
linregfit <- function(FPdata, rec_type) {
  if(rec_type == "int") {
    FPdata$f405fit <- FPdata$f405
    FPdata$dFF <- FPdata$f465
    recEnds <- get_recEnds(FPdata)
    starting = 1
    for (ending in recEnds) {
      segdata <-  FPdata[starting:ending, c("f405", "f465")]
      p <- lm(f465 ~ f405, segdata)['coefficients'][[1]]
      FPdata$f405fit[starting:ending] <- p[1]*segdata$f405/segdata$f405 + p[2]*segdata$f405
      FPdata$dFF[starting:ending] <- segdata$f465 - segdata$f405 
      starting = ending + 1
    }
  } else {
    p <- lm(f465 ~ f405, FPdata)['coefficients'][[1]]
    FPdata$f405fit <- p[1]*FPdata$f405/FPdata$f405 + p[2]*FPdata$f405
    FPdata$dFF <- FPdata$f465 - FPdata$f405fit
  }
  return(FPdata)
}

# Exponential fit
expfit <- function(FPdata, FPdata_info, filename, twof) {
  
  bl_info <- FPdata_info[FPdata_info$Filename==filename, 'Baseline.(min)']
  bl_info <- bl_info[!is.na(bl_info)]
  bl_end <- which.min(abs(FPdata$time - as.double(bl_info)*60))
  
  if(twof){
    cols <- c("dFF", "f465", "fred")
  } else{
    cols <- c("dFF", "f465")
  }
  
  for(col in cols) {
    y0 <- norm01(FPdata[,col])
    y <- na.omit(y0[1:bl_end])
    y[y==0] <- 1e-10
    x <- c(1:length(y))
    p <- lm(log(y) ~ x)['coefficients'][[1]]
    
    nlfx <- function(x, a, b) { a*exp(b*x) }
    nlsfit <- nls(y ~ nlfx(x, a, b) , start=list(a=exp(p[1]), b=p[2]))
    coef <- coef(nlsfit)
    
    model <- coef[1]*exp(coef[2]*c(1:length(na.omit(y0)))) 
    
    eblcol <- paste(col, ".ebl", sep="")
    blrcol <- paste(col, ".blr", sep="")
    
    FPdata[eblcol] <- y0
    FPdata[eblcol][!is.na(FPdata[eblcol])] <- model
    FPdata[blrcol] <- y0
    FPdata[blrcol] <- FPdata[blrcol] - FPdata[eblcol]
    FPdata[blrcol] <- norm01(FPdata[blrcol])
  }
  
  return(FPdata)
}

# Centering
centering <- function(FPdata, FPdata_info, filename,twof){
  bl_info <- FPdata_info[FPdata_info$Filename==filename, 'Baseline.(min)']
  bl_info <- bl_info[!is.na(bl_info)]
  bl_end <- which.min(abs(FPdata$time - as.double(bl_info)*60))
  
  if(twof){
    cols <- c("f465", "dFF", "f465.blr", "dFF.blr", "fred", "fred.blr")
  } else{
    cols <- c("f465", "dFF", "f465.blr", "dFF.blr")
  }
  
  for(col in cols){
    bl_mean <- mean(na.omit(FPdata[1:bl_end, col]))
    ctrcol <- paste(col, ".c", sep="")
    FPdata[ctrcol] <- (FPdata[col] - bl_mean)/bl_mean
  }
  
  return(FPdata)
}

Run_FPAnalysis <- function(FPdata, FPdata_info, filename,
                           downsamp, ds_val, rec_type, edge_trim, SMA, SMA_val, isos, center, blr, twof){
  if(downsamp == "deci"){
    FPdata <- decimate(FPdata, ds_val)
  } else if(downsamp == "bin"){
    try(FPdata <- binds(FPdata, ds_val, rec_type, twof))
  }
  if(rec_type == "int"){
    FPdata <- trimEdges(FPdata, edge_trim)
  }
  if(SMA){
    FPdata <- simpleRoll(FPdata, SMA_val, rec_type, twof)
  }
  
  FPdata <- linregfit(FPdata, rec_type)
  
  FPdata <- expfit(FPdata, FPdata_info, filename, twof)
  
  FPdata <- centering(FPdata, FPdata_info, filename, twof)
  
  return(FPdata)
}

# Savefilename generator
savefilename <- function(FPdata_run, rec_type, edge_trim, SMA, SMA_val, filename, savetag){
  Fs <- signif(getFs(FPdata_run),2)
  savefilename <- paste(filename, "_", Fs, "Hz", sep="")
  if(rec_type == "int"){
    savefilename <- paste(filename, "_", signif(edge_trim/1000,1), "s-trim", sep="")
  }
  if(SMA){
    savefilename <- paste(savefilename, "_SMA-", SMA_val, savetag, sep="")
  } else{
    savefilename <- paste(savefilename, savetag, sep="")
  }
}
