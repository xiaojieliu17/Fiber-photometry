# LL-FP-SAplot_v.1.3.R
# Author: Shana Terai Snarrenberg
# November 2020

library(ggplot2)
library(RColorBrewer)

plotFPdata <- function(FPdata, FPdata_info, filename, plot_type, stim) {
  if(plot_type == "raw"){
    numvals <- dim(FPdata)[1]
    data2plot <- data.frame(signal=c(rep('f465',numvals), rep('f405',numvals)),
                            xvals=rep(c(1:numvals),2),
                            yvals=c(FPdata$f465-mean(na.omit(FPdata$f465)),
                                    FPdata$f405-mean(na.omit(FPdata$f405))))
    p <- ggplot(data2plot, aes(x=xvals, y=yvals, group=signal)) + 
      geom_path(aes(color=signal), na.rm=T) + 
      scale_color_manual(values=brewer.pal(n=6,name = 'Paired')[5:6]) +
      labs(title=paste(filename, "- Raw Data"), x='Time (s)', y='')
  } else if(plot_type == "fit"){
    numvals <- dim(FPdata)[1]
    data2plot <- data.frame(signal=c(rep('f465',numvals), rep('f405fit',numvals)),
                            xvals=rep(c(1:numvals),2),
                            yvals=c(FPdata$f465,FPdata$f405fit))
    p <- ggplot(data2plot, aes(x=xvals, y=yvals, group=signal)) + 
      geom_path(aes(color=signal), na.rm=T) + 
      scale_color_manual(values=brewer.pal(n=4,name = 'Paired')[1:2]) +
      labs(title=paste(filename, "- Linear Fit"), x='Time (s)', y='')
  } else{
    # colors = list("dFF.c"=4, "dFF.blr.c"=8)
    numvals <- dim(FPdata)[1]
    data2plot <- data.frame(xvals=rep(c(1:numvals)), yvals=FPdata[,plot_type])
    p <- ggplot(data2plot, aes(x=xvals, y=yvals)) + 
      geom_path(color=brewer.pal(n=4,name = 'Paired')[4], na.rm=T) + 
      labs(title=paste(filename,"-",plot_type), x='Time (s)', y='')
  }
  
  if(stim){
    stim_info <-FPdata_info[FPdata_info$Filename==filename,c('Baseline.(min)','Stimulus.type')]
    stim_info <- stim_info[!is.na(stim_info)]
    if(stim_info[2] != "none" && stim_info[2] != "None"){
      stim_time <- which.min(abs(FPdata$time - as.double(stim_info[1])*60))
      if(plot_type == "raw"){
        stim_label_y <- max(na.omit(FPdata$f465) - mean(na.omit(FPdata$f465))) - 
          diff(range(na.omit(FPdata$f465) - mean(na.omit(FPdata$f465))))/10
      } else if(plot_type == "fit"){
        stim_label_y <- max(na.omit(FPdata['f465'])) - diff(range(na.omit(FPdata['f465'])))/10
      } else{
        stim_label_y <- max(na.omit(FPdata[plot_type])) - diff(range(na.omit(FPdata[plot_type])))/10
      }
      p <- p + 
        geom_vline(xintercept = stim_time, color = 'gray') +
        geom_text(label = stim_info[2], x = stim_time-50, y = stim_label_y, size = 3, color = 'gray')
    }
  }
  recEnds <- get_recEnds(FPdata)
  p <- p +
    scale_x_continuous(breaks=recEnds[c(TRUE, FALSE)], 
                       labels=round(FPdata$time[recEnds[c(TRUE, FALSE)]], digits=0)) +
    theme(axis.text.x = element_text(angle = -45))
  return(p)
}