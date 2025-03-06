ScatterPLOT3 <- function(dataX, dataY, titleX="dataX", titleY="dataY", varname, show_density=F,
                            log=FALSE, obstitle="OBS", simtitle="SIM", lonname="lon", latname="lat", plotname="Scatter.png", backcol="white",
                            decimal=3, smax, smin, psize=2, palpha=0.1, majorticks=1, minorticks=0.1, mfactor=1,
                            intercept1=rep(0,10),
                            slope1=seq(0.95,0.5,-0.05),
                            label1=paste0("-",seq(5,50,5),"%"),
                            intercept2=rep(0,10), 
                            slope2=seq(1.05,1.50,0.05),
                            label2=paste0("+",seq(5,50,5),"%")
) {
  
  ### LIBRARY
  library("ggplot2")
  source(paste0(path_call,"BVM_Extra.R"))
  
  ### INPUT
  #dataX <- spex_AOD355_all
  #dataY <- atlid_AOD355_all
  
  ### READ OBS
  dataX  <- dataX * mfactor
  
  ### READ EXPERIMENTS
  dataY <- dataY * mfactor
  
  ### REMOVE ALL NA VALUES
  dataX <- dataX[!is.nan(dataX)]
  dataY <- dataY[!is.nan(dataY)]
  
  ### BVM STATISTICS
  BVMtable <- BVM_Extra(obs=dataX, mod=dataY, decimal=decimal)
  
  ### INITIALIZATION
  dpi=300
  
  commonTheme <- list(labs(color="Density",fill="Density", x=titleX, y=titleY), theme_bw(), theme(legend.position=c(0.99,0.05), legend.justification=c(1,0)))
  titleS      <- paste0("ME = ",BVMtable$MeanBias," (MNMB = ",BVMtable$MNMB,")",
                        "\nMAE = ",BVMtable$MAE," (FGE = ",BVMtable$FGE,")",
                        "\nRMSE = ",BVMtable$RMSE,
                        "\nSDdiff = ",BVMtable$diffSD,
                        "\nR = ",BVMtable$R,
                        "\nN = ",length(c(dataX)))
  df          <- data.frame(x=dataX, y=dataY, exp="mydata")
  if (exists("smin")==FALSE | exists("smax")==FALSE) {
    smin <- min(c(df$x,df$y),na.rm=T)
    smax <- max(c(df$x,df$y),na.rm=T)
  }
  
  ### GGPLOT
  p <- ggplot(data=df,aes(x,y)) + 
    geom_point(aes(colour=exp), size=psize) + 
    scale_alpha(range = c(0.2,0.6)) +
    ### COLOURS SELECTION
    scale_colour_manual(values=rgb(0,0,0,palpha)) +
    ### STATISTICS TABLES
    annotate("text", x=smax*0.01, y=smax, label=titleS, vjust=1.3, hjust=0, color="black", family="Century Gothic") +
    # THEME STUFF
    guides(alpha="none") +
    #guides(fill="none") +
    guides(colour="none") +
    commonTheme +
    theme(axis.text=element_text(size=10, family="Century Gothic")) +
    theme(text=element_text(size=10, family="Century Gothic")) +
    theme(legend.title=element_blank())
  
  ### SHOW DENSITY
  if (show_density) { 
    p <- p + 
      stat_density2d(aes(fill=exp, alpha=..level..), geom='polygon', bins=10 ) + 
      scale_fill_manual(values=c(rgb(1,0,0),rgb(0,0,1)))
  }
  
  if (log == "log10") {
    a <- c(1:10 %o% 10^(-5:5))
    b <- a[seq(1,length(a),10)]
    p <- p + 
      annotation_logticks(sides="lb") +
      scale_x_continuous(limits = c(smin,smax), trans = "log10", breaks=b, minor_breaks=a) + 
      scale_y_continuous(limits = c(smin,smax), trans = "log10", breaks=b, minor_breaks=a)
    
    p <- p +
      geom_abline(intercept=0, slope=1, color="grey30", linetype="solid", size=0.6)    +
      geom_abline(intercept=0.5, slope=1, color="grey30", linetype="solid", size=0.1)  +
      geom_abline(intercept=-0.5, slope=1, color="grey30", linetype="solid", size=0.1)
  }
  
  if (log != "log10") {
    a <- seq(-10,10,minorticks)
    b <- seq(-10,10,majorticks)
    p <- p + 
      scale_x_continuous(limits = c(smin,smax) ) +
      scale_y_continuous(limits = c(smin,smax) )
    
    p <- p +
      ### PERCENT LINES AND THEIR LABELS
      geom_abline(intercept=0, slope=1, color="grey30", linetype="solid", size=0.6) +
      geom_abline(intercept=intercept1, slope=slope1, color="grey30", linetype="dotted", size=0.1) +
      geom_abline(intercept=intercept2, slope=slope2, color="grey30", linetype="dotted", size=0.1) +
      annotate("text", x=smax       , y=smax*1.01*slope1, angle=atan(slope1)*180/pi, label=label1, color="grey30", family="Century Gothic") +
      annotate("text", x=smax*1.01/slope2, y=smax       , angle=atan(slope2)*180/pi, label=label2, color="grey30", family="Century Gothic")
  }
  
  p <- p + theme(panel.background = element_rect(fill = backcol))
  p <- p + theme(plot.background = element_rect(fill = backcol))
  
  p
  
}
