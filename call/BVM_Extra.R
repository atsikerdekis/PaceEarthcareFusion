library("hydroGOF")
library("Rmisc")

BVM_Extra <- function(obs, mod, decimal=2, numeric=FALSE, ... ){
  ### Important update for relative statistics 26/10/2023
  ### Remove NAs
  o     <- c(obs)
  f     <- c(mod)
  ID    <- which(!is.na(o) & !is.na(f))
  o     <- o[ID]
  f     <- f[ID]
  
  N     <- length(f)
  diff  <- f - o
  sum   <- f + o
  meano <- mean(o,na.rm=T)
  meanf <- mean(f,na.rm=T)
  sdo   <- sd(o,na.rm=T)
  sdf   <- sd(f,na.rm=T)
  
  OBS_MEAN    <- round(meano,decimal)                                                    # Mean of OBS
  MOD_MEAN    <- round(meanf,decimal)                                                    # Mean of MOD
  MNMB        <- paste(round((sum(diff/sum,na.rm=T)*(2/N))*100,0),"%",sep="")            # MODIFIED NORMALIZED MEAN BIAS
  FGE         <- paste(round((sum(abs(diff/sum),na.rm=T)*(2/N))*100,0),"%",sep="")       # FRACTIONAL GROSS ERROR
  R           <- round(cor(o,f, method="pearson", use="complete.obs"),decimal)           #(sum((f-meanf)*(o-meano),na.rm=T)/N)/(sdo*sdf) # CORRELATION COEFFICIENT
  MaxBias     <- round(max(diff,na.rm=T),decimal)
  MinBias     <- round(min(diff,na.rm=T),decimal)
  UPCIBias    <- round(CI(diff[!is.na(diff)], ci = 0.95)[1],decimal)
  MeanBias    <- round(CI(diff[!is.na(diff)], ci = 0.95)[2],decimal)
  LWCIBias    <- round(CI(diff[!is.na(diff)], ci = 0.95)[3],decimal)
  PercentBias <- paste(round(pbias(sim=c(f), obs=c(o), na.rm=T),0),"%",sep="")
  RMSE        <- round(rmse(sim=c(f), obs=c(o), na.rm=T),decimal)
  NRMSE       <- paste(round(nrmse(sim=c(f), obs=c(o), na.rm=T),0),"%",sep="")
  obsSD       <- round(sd(o,na.rm=T),decimal)
  modSD       <- round(sd(f,na.rm=T),decimal)
  diffSD      <- round(sd(f-o,na.rm=T),decimal)
  #RatioSD     <- round(rSD(sim=c(f), obs=c(o), na.rm=T),decimal)
  MAE         <- round( sum(abs(diff[!is.na(diff)])) / N , decimal)
  
  if (numeric==TRUE) { MNMB        <- round((sum(diff/sum,na.rm=T)*(2/N))*100,0) }      # MODIFIED NORMALIZED MEAN BIAS
  if (numeric==TRUE) { FGE         <- round((sum(abs(diff/sum),na.rm=T)*(2/N))*100,0) } # FRACTIONAL GROSS ERROR
  if (numeric==TRUE) { PercentBias <- round(pbias(sim=c(f), obs=c(o), na.rm=T),0) }
  if (numeric==TRUE) { NRMSE       <- round(nrmse(sim=c(f), obs=c(o), na.rm=T),0) }
  
  return(data.frame(N=N, ObsMean=OBS_MEAN, ModMean=MOD_MEAN, MNMB=MNMB, FGE=FGE, R=R, MAE=MAE,
                    MaxBias=MaxBias, MinBias=MinBias, UPCIBias=UPCIBias, MeanBias=MeanBias, LWCIBias=LWCIBias,
                    PercentBias=PercentBias, RMSE=RMSE, NRMSE=NRMSE, obsSD=obsSD, modSD=modSD, diffSD, row.names=NULL)) # RatioSD=RatioSD
}


