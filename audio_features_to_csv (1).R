df=NULL
files <- list.files(path="audio_wav/", pattern="*.wav", full.names=TRUE, recursive=FALSE)
for (i in 1:length(files))
{
  newWobj <- readWave(files[i])
  
  #frequency spectrum analysis
  songspec <- seewave::spec(newWobj, f = newWobj@samp.rate, plot = FALSE)
  analysis <- seewave::specprop(songspec, f = newWobj@samp.rate, flim = c(0, 280/1000), plot = FALSE)
  meanfreq <- analysis$mean/1000
  sd <- analysis$sd/1000
  median <- analysis$median/1000
  Q25 <- analysis$Q25/1000
  Q75 <- analysis$Q75/1000
  IQR <- analysis$IQR/1000
  skew <- analysis$skewness
  kurt <- analysis$kurtosis
  sp.ent <- analysis$sh
  sfm <- analysis$sfm
  mode <- analysis$mode/1000
  centroid <- analysis$cent/1000
  
  #Fundamental frequency parameters
  ff <- seewave::fund(newWobj, f = newWobj@samp.rate, ovlp = 50, threshold = 5, fmax = 280, ylim=c(0, 280/1000), plot = FALSE, wl = 2048)[, 2]
  meanfun<-mean(ff, na.rm = T)
  minfun<-min(ff, na.rm = T)
  maxfun<-max(ff, na.rm = T)
  
  #Dominant frecuency parameters 
  y <- seewave::dfreq(newWobj, f = newWobj@samp.rate, wl = 2048, ylim=c(0, 280/1000), ovlp = 0, plot = F, threshold = 5, bandpass = c(0,22) * 1000)[, 2]
  meandom <- mean(y, na.rm = TRUE)
  mindom <- min(y, na.rm = TRUE)
  maxdom <- max(y, na.rm = TRUE)
  dfrange <- (maxdom - mindom)
  changes <- vector()
  for(j in which(!is.na(y)))
  {
    change <- abs(y[j] - y[j + 1])
    changes <- append(changes, change)
  }
  
  #modulation index calculation
  if(mindom==maxdom) modindx<-0 else modindx <- mean(changes, na.rm = T)/dfrange
  
  df = rbind(df, data.frame(meanfreq, sd, median, Q25, Q75, IQR, skew, kurt, sp.ent, sfm, mode, centroid, meanfun, minfun, maxfun, meandom, mindom, maxdom, dfrange, modindx))
}
write.csv(df, file ="voice_wav_to_csv.csv", row.names = FALSE)
