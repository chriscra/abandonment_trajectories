# Liang's code for accessing the LUH2 data


##resampling other variables based on the resolution of wc.current
  #Following is the code for resampling (fix resolution)
  #lu.file<-"C:/Users/liangm/2019_Princeton/Pika/Data/Environment_vars/LUH2/Original data(from LUH website)/states.nc"
  #path_lu.current<-"C:/Users/liangm/2019_Princeton/Pika/Data/Environment_vars/LUH2/Current(1960-1990)_5min/"
  #lu.current<-nc_open(lu.file)
  #lu.current_vars<-names(lu.current$var)[1:14]
  #lu.current<-lapply(lu.current_vars,function(x){
  #  dat.stack<-stack(lu.file,varname=x,bands=1111:1141) #read in year 1960-1990
  #  dat.stack.mean<-calc(dat.stack,mean) #Calculate means of all the years
  #  dat.resample<-resample(dat.stack.mean,wc.current[[1]])
  #  writeRaster(dat.resample,paste(path_lu.current,x,".tif",sep=""),formate="GTiff",overwrite=T)
  #})
 
  #Following is the code for resampling (fix resolution)
  #path_Other_env_original<-"C:/Users/liangm/2019_Princeton/Pika/Data/Environment_vars/Other_environmental_vars/Original/"
  #path_Other_env<-"C:/Users/liangm/2019_Princeton/Pika/Data/Environment_vars/Other_environmental_vars/"
  #Other_env_original<-list.files(path_Other_env_original,pattern="*.tif")
  #lapply(Other_env_original,function(x){
  #  dat.file<-paste(path_Other_env_original,x,sep="")
  #  dat<-raster(dat.file)
  #  dat.resample<-resample(dat,wc.current[[1]])
  #  writeRaster(dat.resample,paste(path_Other_env,x,sep=""),formate="GTiff",overwrite=T)
  #})
 
  #Following is the code for resampling
  #lu.future.file<-"C:/Users/liangm/2019_Princeton/Pika/Data/Environment_vars/LUH2/Original data(from LUH website)/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp585-2-1-f_gn_2015-2100.nc"
  #path_lu.future<-"C:/Users/liangm/2019_Princeton/Pika/Data/Environment_vars/LUH2/Future(2061-2080)_5min_LUH2_v2f_RCP8.5_SSP5_REMIND-MAGPIE/"
  #lu.future<-nc_open(lu.future.file)
  #lu.future_vars<-names(lu.future$var)[1:14]
  #lu.future<-lapply(lu.future_vars,function(x){
  #  dat.stack<-stack(lu.future.file,varname=x,bands=47:66) #read in year 2061-2080
  #  dat.stack.mean<-calc(dat.stack,mean) #Calculate means of all the years
  #  dat.resample<-resample(dat.stack.mean,wc.current[[1]])
  #  writeRaster(dat.resample,paste(path_lu.future,x,".tif",sep=""),formate="GTiff",overwrite=T)
  #})   
  
  ###3.Prepare current land-use data
 
  path_lu.current<-"C:/Users/liangm/2019_Princeton/Pika/Data/Environment_vars/LUH2/Current(1960-1990)_5min/"
 
  #LUH2 v2f Release RCP8.5 SSP5 (from REMIND-MAGPIE)
  #Mean values of year 1960-1990
  lu.current<-list.files(path_lu.current,pattern="*.tif")
  lu.current<-lapply(lu.current,function(x) paste(path_lu.current,x,sep=""))
  lu.current<-lapply(lu.current,raster)
 
  #plot out land-use variables (1960-1990)
  #lu.current.stack<-do.call(stack,lu.current)
  #jpeg("C:/Users/liangm/2019_Princeton/Pika/Results/LUH2_1960-1990.jpg",width=960,height=576)
  #plot(lu.current.stack,maxnl=14,nc=4,cex.main=2)
  #dev.off()