#===============================================================================
#This routine perform CDO operator sellonlatbox  on a list of in directory for
# the area of interest.
#                  Written by :Ahmed Homoudi
#                      September 2020
#===============================================================================

library(stringr)

nc.files<-list.files(pattern = ".nc$")

command<-lapply(nc.files, function(x){
  output<-unlist(str_split(x, "[.]"))[1]
  
  #general format "cdo sellonlatbox,lon1,lon2,lat1,lat2 *extra* infile outfile"
  #*extra* should be added after sellonlatbox and  can be -selname,variable or
  #* -selgridname,gaussian depending on the model.
  
  command<-paste0("cdo sellonlatbox,77.5,322.5,-63.47368,63.47368 ",x," ",output,"_.nc")
  
  
 })

#run CDO commands
lapply(command, function(x){
  system.time(system(x))
})

#remove original files 
lapply(nc.files, function(x){                                                  
  system(paste("rm",x))
})

#################################  END  ########################################