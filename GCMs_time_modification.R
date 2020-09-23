#===============================================================================
#This routine is written to modify GCMs output on 6 hrs 
#since some files has step 00:00 of the next month  at the end of the file.
#Also the routine can crop GCMs output to the study area of interest.
#                  Written by :Ahmed Homoudi
#                      September 2020
#===============================================================================
library(stringr)

#list all the file in the directory 
nc.files<-list.files(pattern = ".nc$")
#input_files <- paste(nc.files,collapse="")

#the accpected files pattern 
output_pattern<-unlist(str_split(nc.files[2], "r1i1p1"))[1]

#name of the output of mergeing all files 
output_file<-paste0(output_pattern,"r1i1p1_all.nc")

#merge all the files in the directory
system(paste("cdo mergetime *.nc",output_file))                                

#split merged file by year 
system(paste0("cdo splityear ",output_file," ",output_pattern,"r1i1p1_"))      

#remove original files 
lapply(nc.files, function(x){                                                  
 system(paste("rm",x))
})

#remove big/merged file
system(paste("rm",output_file))                                                

#list the new files 
nc.files<-list.files(pattern = ".nc$")                                          

#rename files
lapply(nc.files, function(x){                                                  
  output_time<-unlist(str_split(unlist(str_split(x, "_"))[6], "[.]"))[1]
  
  new_time<-paste0(output_time,"010100-",output_time,"12311800")
  
  system(paste0("mv ",x," ",output_pattern,"r1i1p1_",new_time,".nc"))
  
  #output<-paste0(output_pattern,"r1i1p1_",new_time,".nc")
  
})

#list the new files 
nc.files<-list.files(pattern = ".nc$")                                          

lapply(nc.files, function(x){
  #crop data files 
  output<-unlist(str_split(x, "[.]"))[1]                                       
  
  #general format "cdo sellonlatbox,lon1,lon2,lat1,lat2 *extra* infile outfile"
  #*extra* can be -selname,variable or -selgrid,gaussian
  #*
  command<-paste0("cdo sellonlatbox,77.8125,323.4375,-62.5,62.5 -selname,ua ",x," ",output,"_.nc")
  
  system.time(system(command))
})

#remove original files 
lapply(nc.files, function(x){                                                   
  system(paste("rm",x))
})

#################################  END  ########################################

