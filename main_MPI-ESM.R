#===============================================================================
#This code is part of Master thesis work, it is written to vertically interpolate
# CMIP5 dataset on model level (hybrid sigma level) to pressure level.

#After : 
#Trenberth, K. E., Berry, J. C., & Buja, L. E. (1993). Vertical Interpolation 
#and Truncation of Model-Coordinate Data. NCAR Tech. Note TN-396+STR, 54 pp.

#ECMWF. (2014). IFS DOCUMENTATION – Cy40R1, Operational implementation 22 Novem-
#ber 2013, PART VII : Wave Model. ECMWF IFS Documentation, November 2013, 1–79.


                    #Author: Ahmed.A.B.Homoudi
                      #Date:October 2020
#CMIP5 models that can be processed with code : 
  #MPI-M-MPI-ESM-LR
  #MPI-M-MPI-ESM-MR
#CMIP5 models that can not be processed with code : 
  #CCCma-CanESM2
  #IPSL-IPSL-CM5A-MR
  #MOHC-HadGEM2-ES
  #CNRM-CERFACS-CNRM-CM5
  #CSIRO-QCCCE-CSIRO-Mk3-6-0
  #ICHEC-EC-EARTH
  #MIROC-MIROC5
  #NCC-NorESM1-M
  #NOAA-GFDL-GFDL-ESM2M


#1==============================================================================
library(stringr)

#2==============================================================================
#Listing all files in directory
u_nc.files<- paste0("u_wind/",list.files(path = "u_wind/", pattern = ".nc$"))
v_nc.files<- paste0("v_wind/",list.files(path = "v_wind/", pattern = ".nc$"))

q_nc.files<- paste0("q/",list.files(path = "q/", pattern = ".nc$"))
t_nc.files<- paste0("t/",list.files(path = "t/", pattern = ".nc$"))
#2==============================================================================
#Reading surface geopotential files.
z_nc.files<- paste0("surface/",list.files(path = "surface/", pattern = ".nc$"))
ps_nc.files <- paste0("sp/",list.files(path = "sp/", pattern = ".nc$"))

#3==============================================================================
# Eastward wind vertical interpolation

source("wind_afterburner/wind_afterburner_MPI-ESM.R")

required_PLev<-seq(85000,100000,2500) #Pa 

for (x in 1:length(u_nc.files)){
  Wind_afterburner(wind_file = u_nc.files[x],
                   pressure_file = ps_nc.files[x],
                   req_press_levels = required_PLev)
  
}

#4==============================================================================
# Northward wind vertical interpolation 
source("wind_afterburner/wind_afterburner_MPI-ESM.R")

required_PLev<-seq(85000,100000,2500) #Pa 

for (x in 1:length(v_nc.files)){
  Wind_afterburner(wind_file = v_nc.files[x],
                   pressure_file = ps_nc.files[x],
                   req_press_levels = required_PLev)
  
}
#5==============================================================================
# Specific humidity and temperature vertical interpolation 

required_PLev<-85000   #Pa

source("spec_hum_afterburner/spec_hum_afterburner_MPI-ESM.R")

for (x in 1:length(q_nc.files)){
  
  Specific_humidity_afterburner(spec_hum_file = q_nc.files[x],
                                pressure_file = ps_nc.files[x],
                                req_press_levels = required_PLev)
}

#6==============================================================================
# Temperature vertical interpolation

required_PLev<-85000   #Pa

source("temp_afterburner/temp_afterburner_MPI-ESM.R")

for (x in 1:length(t_nc.files)){
  
  file_model<-unlist(str_split(unlist(str_split(q_nc.files[x], "[/]"))[2],"_"))[3]
  
  surface_geopotential <-grep(file_model, z_nc.files, value=TRUE, fixed=TRUE)
  
  Temperature_afterburner(temp_file =t_nc.files[x],
                          pressure_file = ps_nc.files[x],
                          surf_gepo = surface_geopotential,
                          req_press_levels = required_PLev)
}
#=================================END===========================================
#Test

#file_model<-"MPI-ESM-"
#u_nc.files<- grep(file_model, u_nc.files, value=TRUE, fixed=TRUE)
#v_nc.files<- grep(file_model, v_nc.files, value=TRUE, fixed=TRUE)

#q_nc.files<- grep(file_model, q_nc.files, value=TRUE, fixed=TRUE)
#t_nc.files<- grep(file_model, t_nc.files, value=TRUE, fixed=TRUE)
#z_nc.files<- grep(file_model, z_nc.files, value=TRUE, fixed=TRUE)
#ps_nc.files<-grep(file_model, ps_nc.files, value=TRUE, fixed=TRUE)

