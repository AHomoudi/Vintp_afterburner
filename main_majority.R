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
    #CNRM-CERFACS-CNRM-CM5
    #CSIRO-QCCCE-CSIRO-Mk3-6-0
    #ICHEC-EC-EARTH
    #MIROC-MIROC5
    #NCC-NorESM1-M
    #NOAA-GFDL-GFDL-ESM2M
#CMIP5 models that can not be processed with code : 
    #CCCma-CanESM2
    #IPSL-IPSL-CM5A-MR
    #MOHC-HadGEM2-ES
    #MPI-M-MPI-ESM-LR
    #MPI-M-MPI-ESM-MR

#1==============================================================================
library(stringr)
library(affxparser)

#2==============================================================================
#Listing all files in directory
u_nc.files<- paste0("u_wind/",list.files(path = "u_wind/", pattern = ".nc$"))
v_nc.files<- paste0("v_wind/",list.files(path = "v_wind/", pattern = ".nc$"))

q_nc.files<- paste0("q/",list.files(path = "q/", pattern = ".nc$"))
t_nc.files<- paste0("t/",list.files(path = "t/", pattern = ".nc$"))
#2==============================================================================
#Reading surface geopotential files.
z_nc.files<- paste0("surface/",list.files(path = "surface/", pattern = ".nc$"))

#3==============================================================================
# Eastward wind vertical interpolation

source("wind_afterburner/wind_afterburner.R")

required_PLev<-seq(85000,100000,2500) #Pa 

for (x in 1:length(u_nc.files)){
    Wind_afterburner(wind_file = u_nc.files[x],
                     req_press_levels = required_PLev)
    #gc()

}

#4==============================================================================
# Northward wind vertical interpolation 
source("wind_afterburner/wind_afterburner.R")

required_PLev<-seq(85000,100000,2500) #Pa 

for (x in 1:length(v_nc.files)){
    Wind_afterburner(wind_file = v_nc.files[x],
                     req_press_levels = required_PLev)
    gc()
    
}
#5==============================================================================
# Specific humidity vertical interpolation 
required_PLev<-85000   #Pa

source("spec_hum_afterburner/spec_hum_afterburner.R")

for (x in 1:length(q_nc.files)){
    
    Specific_humidity_afterburner(spec_hum_file = q_nc.files[x],
                                  req_press_levels = required_PLev)
    gc()
}

#6==============================================================================
# Temperature vertical interpolation 
required_PLev<-85000   #Pa

source("temp_afterburner/temp_afterburner.R")

for (x in 1:length(t_nc.files)){
    
    file_model<-unlist(str_split(unlist(str_split(t_nc.files[x], "[/]"))[2],"_"))[3]
    
    surface_geopotential <-grep(file_model, z_nc.files, value=TRUE, fixed=TRUE)
    
    Temperature_afterburner(temp_file =t_nc.files[x],
                            surf_gepo = surface_geopotential,
                            req_press_levels = required_PLev)
    gc()
}
#=================================END===========================================

#Test
#u_nc.files<-u_nc.files[c(2,3,4,7,10)]
#v_nc.files<- v_nc.files[c(2,3,4,7,10)]
#t_nc.files<-t_nc.files[c(2,3,4,7,10)]
#q_nc.files<-q_nc.files[c(2,3,4,7,10)]

#z_nc.files<-z_nc.files[c(2,3,4,7,10)]
