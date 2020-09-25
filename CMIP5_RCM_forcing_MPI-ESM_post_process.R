#===============================================================================
#This routine is written to convert products of CMIP5_RCM_forcing_MPI-ESM from 
#GRIB format to netcdf as well as converting divergence and relative vorticity 
#to wind components u and v. Plus extract temperature, surface air pressure to 
#separate files 
#The process also includes interpolation from spectral to a linear Gaussian grid
#if necessary
#Also the routine can crop data set to the study area of interest.
#                  Written by :Ahmed Homoudi
#                      September 2020
#===============================================================================
library(stringr)
library(stringi)


#Listing all files in directory
d_v_t_nc.files<- paste0("dv_vor_ta/",list.files(path = "dv_vor_ta/", pattern = ".grb"))
hus_ps_nc.files<- paste0("hus_ps/",list.files(path = "hus_ps/", pattern = ".grb"))


  #empty data frame to store meta data 
  DATA<-data.frame(No=seq(1,length(d_v_t_nc.files),1))
  
  for (i in 1: length(d_v_t_nc.files)){
    
   med<-unlist(strsplit(unlist(str_split(d_v_t_nc.files[i],
                                                       "[/]"))[2], "_"))[2]
   #c5 files contain divergence, relative vorticity and temperature 
   #c5_133 files  contain specific humidity and surface air pressure 
   DATA$c5_model[i]<-unlist(str_split(med,
                                      "[-]"))[2]
   #serial number of of the grib file 
   DATA$c5_serial_no[i] <- unlist(strsplit(unlist(strsplit(unlist(str_split(d_v_t_nc.files[i],
                                                 "[/]"))[2], "_"))[6],"[.]"))[1]
   #output pattern 
   DATA$c5_pattern[i]<-unlist(strsplit(unlist(str_split(d_v_t_nc.files[i],
                                                                        "[/]"))[2], "[.]"))[1]
   #files names
   DATA$c5_files[i]<-unlist(str_split(d_v_t_nc.files[i],
                                                        "[/]"))[2]
   
   med<-unlist(strsplit(unlist(str_split(hus_ps_nc.files[i],
                                         "[/]"))[2], "_"))[2]
   DATA$c5_133_model[i]<-unlist(str_split(med,
                                      "[-]"))[2]
   #serial number of of the grib file 
   DATA$c5_133_serial_no[i] <- unlist(strsplit(unlist(strsplit(unlist(str_split(hus_ps_nc.files[i],
                                                                            "[/]"))[2], "_"))[7],"[.]"))[1]
   #output pattern 
   DATA$c5_133_pattern[i]<-unlist(strsplit(unlist(str_split(hus_ps_nc.files[i],
                                                        "[/]"))[2], "[.]"))[1]
   #files names
   DATA$c5_133_files[i]<-unlist(str_split(hus_ps_nc.files[i],
                                      "[/]"))[2]
  }
  
  
  wdir<-getwd()
  setwd(paste0(wdir,"/dv_vor_ta"))

  for (i in 1: length(d_v_t_nc.files)){
    
    if(DATA$c5_model[i] == "LR"){
       #Splits a dataset into pieces, one for each different z-axis.xxx will have 
       #two digitswith the z-axis number 
       # 01 for pressure level 02 for model level 
       
       
       
       command<-paste("cdo splitzaxis",DATA$c5_files[i],paste0(DATA$c5_pattern[i],"_"))
       system(command)
       
       # pressure level gridpoint fields
       command<-paste("cdo -f nc copy -sp2gpl",paste0(DATA$c5_pattern[i],"_","01.grb"),
                      paste0(DATA$c5_pattern[i],"_","01_PL_GP.grb"))
       system(command)
       
       #remove intermediate file created in line 67 
       system(paste0("rm ",paste0(DATA$c5_pattern[i],"_","01.grb")))
       
       #change variables name 
       command<-paste("cdo chname,var130,ta -chname,var138,sd -chname,var155,svo ",
                      paste0(DATA$c5_pattern[i],"_","01_PL_GP.grb"),
                      paste0(DATA$c5_pattern[i],"_","01_PL_GP_changed.grb"))
       system(command)
       
       time_span<- stri_remove_empty(unlist(strsplit(system(paste("cdo showtimestamp",paste0(DATA$c5_pattern[i],"_","01_PL_GP_changed.grb")),
                                                            intern = TRUE),"[ ]")))
       time_span<-str_replace_all(time_span, "[[:punct:]]", " ")%>% str_remove( pattern = "[T]")%>%
          str_replace_all(fixed(" "), "")
       
       #extract temperature data 
       command<- paste("cdo selname,ta",paste0(DATA$c5_pattern[i],"_","01_PL_GP_changed.grb"),
                       paste0("ta_6hrLev_MPI-ESM-LR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc"))
       system(command)
       
       #move temperature file to its directory 
       move_command<-paste0("mv ","ta_6hrLev_MPI-ESM-LR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc ",
                            "t/")
       system(move_command)
       
       #calculation of u and v wind 
       command<- paste("cdo dv2uv -gp2sp -delname,ta",paste0(DATA$c5_pattern[i],"_","01_PL_GP_changed.grb"),
                       paste0(DATA$c5_pattern[i],"_","01_PL_GP_v_u.grb"))
       system(command)
       
       #extract u wind data 
       command<- paste("cdo selname,u",paste0(DATA$c5_pattern[i],"_","01_PL_GP_v_u.grb"),
                       paste0("ua_6hrLev_MPI-ESM-LR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc"))
       system(command)
       
       #move u_wind file to its directory 
       move_command<-paste0("mv ","ua_6hrLev_MPI-ESM-LR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc ",
                            "u_wind/")
       system(move_command)
       
       #extract v wind data 
       command<- paste("cdo selname,v",paste0(DATA$c5_pattern[i],"_","01_PL_GP_v_u.grb"),
                       paste0("va_6hrLev_MPI-ESM-LR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc"))
       system(command)
       
       #move v_wind file to its directory 
       move_command<-paste0("mv ","va_6hrLev_MPI-ESM-LR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc ",
                            "v_wind/")
       system(move_command)
       
       #remove intermediate files 
       
       system(paste("rm",paste0(DATA$c5_pattern[i],"_","01_PL_GP_v_u.grb"),
                    paste0(DATA$c5_pattern[i],"_","01_PL_GP_changed.grb"),
                    paste0(DATA$c5_pattern[i],"_","01_PL_GP.grb"))) 
       print(paste("file number ",i))
       
    }
    if(DATA$c5_model[i] == "MR"){
        #Splits a dataset into pieces, one for each different z-axis.xxx will have 
        #two digitswith the z-axis number 
        # 01 for pressure level 02 for model level 
        
        
        command<-paste("cdo splitzaxis",DATA$c5_files[i],paste0(DATA$c5_pattern[i],"_"))
        system(command)
        
        # pressure level gridpoint fields
        command<-paste("cdo -f nc copy -sp2gpl",paste0(DATA$c5_pattern[i],"_","01.grb"),
                       paste0(DATA$c5_pattern[i],"_","01_PL_GP.grb"))
        system(command)
        
        #remove intermediate file created in line 145
        system(paste0("rm ",paste0(DATA$c5_pattern[i],"_","01.grb")))
        
        
        time_span<- stri_remove_empty(unlist(strsplit(system(paste("cdo showtimestamp",paste0(DATA$c5_pattern[i],"_","01_PL_GP.grb")),
                                                             intern = TRUE),"[ ]")))
        time_span<-str_replace_all(time_span, "[[:punct:]]", " ")%>% str_remove( pattern = "[T]")%>%
           str_replace_all(fixed(" "), "")
        
        #change variables name 
        command<-paste("cdo chname,var130,ta -chname,var138,sd -chname,var155,svo ",
                       paste0(DATA$c5_pattern[i],"_","01_PL_GP.grb"),
                       paste0(DATA$c5_pattern[i],"_","01_PL_GP_changed.grb"))
        system(command)
        
        
        #extract temperature data 
        command<- paste("cdo selname,ta",paste0(DATA$c5_pattern[i],"_","01_PL_GP_changed.grb"),
                        paste0("ta_6hrLev_MPI-ESM-MR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc"))
        system(command)
        
        #move temperature file to its directory 
        move_command<-paste0("mv ","ta_6hrLev_MPI-ESM-MR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc ",
                             "t/")
        system(move_command)
        
        #calculation of u and v wind 
        command<- paste("cdo dv2uv -gp2sp -delname,ta",paste0(DATA$c5_pattern[i],"_","01_PL_GP_changed.grb"),
                        paste0(DATA$c5_pattern[i],"_","01_PL_GP_v_u.grb"))
        system(command)
        
        #extract u wind data 
        command<- paste("cdo selname,u",paste0(DATA$c5_pattern[i],"_","01_PL_GP_v_u.grb"),
                        paste0("ua_6hrLev_MPI-ESM-MR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc"))
        system(command)
        
        #move u_wind file to its directory 
        move_command<-paste0("mv ","ua_6hrLev_MPI-ESM-MR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc ",
                             "u_wind/")
        system(move_command)
        
        #extract v wind data 
        command<- paste("cdo selname,v",paste0(DATA$c5_pattern[i],"_","01_PL_GP_v_u.grb"),
                        paste0("va_6hrLev_MPI-ESM-MR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc"))
        system(command)
        
        #move v_wind file to its directory 
        move_command<-paste0("mv ","va_6hrLev_MPI-ESM-MR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc ",
                             "v_wind/")
        system(move_command)
        
        #remove intermediate files 
        
        system(paste("rm",paste0(DATA$c5_pattern[i],"_","01_PL_GP_v_u.grb"),
                     paste0(DATA$c5_pattern[i],"_","01_PL_GP.grb"),
                     paste0(DATA$c5_pattern[i],"_","01_PL_GP_changed.grb"))) 
     }
     
    }
################################################################################  
  setwd(paste0(wdir,"/hus_ps"))
  
  
  for (i in 1: length(d_v_t_nc.files)){
     if(DATA$c5_model[i] == "LR"){
        #Splits a dataset into pieces, one for each different z-axis.xxx will have 
        #two digitswith the z-axis number 
        # 01 for pressure level 02 for model level 
           
           command<-paste("cdo splitzaxis",DATA$c5_133_files[i],paste0(DATA$c5_133_pattern[i],"_"))
           system(command)
           
           # pressure level gridpoint fields
           command<-paste("cdo -f nc copy ",paste0(DATA$c5_133_pattern[i],"_","01.grb"),
                          paste0(DATA$c5_133_pattern[i],"_","01_PL_GP.grb"))
           system(command)
           
           # model level gridpoint fields
           command<-paste("cdo -f nc copy ",paste0(DATA$c5_133_pattern[i],"_","02.grb"),
                          paste0(DATA$c5_133_pattern[i],"_","02_ML_GP.grb"))
           system(command)
           
           time_span<- stri_remove_empty(unlist(strsplit(system(paste("cdo showtimestamp",paste0(DATA$c5_133_pattern[i],"_","01_PL_GP.grb")),
                                                                intern = TRUE),"[ ]")))
           time_span<-str_replace_all(time_span, "[[:punct:]]", " ")%>% str_remove( pattern = "[T]")%>%
              str_replace_all(fixed(" "), "")
           
           #change variables name 
           command<-paste("cdo chname,var133,q ",
                          paste0(DATA$c5_133_pattern[i],"_","01_PL_GP.grb"),
                          paste0(DATA$c5_133_pattern[i],"_","01_PL_GP_changed.grb"))
           system(command)
           
           #change variables name 
           command<-paste("cdo chname,var134,sp ",
                          paste0(DATA$c5_133_pattern[i],"_","02_ML_GP.grb"),
                          paste0(DATA$c5_133_pattern[i],"_","02_ML_GP_changed.grb"))
           system(command)
           
           #extract specific humidity data 
           command<- paste("cdo selname,q",paste0(DATA$c5_133_pattern[i],"_","01_PL_GP_changed.grb"),
                           paste0("hus_6hrLev_MPI-ESM-LR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc"))
           system(command)
           
           #move  specific humidity file to its directory 
           move_command<-paste0("mv ","hus_6hrLev_MPI-ESM-LR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc ",
                                "q/")
           system(move_command)
           
           #extract surface pressure data 
           command<- paste("cdo selname,sp",paste0(DATA$c5_133_pattern[i],"_","02_ML_GP_changed.grb"),
                           paste0("ps_6hrLev_MPI-ESM-LR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc"))
           system(command)
           
           #move  specific humidity file to its directory 
           move_command<-paste0("mv ","ps_6hrLev_MPI-ESM-LR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc ",
                                "ps/")
           system(move_command)
           
           #remove intermediate files 
           system(paste("rm",paste0(DATA$c5_133_pattern[i],"_","01_PL_GP.grb"),
                        paste0(DATA$c5_133_pattern[i],"_","01.grb"),
                        paste0(DATA$c5_133_pattern[i],"_","01_PL_GP_changed.grb"),
                        paste0(DATA$c5_133_pattern[i],"_","02_ML_GP.grb"),
                        paste0(DATA$c5_133_pattern[i],"_","02.grb"),
                        paste0(DATA$c5_133_pattern[i],"_","02_ML_GP_changed.grb"))) 
           print(paste("file number ",i))
        }
     if(DATA$c5_model[i] == "MR"){
        
        #Splits a dataset into pieces, one for each different z-axis.xxx will have 
        #two digitswith the z-axis number 
        # 01 for pressure level 02 for model level 
        
           command<-paste("cdo splitzaxis",DATA$c5_133_files[i],paste0(DATA$c5_133_pattern[i],"_"))
           system(command)
           
           # pressure level gridpoint fields
           command<-paste("cdo -f nc copy ",paste0(DATA$c5_133_pattern[i],"_","01.grb"),
                          paste0(DATA$c5_133_pattern[i],"_","01_PL_GP.grb"))
           system(command)
           
           # model level gridpoint fields
           command<-paste("cdo -f nc copy ",paste0(DATA$c5_133_pattern[i],"_","02.grb"),
                          paste0(DATA$c5_133_pattern[i],"_","02_ML_GP.grb"))
           system(command)
           
           #change variables name 
           command<-paste("cdo chname,var133,q ",
                          paste0(DATA$c5_133_pattern[i],"_","01_PL_GP.grb"),
                          paste0(DATA$c5_133_pattern[i],"_","01_PL_GP_changed.grb"))
           system(command)
           
           #change variables name 
           command<-paste("cdo chname,var134,sp ",
                          paste0(DATA$c5_133_pattern[i],"_","02_ML_GP.grb"),
                          paste0(DATA$c5_133_pattern[i],"_","02_ML_GP_changed.grb"))
           system(command)
           
           time_span<- stri_remove_empty(unlist(strsplit(system(paste("cdo showtimestamp",paste0(DATA$c5_133_pattern[i],"_","01_PL_GP.grb")),
                                                                intern = TRUE),"[ ]")))
           time_span<-str_replace_all(time_span, "[[:punct:]]", " ")%>% str_remove( pattern = "[T]")%>%
              str_replace_all(fixed(" "), "")
           
           #extract specific humidity data 
           command<- paste("cdo selname,q",paste0(DATA$c5_133_pattern[i],"_","01_PL_GP.grb"),
                           paste0("hus_6hrLev_MPI-ESM-MR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc"))
           system(command)
           
           #move  specific humidity file to its directory 
           move_command<-paste0("mv ","hus_6hrLev_MPI-ESM-MR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc ",
                                "q/")
           system(move_command)
           
           #extract surface pressure data 
           command<- paste("cdo selname,sp",paste0(DATA$c5_133_pattern[i],"_","02_ML_GP.grb"),
                           paste0("ps_6hrLev_MPI-ESM-MR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc"))
           system(command)
           
           #move  specific humidity file to its directory 
           move_command<-paste0("mv ","ps_6hrLev_MPI-ESM-MR_historical_r1i1p1_",time_span[1],"-",time_span[length(time_span)],".nc ",
                                "ps/")
           system(move_command)
           
           #remove intermediate files 
           system(paste("rm",paste0(DATA$c5_133_pattern[i],"_","01_PL_GP.grb"),
                        paste0(DATA$c5_133_pattern[i],"_","01.grb"),
                        paste0(DATA$c5_133_pattern[i],"_","02_ML_GP.grb"),
                        paste0(DATA$c5_133_pattern[i],"_","02.grb"))) 
        }
     }
      
#################################  END  ########################################