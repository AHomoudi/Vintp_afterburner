#A function that will receive temperature + surface air pressure + surface 
#geopotential nc files +  required 
#pressure levels then  write the interpolated values to a netcdf file 

Temperature_afterburner<-function(temp_file,pressure_file,surf_gepo,req_press_levels){
  
  require(ff)
  require(ncdf4)
  require(stringi)
  #1============================================================================ 
  
  blabla<- unlist(strsplit(unlist(strsplit(temp_file,"[/]"))[2],"_"))
  
  #Read temperature data out of netCDF files 
  ncin<-nc_open(temp_file)
  
  DIM<-ncin[["var"]][[blabla[1]]][["varsize"]]
  
  temp_data<-ff(ncvar_get(ncin,blabla[1]),
                dim = DIM,
                dimnames = list(longitude= ncin[["dim"]][["lon"]][["vals"]],
                                latitude= ncin[["dim"]][["lat"]][["vals"]],
                                lev = ncin[["dim"]][["lev"]][["vals"]],
                                TIME =ncin[["dim"]][["time"]][["vals"]]))
  dim(temp_data)
  
 
  # Read extra data 
  p0<-1.0
  a<-ncvar_get(ncin,"hyam")
  b<-ncvar_get(ncin,"hybm")
  
  a_bnds<-ncvar_get(ncin,"hyai")
  #a_bnds<-c(med[1,],med[2,dim(med)[2]])
  
  b_bnds<-ncvar_get(ncin,"hybi")
  #b_bnds<-c(med[1,],med[2,dim(med)[2]])
  
  #rm(med)
  
  longitude <- ncin[["dim"]][["lon"]][["vals"]]
  latitude<- ncin[["dim"]][["lat"]][["vals"]]
  lev <- ncin[["dim"]][["lev"]][["vals"]]
  TIME <- ncin[["dim"]][["time"]][["vals"]]
  

  #2============================================================================ 
  
  #Read surface air pressure 
  
  ncin<-nc_open(pressure_file)
  
  DIM<-ncin[["var"]][["sp"]][["varsize"]]
  
  surface_pressure<-ff(ncvar_get(ncin,"sp"),
                       dim = DIM,
                       dimnames = list(longitude= ncin[["dim"]][["lon"]][["vals"]],
                                       latitude= ncin[["dim"]][["lat"]][["vals"]],
                                       TIME =ncin[["dim"]][["time"]][["vals"]]))
  
  dim(surface_pressure)
  #3============================================================================ 
  #read Geopotential data 
  ncin<-nc_open(surf_gepo)
  
  med <- ncvar_get(ncin,"orog")
  
  H<-(med * 6356.766e03)*9.80665/(med + 6356.766e03)
  
  surf_gepo_array<-ff(H,
                      dim = dim(ncvar_get(ncin,"orog")),
                      dimnames = list(longitude= ncin[["dim"]][["lon"]][["vals"]],
                                      latitude= ncin[["dim"]][["lat"]][["vals"]]))
  rm(ncin)
  #4============================================================================
  
  #load pressure on model level calculator 
  dyn.load("wind_afterburner/press_calc.so")
  #check
  is.loaded("press_calc")
  
  pressure<-ff(array(0.00),dim =dim(temp_data))
  
  DIM<-dim(temp_data)
  
  result<- array(.Fortran("press_calc",
                          m=as.integer(DIM[1]),
                          n=as.integer(DIM[2]),
                          o=as.integer(DIM[3]),
                          p=as.integer(DIM[4]),
                          ps=as.numeric(surface_pressure[]),
                          p0=as.numeric(p0),
                          a=as.numeric(a),
                          b=as.numeric(b),
                          press=as.numeric(pressure[]))$press,
                 dim =DIM)
  
  pressure<-ff(result,dim = dim(temp_data),dimnames = dimnames(temp_data))
  
  rm(result)
  #5============================================================================
  
  #Pressure on half levels 
  
  #load pressure on model level calculator 
  dyn.load("wind_afterburner/press_calc.so")
  #check
  is.loaded("press_calc")
  
  DIM<-dim(temp_data)
  
  output_DIM<-c(DIM[1],DIM[2],length(b_bnds),DIM[4])
  
  pressure_hlf<-ff(array(0.00),dim =output_DIM)
  
  dim(pressure_hlf)
  
  result<- array(.Fortran("press_calc",
                          m=as.integer(DIM[1]),
                          n=as.integer(DIM[2]),
                          o=as.integer(length(b_bnds)),
                          p=as.integer(DIM[4]),
                          ps=as.numeric(surface_pressure[]),
                          p0=as.numeric(p0),
                          a=as.numeric(a),
                          b=as.numeric(b),
                          press=as.numeric(pressure_hlf[]))$press,
                 dim =output_DIM)
  
  DIMNAMES<-dimnames(temp_data)
  
  DIMNAMES[["lev"]]<-b_bnds
  
  pressure_hlf<-ff(result,dim = output_DIM,
                   dimnames = DIMNAMES)
  
  rm(result)
  
  
  #6============================================================================
  #load vertical interpolate subroutine for temperature 
  dyn.load("temp_afterburner/vintp2p_afterburner_ta.so")
  #check
  is.loaded("ta_vertical_interpolation")
  
  
  DIMNAMES<-dimnames(temp_data)
  
  DIMNAMES[["lev"]]<-req_press_levels
  
  DIM<-dim(temp_data)
  
  output_DIM<-c(DIM[1],DIM[2],length(req_press_levels),DIM[4])
  
  output_array<-ff(array(0.00,dim =output_DIM),dim =output_DIM)
  
  result<- array(.Fortran("ta_vertical_interpolation",
                          m=as.integer(DIM[1]),
                          n=as.integer(DIM[2]),
                          o=as.integer(DIM[3]),
                          p=as.integer(DIM[4]),
                          req = as.integer(length(req_press_levels)),
                          pres=as.numeric(req_press_levels),
                          geo= as.numeric(surf_gepo_array[]),
                          pressure_full_level=as.numeric(pressure[]),
                          surface_pressure=as.numeric(surface_pressure[]),
                          ta_on_model_level=as.numeric(temp_data[]),
                          ta_on_press_level=as.numeric(output_array[]))$ta_on_press_level,
                 dim =output_DIM)
  
  Temperature<-ff(result, dim = output_DIM,dimnames = DIMNAMES)
  
  rm(result)
  #7============================================================================
  # Writing netcdf file of the interpolated values 
  ncin<-nc_open(temp_file)
  
  dimLON  <- ncdim_def('lon', units=ncin[["dim"]][["lon"]][["units"]],
                       longname='longitude',
                       vals=longitude)
  
  dimLAT  <- ncdim_def('lat', units=ncin[["dim"]][["lat"]][["units"]],
                       longname='latitude',
                       vals=latitude)
  dimPLEV  <- ncdim_def('lev', units="Pa",
                        longname='pressure level',
                        vals=req_press_levels)
  
  dimTime <- ncdim_def('time', units=ncin[["dim"]][["time"]][["units"]],
                       longname='time', 
                       calendar=ncin[["dim"]][["time"]][["calendar"]],
                       vals=TIME,unlim=TRUE)
  # define variables
  fillvalue <- 1e32
  
  dlname <- "Air Temperature"
  
  var_def <- ncvar_def(name = blabla[1],
                       units = "K",
                       list(dimLON,dimLAT,dimPLEV,dimTime),
                       fillvalue,dlname,prec="double")
  
  #netcdf file name 
  time_span<- stri_remove_empty(unlist(strsplit(system(paste("cdo showtimestamp",temp_file),
                                                       intern = TRUE),"[ ]")))
  
  time_span<-str_replace_all(time_span, "[[:punct:]]", " ")%>% str_remove( pattern = "[T]")%>%
    str_replace_all(fixed(" "), "")
  
  
  ncname<-paste0(blabla[1],"_6hrPlev_",blabla[3],"_",blabla[4],
                 "_",blabla[5],"_",
                 time_span[1],"-",time_span[length(time_span)],"_.nc")
  #netCDF file location 
  
  ncpath <- paste0(blabla[1],"/")
  
  ncfname <- paste0(ncpath, ncname)
  
  #create netCDF file and put arrays
  ncoutput<-nc_create(ncfname,
                      list(var_def),
                      force_v4=TRUE,verbose = TRUE) 
  #put variable 
  
  ncvar_put(ncoutput,var_def,Temperature[])
  
  #put additional attributes into dimension and data variables
  ncatt_put(ncoutput,"lon","axis","X") 
  ncatt_put(ncoutput,"lat","axis","Y")
  ncatt_put(ncoutput,"time","axis","T")
  
  nc_close(ncoutput)
  
  # add global attributes
  #ncatt_put(ncoutput,0,"title",title$value)
  #ncatt_put(ncoutput,0,"institution",institution$value)
  #ncatt_put(ncoutput,0,"source",datasource$value)
  #ncatt_put(ncoutput,0,"references",references$value)
  #history <- paste("P.J. Bartlein", date(), sep=", ")
  #ncatt_put(ncoutput,0,"history",history)
  #ncatt_put(ncoutput,0,"Conventions",Conventions$value)
  
  
  
}
#Test

#temp_file <- t_nc.files[1]
#req_press_levels<- required_PLev
#surf_gepo<-surface_geopotential
#pressure_file <- ps_nc.files[1]
