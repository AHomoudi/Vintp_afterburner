#A function that will receive wind nc files + surface air pressure +  required 
#pressure levels then  write the interpolated values to a netcdf file 

Wind_afterburner<-function(wind_file,pressure_file,req_press_levels){
  
  require(ff)
  require(ncdf4)
  require(stringi)
  #1============================================================================  
  blabla <- unlist(strsplit(unlist(strsplit(wind_file,"[/]"))[2],"_"))
  
  variable_infile <-str_split_fixed(blabla[1],pattern = "", n = 2)
  
  ncin<-nc_open(wind_file)
  
  DIM<-ncin[["var"]][[variable_infile[1]]][["varsize"]]
  
  wind_data<-ff(ncvar_get(ncin,variable_infile[1]),
                dim = DIM,
                dimnames = list(longitude= ncin[["dim"]][["lon"]][["vals"]],
                                latitude= ncin[["dim"]][["lat"]][["vals"]],
                                lev = ncin[["dim"]][["lev"]][["vals"]],
                                TIME =ncin[["dim"]][["time"]][["vals"]]))
  
  
  
  dim(wind_data)
  
  p0<-1.0
  a<-ncvar_get(ncin,"hyam")
  b<-ncvar_get(ncin,"hybm")
  
  longitude <- ncin[["dim"]][["lon"]][["vals"]]
  latitude<- ncin[["dim"]][["lat"]][["vals"]]
  lev <- ncin[["dim"]][["lev"]][["vals"]]
  TIME =ncin[["dim"]][["time"]][["vals"]]
  
  
  #2============================================================================ 
  ncin<-nc_open(pressure_file)
  
  DIM<-ncin[["var"]][["sp"]][["varsize"]]
  
  surface_pressure<-ff(ncvar_get(ncin,"sp"),
                       dim = DIM,
                       dimnames = list(longitude= ncin[["dim"]][["lon"]][["vals"]],
                                       latitude= ncin[["dim"]][["lat"]][["vals"]],
                                       TIME =ncin[["dim"]][["time"]][["vals"]]))
  
  dim(surface_pressure)
  #3============================================================================ 
  #load pressure on model calculator 
  dyn.load("wind_afterburner/press_calc.so")
  #check
  is.loaded("press_calc")
  
  pressure<-ff(array(0.00),dim =dim(wind_data))
  
  DIM <- dim(wind_data)
  
  result<- array(.Fortran("press_calc",ps=as.numeric(surface_pressure[]),
                          p0=as.numeric(p0),
                          a=as.numeric(a),
                          b=as.numeric(b),
                          m=as.integer(DIM[1]),
                          n=as.integer(DIM[2]),
                          o=as.integer(DIM[3]),
                          p=as.integer(DIM[4]),
                          press=as.numeric(pressure[]))$press,
                 dim =DIM)
  
  pressure<-ff(result,dim = dim(wind_data),dimnames = dimnames(wind_data))
  
  rm(result)
  #4============================================================================ 
  #load vertical interpolate subroutine 
  dyn.load("wind_afterburner/vintp2p_afterburner_wind_MPI-ESM.so")
  #check
  is.loaded("wind_vertical_interpolation")
  
  DIM<-dim(wind_data)
  
  output_DIM<-c(DIM[1],DIM[2],length(req_press_levels),DIM[4])
  
  #control<-ff(array(0.00,dim =output_DIM),dim =output_DIM)
  
  output_array<-ff(array(0.00,dim =output_DIM),dim =output_DIM)
  
  result<- array(.Fortran("wind_vertical_interpolation",
                          wind_on_model_level=as.numeric(wind_data[]),
                          pres=as.numeric(req_press_levels),
                          pressure_full_level=as.numeric(pressure[]),
                          m=as.integer(DIM[1]),
                          n=as.integer(DIM[2]),
                          o=as.integer(DIM[3]),
                          p=as.integer(DIM[4]),
                          req = as.integer(length(req_press_levels)),
                          wind_on_press_level=as.numeric(output_array[]))$wind_on_press_level,
                 dim =output_DIM)
  
  DIMNAMES<-dimnames(wind_data)
  
  DIMNAMES[["lev"]]<-req_press_levels
  
  interpolated_wind<- ff(result, dim = output_DIM,dimnames = DIMNAMES)
  
  rm(result)
  #5============================================================================ 
  # Writing netcdf file of the interpolated values 
  
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
  
  ncin<-nc_open(wind_file)
  
  if (variable_infile[1]=="u"){
    dlname <-"Eastward Wind"
    var_def <- ncvar_def(name = "ua",
                         units = ncin[["var"]][[variable_infile[1]]][["units"]],
                         list(dimLON,dimLAT,dimPLEV,dimTime),
                         fillvalue,dlname,prec="double")
  } 
  if (variable_infile[1]=="v"){
    dlname <-"Northward Wind"
    var_def <- ncvar_def(name = "va",
                         units = ncin[["var"]][[variable_infile[1]]][["units"]],
                         list(dimLON,dimLAT,dimPLEV,dimTime),
                         fillvalue,dlname,prec="double")
  }
  

  #netcdf file name 
  time_span<- stri_remove_empty(unlist(strsplit(system(paste("cdo showtimestamp",wind_file),
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
  
  ncvar_put(ncoutput,var_def,interpolated_wind[])
  
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
#wind_file <-u_nc.files[1]
#req_press_levels<- required_PLev
#pressure_file <- ps_nc.files[1]
