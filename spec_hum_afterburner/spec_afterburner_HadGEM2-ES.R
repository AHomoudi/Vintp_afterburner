#A function that will receive Specific_humidity nc file +surface air pressure +   
#required pressure levels then  write the interpolated values to a netcdf file 

Specific_humidity_afterburner<-function(spec_hum_file,temp_file,surf_gepo,pressure_file,req_press_levels){
  
  require(ff)
  require(ncdf4)
  require(stringi)
  #1============================================================================ 
  #Reading data out of netCDF files 
  
  blabla <- unlist(strsplit(unlist(strsplit(spec_hum_file,"[/]"))[2],"_"))
  
  #Read specific humidity 
  ncin<-nc_open(spec_hum_file)
  
  DIM<-ncin[["var"]][[blabla[1]]][["size"]]
    
  spec_hum_data<-ff(ncvar_get(ncin,blabla[1]),
                  dim = DIM,
                  dimnames = list(longitude= ncin[["dim"]][["lon"]][["vals"]],
                                  latitude= ncin[["dim"]][["lat"]][["vals"]],
                                  lev = ncin[["dim"]][["lev"]][["vals"]],
                                  TIME =ncin[["dim"]][["time"]][["vals"]]))
  DIMNAMES<- dimnames(spec_hum_data)
  
  DIMNAMES[["lev"]]<- Rev(DIMNAMES[["lev"]])
  
  spec_hum_data<-ff(Rev(spec_hum_data[],3),dim = DIM,
                dimnames = DIMNAMES)
  
  dim(spec_hum_data)
  
  longitude <- ncin[["dim"]][["lon"]][["vals"]]
  latitude<- ncin[["dim"]][["lat"]][["vals"]]
  lev <- Rev(ncin[["dim"]][["lev"]][["vals"]])
  TIME <-ncin[["dim"]][["time"]][["vals"]]
  
  
  
  p0<-1.0
  
  a<-Rev(ncvar_get(ncin,"lev"))
  
  a_bnds<-Rev(ncvar_get(ncin,"lev_bnds"))
  
  b<-as.vector(read.csv("b.csv",header = T)$b)
  
  
  #2============================================================================ 
  
  ncin<-nc_open(pressure_file)
  
  DIM<-ncin[["var"]][["ps"]][["varsize"]]
  
  
  surface_pressure<-ff(ncvar_get(ncin,"ps"),
                       dim = DIM,
                       dimnames = list(longitude= ncin[["dim"]][["lon"]][["vals"]],
                                       latitude= ncin[["dim"]][["lat"]][["vals"]],
                                       TIME =ncin[["dim"]][["time"]][["vals"]]))
  
  dim(surface_pressure)
  
  #3============================================================================ 
  #read Geopotential data /orography
  ncin<-nc_open(surf_gepo)
  
  med <- ncvar_get(ncin,"orog")
  
  #H<-(med * 6356.766e03)*9.80665/(med + 6356.766e03)
  
  surf_gepo_array<-ff(med,
                      dim = dim(ncvar_get(ncin,"orog")),
                      dimnames = list(longitude= ncin[["dim"]][["lon"]][["vals"]],
                                      latitude= ncin[["dim"]][["lat"]][["vals"]]))
  rm(ncin,med)
  
  dim(surf_gepo_array)
  #3============================================================================ 
  #Read temperature data 
  blabla2 <- unlist(strsplit(unlist(strsplit(temp_file,"[/]"))[2],"_"))
  
  
  ncin<-nc_open(temp_file)
  
  DIM<-ncin[["var"]][[blabla2[1]]][["varsize"]]
  
  temp_data<-ff(ncvar_get(ncin,blabla2[1]),
                dim = DIM,
                dimnames = list(longitude= ncin[["dim"]][["lon"]][["vals"]],
                                latitude= ncin[["dim"]][["lat"]][["vals"]],
                                lev = ncin[["dim"]][["lev"]][["vals"]],
                                TIME =ncin[["dim"]][["time"]][["vals"]]))
  
  DIMNAMES2<- dimnames(temp_data)
  
  DIMNAMES2[["lev"]]<- Rev(DIMNAMES2[["lev"]])
  
  temp_data<-ff(Rev(temp_data[],3),dim = DIM,
                dimnames = DIMNAMES2)
  
  dim(temp_data)
  #4============================================================================ 
  #load pressure on model calculator 
  dyn.load("spec_hum_afterburner/press_calc_HadGEM2-ES.so")
  #check
  is.loaded("press_calc")
  
  DIM<-dim(spec_hum_data)
  
  output_array<-ff(array(0.00),dim =DIM)
  
  
  
  result<- array(.Fortran("press_calc",
                          m=as.integer(DIM[1]),
                          n=as.integer(DIM[2]),
                          o=as.integer(DIM[3]),
                          p=as.integer(DIM[4]),
                          p0=as.numeric(p0),
                          a=as.numeric(a),
                          b=as.numeric(b),
                          orog =as.numeric(surf_gepo_array[]) ,
                          ps= as.numeric(surface_pressure[]),
                          temp_ml = as.numeric(temp_data[]) ,
                          press_ml=as.numeric(output_array[]))$press_ml,
                 dim =DIM)
  
  pressure<-ff(result,dim = dim(spec_hum_data),dimnames = dimnames(spec_hum_data))
  
  rm(result)
  
  dim(pressure)

  #5============================================================================
  
  #load vertical interpolate subroutine for specific humidity
  dyn.load("spec_hum_afterburner/vintp2p_afterburner_hus.so")
  #check
  is.loaded("hus_vertical_interpolation")
  
  DIM<-dim(spec_hum_data)
  
  output_DIM<-c(DIM[1],DIM[2],length(req_press_levels),DIM[4])
  
  output_array<-ff(array(0.00,dim =output_DIM),dim =output_DIM)

  result<- array(.Fortran("hus_vertical_interpolation",
                          m=as.integer(DIM[1]),
                          n=as.integer(DIM[2]),
                          o=as.integer(DIM[3]),
                          p=as.integer(DIM[4]),
                          req = as.integer(length(req_press_levels)),
                          pres=as.numeric(req_press_levels),
                          pressure_full_level=as.numeric(pressure[]),
                          hus_on_model_level=as.numeric(spec_hum_data[]),
                          hus_on_press_level=as.numeric(output_array[]))$hus_on_press_level,
                 dim =output_DIM)
  
  DIMNAMES<-dimnames(spec_hum_data)
  
  DIMNAMES[["lev"]]<-req_press_levels
  
  Specific_humidity<- ff(result, dim = output_DIM,
                         dimnames =DIMNAMES )
  rm(result)
  
  #5============================================================================
  # Writing netcdf file of the interpolated values 
  
  ncin<-nc_open(spec_hum_file)
  
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
  
  dlname <- "Specific Humidity"
  
  var_def <- ncvar_def(name = blabla[1],
                       units = "kg/kg",
                       list(dimLON,dimLAT,dimPLEV,dimTime),
                       fillvalue,dlname,prec="double")
  
  #netcdf file name 
  time_span<- stri_remove_empty(unlist(strsplit(system(paste("cdo showtimestamp",spec_hum_file),
                                                       intern = TRUE),"[ ]")))
  
  time_span<-str_replace_all(time_span, "[[:punct:]]", " ")%>% str_remove( pattern = "[T]")%>%
    str_replace_all(fixed(" "), "")
  
  
  ncname<-paste0(blabla[1],"_6hrPlev_",blabla[3],"_",blabla[4],
                 "_",blabla[5],"_",
                 time_span[1],"-",time_span[length(time_span)],"_.nc")
  #netCDF file location 
  
  ncpath <- "hus/"
  
  ncfname <- paste0(ncpath, ncname)
  
  #create netCDF file and put arrays
  ncoutput<-nc_create(ncfname,
                      list(var_def),
                      force_v4=TRUE,verbose = TRUE) 
  #put variable 
  
  ncvar_put(ncoutput,var_def,Specific_humidity[])
  
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

  #history <- paste( bla bla bla ....etc )
  #ncatt_put(ncoutput,0,"history",history)
  #ncatt_put(ncoutput,0,"Conventions",Conventions$value)

  #history <- paste(bla bla bla ...etc )
  #ncatt_put(ncoutput,0,"history",history)
  #ncatt_put(ncoutput,0,"Conventions",Conventions$value)


  
  
  
}
#Test
#spec_hum_file <-q_nc.files[1]
#req_press_levels<- required_PLev
#pressure_file <- ps_nc.files[1]
