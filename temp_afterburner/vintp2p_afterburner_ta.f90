subroutine ta_vertical_interpolation(m,n,o,p,req,pres,geop,&
	pressure_full_level,surface_pressure,ta_on_model_level,ta_on_press_level)
implicit none
integer :: m,n,o,p,q,req,o_sub
integer :: x,y,s,z,t,plev
double precision ::pres(req),ta_on_model_level(m,n,o,p)
double precision ::geop(m,n)
double precision ::pressure_full_level(m,n,o,p),surface_pressure(m,n,p)
double precision ::delta_ta,delta_p,grad_ta_p,diff_p
double precision ::tstar,ztmsl,zrg,zalp,zalph,zhts,ztsz 
double precision, intent(out) :: ta_on_press_level(m,n,req,p)
real ::arg = -1.0,z1
real ::PlanetGrav = 9.80665
real ::PlanetRD = 287.058
real ::zlapse = 0.0065
zrg   = 1.0 / PlanetGrav
o_sub = o - 1
do plev=1,req
 do t=1,p 
  do x=1,m
   do y=1,n 
    do s=1,o_sub	
!above uppest level
     if(pres(plev) .LT. pressure_full_level(x,y,1,t)) then 
      ta_on_press_level(x,y,plev,t) = sqrt(arg) 
     end if 
! in between levels
     if(pres(plev) .GE. pressure_full_level(x,y,s,t) .AND. pres(plev) .LE. &
      pressure_full_level(x,y,s+1,t) ) then 
      delta_ta = ta_on_model_level(x,y,s,t) - ta_on_model_level(x,y,s+1,t)
      delta_p = pressure_full_level(x,y,s,t) - pressure_full_level(x,y,s+1,t)
      grad_ta_p = delta_ta /delta_p
      diff_p = pres(plev) - pressure_full_level(x,y,s,t) 
      ta_on_press_level(x,y,plev,t) = ta_on_model_level(x,y,s,t) + grad_ta_p &
      * diff_p 
     end if  
! extrapolation below the ground  
     if(pres(plev) .GT. pressure_full_level(x,y,o,t)) then
! temperature at surface equation 5 
      tstar = (1.0 + zlapse * PlanetRD * zrg * (surface_pressure(x,y,t) &
       /pressure_full_level(x,y,o,t) - 1.0)) * ta_on_model_level(x,y,o,t)
      ztsz  = tstar
!T _ 0 
      z1    = tstar + zlapse * zrg * geop(x,y)
! tests and modifications 
! equation 14.3
      if(tstar .LT. 255.0) then
       tstar = 0.5 * (255.0 + tstar)
       ztmsl = tstar + zlapse * zrg * geop(x,y)
      end if 
! equation 14.2 
      if(ztmsl .GT. 290.5 .AND. tstar .GT. 290.5) then
       tstar = 0.5 * (290.5 + tstar)
       ztmsl = tstar
      end if 
! equation 14.1
      if(ztmsl .GT. 290.5 .AND. tstar .LE. 290.5) then
       ztmsl=290.5
       zalph = PlanetRD*zlapse*zrg
      end if 
      if(ztmsl-tstar .LE. 0.000001 .AND. tstar-ztmsl .LE. 0.000001) then
       zalph=0.0
      end if 
      if((ztmsl-tstar > 0.000001 .OR. tstar-ztmsl > 0.000001) .AND. &
       (geop(x,y) .GT. 0.0001 .OR. geop(x,y) .GT. -0.0001)) then
       zalph = PlanetRD*(ztmsl-tstar)/geop(x,y)
      end if 
! linear interpolation 
      if(pres(plev) .LE. surface_pressure(x,y,t)) then
       ta_on_press_level(x,y,plev,t) = ((surface_pressure(x,y,t) - pres(plev))&
        *ta_on_model_level(x,y,o,t) +(pres(plev)-pressure_full_level(x,y,o,t))&
        *tstar)/ (surface_pressure(x,y,t)-pressure_full_level(x,y,o,t))
      else
! return to intial values
       ztmsl = z1
       tstar = ztsz
       zhts  = geop(x,y) * zrg
       if(zhts .GT. 2000.0 .AND. z1 .GT. 298.0) then
        ztmsl = 298.0
        if(zhts .LT. 2500.0) then 
! eqution 19b
         ztmsl = 0.002*((2500.0 - zhts)*z1 + (zhts-2000.0)*ztmsl)
        end if
       end if  
       if((ztmsl-tstar) .LT. 0.000001) then
        zalph = 0.
       else
        if(geop(x,y) .GT. 0.0001 .OR. geop(x,y) .LT. -0.0001) then
         zalph = PlanetRD*(ztmsl-tstar)/geop(x,y)
        else
         zalph = PlanetRD*zlapse*zrg
        end if
       end if 
       zalp  = zalph * log(pres(plev)/surface_pressure(x,y,t))
       ta_on_press_level(x,y,plev,t) = tstar*(1.0+zalp*(1.0+zalp &
       *(0.5+0.16666666667*zalp)))
      end if 
     end if 
    end do 
   end do 
  end do	
 end do
end do   
end subroutine ta_vertical_interpolation
