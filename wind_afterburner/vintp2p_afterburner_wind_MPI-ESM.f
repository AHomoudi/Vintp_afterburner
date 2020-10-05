subroutine wind_vertical_interpolation(wind_on_model_level,pres,&
	pressure_full_level,m,n,o,p,req,wind_on_press_level)
implicit none
integer :: m,n,o,p,req
integer :: x,y,s,t,plev
double precision :: pres(req),wind_on_model_level(m,n,o,p)
double precision :: pressure_full_level(m,n,o,p)
double precision :: delta_w,delta_p,grad_w_p,diff_p
double precision, intent(out) :: wind_on_press_level(m,n,req,p)
real :: arg = -1.0,NaN
NaN= sqrt(arg)
do plev=1,req
 do t=1,p 
  do  x=1,m
   do  y=1,n 
    do  s=1,o	
!above uppest level
     if(pres(plev) .LT. pressure_full_level(x,y,1,t)) then 
      wind_on_press_level(x,y,plev,t) = NaN
     end if 
! in between levels
     if(pres(plev) .GT. pressure_full_level(x,y,s,t) .AND. pres(plev) .LT. &
       pressure_full_level(x,y,s+1,t)) then 
      delta_w = wind_on_model_level(x,y,s,t) - wind_on_model_level(x,y,s+1,t)
      delta_p = log(pressure_full_level(x,y,s,t)) &
       - log(pressure_full_level(x,y,s+1,t))
      grad_w_p = delta_w /delta_p
      diff_p = log(pres(plev)) - log(pressure_full_level(x,y,s,t))
      wind_on_press_level(x,y,plev,t) = wind_on_model_level(x,y,s,t) &
        +  grad_w_p * diff_p
     end if  
! extrapolation below the ground  
     if(pres(plev) .GT. pressure_full_level(x,y,o,t)) then	
      wind_on_press_level(x,y,plev,t) = wind_on_model_level(x,y,o,t)   
     end if
    end do 
   end do 
  end do	
 end do
end do   
end subroutine wind_vertical_interpolation
