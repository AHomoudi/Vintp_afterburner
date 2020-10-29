subroutine hus_vertical_interpolation(m,n,o,p,req,pres,&
	pressure_full_level,hus_on_model_level,hus_on_press_level)
implicit none
integer :: m,n,o,p,req,o_sub
integer :: x,y,s,t,plev
double precision :: pres(req),hus_on_model_level(m,n,o,p)
double precision :: pressure_full_level(m,n,o,p)
double precision :: delta_hus,delta_p,grad_hus_p,diff_p
double precision, intent(out) :: hus_on_press_level(m,n,req,p)
real :: arg = -1.0,NaN
NaN= sqrt(arg)
o_sub = o - 1
do plev=1,req
 do t=1,p 
  do x=1,m
   do y=1,n 
    do s=1,o_sub	
!above uppest level
    if(pres(plev) .LT. pressure_full_level(x,y,1,t)) then 
     hus_on_press_level(x,y,plev,t) = NaN
    end if 
! in between levels
    if(pres(plev) .GE. pressure_full_level(x,y,s,t) .AND. pres(plev) .LE. &
     pressure_full_level(x,y,s+1,t) ) then 
     delta_hus = hus_on_model_level(x,y,s,t) - hus_on_model_level(x,y,s+1,t)
     delta_p = log(pressure_full_level(x,y,s,t))&
     - log(pressure_full_level(x,y,s+1,t))
     grad_hus_p = delta_hus /delta_p
     diff_p = log(pres(plev)) - log(pressure_full_level(x,y,s,t))
     hus_on_press_level(x,y,plev,t) = hus_on_model_level(x,y,s,t)&
     + grad_hus_p * diff_p 
    end if  
! extrapolation below the ground  
    if(pres(plev) .GT. pressure_full_level(x,y,o,t)) then
     hus_on_press_level(x,y,plev,t) = hus_on_model_level(x,y,o,t)
    end if
    end do 
   end do 
  end do	
 end do
end do   
end subroutine hus_vertical_interpolation
