subroutine press_calc(m,n,o,p,p0,a,b,orog,ps,temp_ml,press_ml)
implicit none
integer :: m,n,o,p
integer :: x,y,s,t
double precision :: ps(m,n,p),a(o),b(o),p0
double precision ::temp_ml(m,n,o,p),orog(m,n)
double precision, intent(out) :: press_ml(m,n,o,p)
real ::PlanetGrav = 9.80665
real ::PlanetRD = 287.058
real ::height_ml,con 
con= PlanetGrav/PlanetRD
do t=1,p 
 do s=1,o
  do x=1,m
   do y=1,n
    height_ml =  a(s)  + b(s) * orog (x,y)
    press_ml(x,y,s,t) = ps(m,n,p) * exp(-1*(con/temp_ml(x,y,s,t)*(height_ml&
     - orog(x,y))))
   end do 
  end do 
 end do	
end do  
end subroutine press_calc
