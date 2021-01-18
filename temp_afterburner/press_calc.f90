	subroutine press_calc(m,n,o,p,ps,p0,a,b,press)
	implicit none
	integer :: m,n,o,p
	integer :: x,y,s,t
	double precision :: ps(m,n,p),a(o),b(o),p0
	double precision, intent(out) :: press(m,n,o,p)

	do 20,t=1,p 
	 do 15, s=1,o
	  do 10, x=1,m
	   do 5, y=1,n
		press(x,y,s,t) = a(s) * p0 + b(s) * ps(x,y,t)
5          end do 
10         end do 
15	  end do	
20	end do  
	end subroutine press_calc
