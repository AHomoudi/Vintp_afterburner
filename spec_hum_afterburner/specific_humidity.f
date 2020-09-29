	subroutine spec_hum(temp,spec_hum,m,n,o,p,pressure,relative_hummidity)
	implicit none
	integer :: m,n,o,p
	integer :: x,y,z,t
	double precision,intent(in) ::temp(m,n,o,p),spec_hum(m,n,o,p)
	double precision,intent(in) ::pressure(m,n,o,p)
	double precision, intent(out) :: relative_hummidity(m,n,o,p)
	double precision :: lvRv,To,eo,eps,saturation_vapour_pressure
	LvRv= 5234.0
	To= 273.15
	eo= 0.6113
	eps = 0.622
        do t = 1, p
	 do z = 1, o
            do y = 1, n
                do x = 1, m
                  saturation_vapour_pressure= eo * exp(LvRv*1/To - LvRv*1/temp(x,y,z,t))
		  relative_hummidity(x,y,z,t) = (spec_hum(x,y,z,t)*pressure(x,y,z,t)/1000 &
						/(((1.00-eps)*spec_hum(x,y,z,t))+ eps))&
				                   /saturation_vapour_pressure
                end do
            end do
	  end do 
        end do
	end subroutine relat_hum
