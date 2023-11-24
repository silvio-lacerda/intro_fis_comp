		program PeriodoPendulo
		implicit real*8 (a-h,o-z)
		parameter(g=9.8d0)
		parameter(al=9.8d0)
		
        pi = 4d0*atan(1d0) 
		theta0 = pi/50
		epsolon = 1e-2

		per_analitico = 2d0*pi*sqrt(al/g)
		
		write(*,*) "Periodo integral: ",periodo_eliptica(theta0,epsolon)
		write(*,*) "Periodo aproximado: ",periodo_aproximado(theta0)
		write(*,*) "Periodo analitico: ",per_analitico		
		end program

		real*8 function f(x,x0)
		implicit real*8 (a-h, o-z)
		real*8 x
		f = 1/sqrt(cos(x) - cos(x0))
		return
		end function

		real*8 function periodo_eliptica(x0,epsolon)
		implicit real*8 (a-h, o-z)
		periodo_eliptica = sqrt(2*al/g)*simpson(x0,epsolon)
		periodo_eliptica = periodo_eliptica + 4*sqrt(2*al/g)*
     & sqrt(epsolon/sin(x0))
		return
		end function

		real*8 function periodo_aproximado(x0)
		implicit real*8 (a-h, o-z)
		g=9.8d0
		al=9.8d0
        
		pi = 4d0*atan(1d0)
		periodo_aproximado = 2*pi*sqrt(al/g)*(1+((x0)**2)/16)
		return
		end function

		real*8 function simpson(x0,epsolon)
        implicit real*8 (a-h, o-z)
        real*8 f
        integer n
		simpson=0d0
		n = 5000
		h = 2*(x0-epsolon)/real(n)
		a=-x0+epsolon+h
        do j=1,n/2
			simpson=simpson+(h/3)*(f(a+h,x0)+4*f(a,x0)+f(a-h,x0))
			a=a+2*h
		end do
        return
        end function

