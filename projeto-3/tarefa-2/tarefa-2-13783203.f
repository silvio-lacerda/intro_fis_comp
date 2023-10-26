        program Integral
        implicit real*8 (a-h, o-z)
        real*8 f, trapezio, simpson, boole
        dimension vec_n(10)
	pi = 4d0*atan(1d0)
        f_analitico = 1/(1+4*pi**2)-1/(dexp(1d0)*(1+4*pi**2)) 
        open(unit=1, file="saida-2-13783203.txt")
        write(1,*) "N", "|", 
     & "h", "|",
     & "Regra do Trapezio", "|",
     & "Regra do Simpson", "|",
     & "Regra de Boole"
        
        do i=1, 10
        n = 12*2**i
        write(1,101) n, "|", 
     & 1d0/n, "|",
     & trapezio(0d0, n, f_analitico), "|",
     & simpson(0d0, n, f_analitico), "|",
     & boole(0d0, n, f_analitico)
101     format(I6,A,F18.16,A,F18.16A,F18.16,A,F18.16)
        end do     
        
        write(1,102) "EXATOS", "|",
     & f_analitico, "|",
     & f_analitico, "|",
     & f_analitico
102    format(A, A, F18.16, A, F18.16, A, F18.16, A, F18.16)
           
        close(unit=1)
	end program

        real*8 function f(x)
        real*8 x
        real*8 PI
        PI = 4d0*atan(1d0)
        f = exp(-x)*cos(2*PI*x)
        return 
        end function

        real*8 function trapezio(x, n,an)
        implicit real*8 (a-h, o-z)
        real*8 f, x, an
        integer n
        trapezio = 0d0
        h = 1d0/n
        do j=0,n-1,2
          trapezio=trapezio+(h/2)*(f(j*h)+2*f(h*(j+1))+f(h*(j+2)))
        end do 
        trapezio = abs(trapezio-an)
        return
        end function
       
        real*8 function simpson(x,n,an)
        implicit real*8 (a-h, o-z)
        real*8 f, x, an
        integer n
        simpson=0d0
        h = 1d0/n
        do j=0,n-1,2
          simpson=simpson+(h/3)*(f(h*(j+2))+4*f(h*(j+1))+f(h*j))
        end do 
        simpson = abs(simpson-an)
        return
        end function

        real*8 function boole(x,n,an)
        implicit real*8 (a-h, o-z)
        real*8 f, x, an
        integer n
        boole=0d0
        h = 1d0/n
        do j=0,n-1,4
          boole=boole+(2*h/45)*(7*f(j*h)+32*f(h*(j+1))+12*f(h*(j+2))+
     & 32*f(h*(j+3))+7*f(h*(j+4)))
	end do 
        boole = abs(boole-an)
        return
        end function
