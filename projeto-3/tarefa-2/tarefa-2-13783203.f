        program Integral
        implicit real*8 (a-h, o-z)
        real*8 f, trapezio, simpson, boole
        dimension vec_h(10)
        vec_h = [1/12d0, 1/24d0, 1/48d0, 1/96d0, 1/192d0, 1/384d0,
     & 1/768d0, 1/1536d0, 1/3072d0, 1/6144d0]

        open(unit=1, file="saida-2-13783203.txt")
        write(1,*) "h", "|",
     & "Regra do Trapezio", "|",
     & "Regra do Simpson", "|",
     & "Regra de Boole"
        
        do i=1, 10
        write(1,*) vec_h(i), "|",
     & trapezio(0d0, vec_h(i)), "|",
     & simpson(0d0, vec_h(i)), "|",
     & boole(0d0, vec_h(i))
        end do        
        close(unit=1)
	end program


        real*8 function f(x)
        real*8 x
        real*8 PI
        PI = 4d0*atan(1d0)
        f = exp(-x)*cos(2*PI*x)
        return 
        end function

        real*8 function trapezio(x,h)
        real*8 f, x, h
        real*8 A, B, C
        A = f(x - h)
        B = f(x)
        C = f(x + h)
        trapezio = (A + 2*B + C)*(h/3)
        return
        end function
       
        real*8 function simpson(x,h)
        real*8 f, x, h
        real*8 A, B, C
        A = f(x+h)
        B = f(x)
        C = f(x-h)
        simpson = (A + 4*B + C)*(h/3)
        return
        end function

        real*8 function boole(x,h)
        real*8 f, x, h
        real*8 A, B, C, D, E
        A = f(x)
        B = f(x+h)
        C = f(x+2*h)
        D = f(x+3*h)
        E = f(x+4*h)
        boole = (7*A + 32*B + 12*C + 32*D + 7*E)*(2*h/45)
        return
        end function
