        program Derivada
        real*8 f
        real*8 simetrica3

        write(*,*) simetrica3(1d0/2d0, 0.5d0)

        end program

        real*8 function f(x)
        real*8 x
        f = exp(x/2)*tan(2*x)
        return
        end function

        real*8 function frente2(x, h)
        real*8 f, x, h
        frente2 = (f(x + h) - f(x))/h
        return
        end function

        real*8 function tras2(x, h)
        real*8 f, x, h
        tras2 = (f(x) - f(x - h))/(h)
        return
        end function

        real*8 function simetrica3(x, h)
        real*8 f, x, h
        simetrica3 = (f(x + h) - f(x - h))/(2*h) 
        return
        end function

        real*8 function simetrica5(x, h)
        real*8 f, x, h   
        simetrica5 = (f(x - 2*h) - 8*f(x - h) + 8*f(x + h) - f(x + 2*h))/(12*h)      
        return
        end function

        real*8 function segundaSimetrica5(x, h)
        real*8 f, x, h
        segundaSimetrica5 = (-f(x - 2*h) + 16*f(x - h) - 30*f(x) + 16*f(x + h) - f(x + 2*h))/(12*(h**2))   
        return
        end function

        real*8 function terceiraAntiSimetrica(x, h)
        real*8 f, x, h
        terceiraAntiSimetrica = (-f(x - 2*h) + 2*f(x - h) - 2*f(x + h) + f(x + 2*h))/(2*(h**3))      
        return
        end function
