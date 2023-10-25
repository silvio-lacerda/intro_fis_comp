        program Raizes
        implicit real*8 (a-h, o-z)
        real*8 procuraDireta, newtonRaphson, metodoSecante
        
        x0 = -10
        err = 10**(-6)
        
        end program

        real*8 function f(x)
        f = x**3 - 4*x**2 - 59*x + 126
        return
        end function

        real*8 function df(x)
        df = 3*x**2 - 8*x - 59
        return
        end function
        
        real*8 function procuraDireta(inter, x)
        real*8 f
        end function

        real*8 function newtonRaphson(inter, x)
        real*8 x, f, df, x_f

        x_f = x
        do i=1, inter
          x_f = x_f - f(x_f)/df(x_f)
        end do
        newtonRaphson = x_f

        return
        end function

        real*8 function metodoSecante(inter, x)
        real*8 f
        end function