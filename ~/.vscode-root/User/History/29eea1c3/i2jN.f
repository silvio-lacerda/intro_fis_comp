        program Raizes
        implicit real*8 (a-h, o-z)
        real*8 procuraDireta, newtonRaphson, metodoSecante

        
        end program

        real*8 function f(x)
        f = x**3 - 4*x**2 - 59*x + 126
        return
        end function

        real*8 function df(x)
        df = 3*x**2 - 8*x - 59
        return
        end function
        
        real*8 function procuraDireta(inter)
        real*8 f
        end function

        real*8 function newtonRaphson(inter)
        real*8 f
        end function

        real*8 function metodoSecante(inter)
        real*8 f
        end function

        recursive real*8 function newtonRaphson(x) result(seq)
        real*8 x, f, df

        seq = x - f(x)/df(x)
        return
        end function newtonRaphson
