        program Raizes
        implicit real*8 (a-h, o-z)
        real*8 procuraDireta, newtonRaphson, metodoSecante
        r = [-7, 2, 9]
        
        x0 = -10
        err = 10**(-6)

        open(unit=1, file="saida-3-13783203.txt")
        write(1,*) "Método de Newton-Raphson"
        r_1 = r(1)+1
        r_2 = r(2)+1
        r_3 = r(3)+1
        do i=1, 6
          r_1 = newtonRaphson(r_1)
          r_2 = newtonRaphson(r_2)
          r_3 = newtonRaphson(r_3)
          write(1,*) i, r1, r2, r3
        end do

        write(1,*) 'As raizes pelo metodo da secante sao:'
      
        y1 = r(1) - 0.5
        x1 = r(1) + 0.5
        x2 = r(2) + 0.5
        y2 = r(2) - 0.5
        x3 = r(3) + 0.5
        y3 = r(3) - 0.5
        do j=1,6 
                x1 = ams(x1,y1)
                x2 = ams(x2,y2)
                x3 = ams(x3,y3)
                write(1,*) j,x1,x2,x3
        end do

        close(unit=1)
        end program

        real*8 function f(x)
        f = x**3 - 4*x**2 - 59*x + 126
        return
        end function

        real*8 function df(x)
        df = 3*x**2 - 8*x - 59
        return
        end function
        
        real*8 function procuraDireta(x)
        real*8 f
        end function

        real*8 function newtonRaphson(x)
        real*8 x, f, df
        newtonRaphson = x - f(x)/df(x)
        return
        end function

        real*8 function metodoSecante(x,y)
        real*8 x, y, f
        metodoSecante = x - f(x)*((x-y)/(f(x)-f(y)))
        end function