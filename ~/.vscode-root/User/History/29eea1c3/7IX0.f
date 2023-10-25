        program Raizes
        implicit real*8 (a-h, o-z)
        real*8 procuraDireta, newtonRaphson, metodoSecante
        r = [-7, 2, 9]
        
        x0 = -10
        err = 10**(-6)

        open(unit=1, file="saida-3-13783203.txt")
        write(1,*) "Método da Procura Direta"

        x1 = r(1)+1
        x2 = r(2)+1
        x3 = r(3)+1
        do i=1, 6
          x1 = newtonRaphson(x1)
          x2 = newtonRaphson(x2)
          x3 = newtonRaphson(x3)
          write(1,*) i, x1, x2, x3
        end do      
        

        write(1,*) "Método de Newton-Raphson"

        x1 = r(1)+1
        x2 = r(2)+1
        x3 = r(3)+1
        do i=1, 6
          x1 = newtonRaphson(x1)
          x2 = newtonRaphson(x2)
          x3 = newtonRaphson(x3)
          write(1,*) i, x1, x2, x3
        end do

        write(1,*) "Método da secante"
      
        y1 = r(1) - 0.5
        x1 = r(1) + 0.5
        x2 = r(2) + 0.5
        y2 = r(2) - 0.5
        x3 = r(3) + 0.5
        y3 = r(3) - 0.5
        do i=1,6 
          x1 = metodoSecante(x1,y1)
          x2 = metodoSecante(x2,y2)
          x3 = metodoSecante(x3,y3)
          write(1,*) i, x1, x2, x3
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
        implicit real*8 (a-h, o-z)
        real*8 f
        parameter (len = 30) 
        dimension vec(len,3)
        parameter (prec = 10e-6)
        
        do i =1,len
          vec(i,j) = 0.0
        end do
        icntr = 1
        fa = f(a)
        i = 1 
        do while (i .eq. 1)
          p = a +(b-a)/2
          fp = f(p)
          if ((fp .eq. 0.0) .or. (abs(b-a)/2 .lt. prec)) then
            vec(icntr,j) = p
            icntr = icntr + 1
            i = i +1
          end if
          if (fa*fp .gt. 0.0) then
            a = p
            fa = fp
            vec(icntr,j) = p
            icntr = icntr + 1
          else
            b = p
            vec(icntr,j) = p
            icntr = icntr + 1
          end if
        end do
        if (j .eq. 3) then
          do i = 1,icntr-1
            write(1,*)i,vec(i,1),vec(i,2),vec(i,3)
          end do
        end if
        return
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