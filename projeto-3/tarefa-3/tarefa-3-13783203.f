        program Raizes
        implicit real*8 (a-h, o-z)
        real*8 newtonRaphson, metodoSecante
        dimension r(3)
        r = [-7, 2, 9]
         
        open(unit=1, file="saida-3-13783203.txt")
        write(1,*) "Método da Procura Direta"
        call procuraDireta()
        

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
        real*8 x
        f = x**3 - 4*x**2 - 59*x + 126
        return
        end function

        real*8 function df(x)
        real*8 x
        df = 3*x**2 - 8*x - 59
        return
        end function
        
        subroutine procuraDireta()
        implicit real*8 (a-h, o-z)
        dimension r_pd(3)
        dimension cont_pd(3)
        x0 = -10
        dx = 1e-1
        j = 1
	do while(j .lt. 4)
	do while(f(x)*f(x+dx) .gt. 0)
	   x=x0+i*dx
	   i=i+1
	end do
	a=x
	b=x+dx
	c=b-a
	cont=0
	xm=(a+b)/2
	do while((c .gt. 1e-6) .or. (abs(f(xm)) .gt. 10e-6))
	  if (f(xm)*f(a).LT.0) then
	    b=xm
	  else if(f(xm)*f(a) .gt. 0) then
	    a=xm
	  end if
	c=b-a
	xm=(a+b)/2
	cont=cont+1
	if(cont .gt. 100) then
	    ! limite de iterações excedido
    	  stop
	end if
	end do
	r_pd(j)=xm
	cont_pd(j) = cont
	x=b+dx
	j=j+1
      	end do
      	write(1,*) maxv(cont_pd), r_pd(1), r_pd(2), r_pd(3)
        return
        end
        
        integer function maxv(arrr)
        implicit real*8 (a-h, o-z)
        dimension arrr(3)
        temp = arrr(1)
        do l=1, 3
       	  if (arrr(l) .GT. temp) then
       	    temp = arrr(l)
       	   end if
        end do
        maxv = temp
        end function

        real*8 function newtonRaphson(x)
        real*8 x, f, df
        newtonRaphson = x - f(x)/df(x)
        return
        end function

        real*8 function metodoSecante(x,y)
        real*8 x, y, f
        metodoSecante = x - f(x)*((x-y)/(f(x)-f(y)))
        return
        end function
