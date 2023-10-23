        program Derivada
        implicit real*8 (a-h, o-z)
        real*8 f
        real*8 frente2, tras2, simetrica3, simetrica5
        real*8 segundaSimetrica5, terceiraAntiSimetrica5
        dimension vec_h(14)
        vec_h = [5.0E-1, 2.0E-1, 1.0E-1, 5.0E-2, 1.0E-2, 5.0E-3, 1.0E-3
     &,5.0E-4, 1.0E-4, 5.0E-5, 1.0E-5, 1.0E-6, 1.0E-7, 1.0E-8]
        x0 = 1d0/2d0

        open(unit=1, file="saida-1-13783203.txt")
        write(1,101) "h", "|",
     & "Simétrica 3 pontos", "|",
     & "P/frente 2 pontos", "|",
     & "P/trás 2 pontos", "|",
     & "Simétrica 5 pontos", "|",
     & "Segunda simétrica 5 pontos", "|",
     & "Terceira anti-simétrica 5 pontos"
101     format(A,9X,A,A,A,A,A,A,A,A,A,A,A,A)

        do i=1, 14
        write(1,100) vec_h(i), "|", 
     & simetrica3(x0, vec_h(i)), "|",
     & frente2(x0, vec_h(i)), "|",
     & tras2(x0, vec_h(i)), "|",
     & simetrica5(x0, vec_h(i)), "|",
     & segundaSimetrica5(x0, vec_h(i)), "|",
     & terceiraAntiSImetrica5(x0, vec_h(i))
100     format(F10.8, A, F24.16, A, F24.16, A, F24.16, A, F24.16, A, 
     & F24.16, A, F30.16)
        end do
        close(unit=1)
        

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
        real*8 A, B, C, D
        A = f(x-2*h)
        B = f(x-h)
        C = f(x+h)
        D = f(x+2*h)
        simetrica5 = (A - 8*B + 8*C - D)/(12*h)      
        return
        end function

        real*8 function segundaSimetrica5(x, h)
        real*8 f, x, h
        real*8  A, B, C, D, E
        A = f(x-2*h)
        B = f(x-h)
        C = f(x)
        D = f(x+h)
        E = f(x+2*h)
        segundaSimetrica5 = (-A + 16*B - 30*C + 16*D - E)/(12*(h**2))   
        return
        end function

        real*8 function terceiraAntiSimetrica5(x, h)
        real*8 f, x, h
        real*8 A, B, C, D
        A = f(x-2*h)
        B = f(x-h)
        C = f(x+h)
        D = f(x+2*h)
        terceiraAntiSimetrica5 = (-A + 2*B - 2*C + D)/(2*(h**3))      
        return
        end function
