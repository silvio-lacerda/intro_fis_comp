        program RAIZES
        
        REAL*8 a,b,c
        WRITE(*,*) "Escreva os coeficientes da equacão de segundo grau"
        WRITE(*,*) "Dada a forma f(x) = ax^2 + bx + c"
        READ(*,*) a, b, c

        r = -b/(2*a)
        delta = (b**2) - (4*a*c)

        IF (delta .EQ. 0) THEN
                WRITE(*,*) r
        ELSE IF (delta .LT. 0) THEN
                WRITE(*,*) "Não existem raízes reais"
        ELSE
                rr = SQRT(delta)/(2*a)
                r1 = r + rr
                r2 = r - rr

                WRITE(*,*) r1
                WRITE(*,*) r2
        ENDIF

        end program
        



