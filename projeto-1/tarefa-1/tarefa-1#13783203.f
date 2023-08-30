        program Raizes

        write(*,*) "Escreva os coeficientes da equacão de segundo grau"
        write(*,*) "Dada a forma f(x) = ax^2 + bx + c"
        read(*,*) a, b, c

        r = -b/(2*a)
        delta = (b**2) - (4*a*c)

        if (delta .EQ. 0) then
                write(*,*) r
        else if (delta .LT. 0) then
                write(*,*) "Não existem raízes reais"
        else
                rr = SQRT(delta)/(2*a)
                r1 = r + rr
                r2 = r - rr

                write(*,*) r1
                write(*,*) r2
        end if

        end program
        



