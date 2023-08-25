        program RaizComplexa
          write(*,*) "Digite um número N: "
          read(*,*) n

          PI = 4.0e0 * atan(1.0e0)
          do i=1, n
            am = 3.0e0**(1.0e0/n)
            ri = am*sin((2.0e0*i*PI)/n)
            rr = 2 + am*cos((2.0e0*i*PI)/n)
            write(*,*) rr, " ", ri,"i"
          end do
        end program
