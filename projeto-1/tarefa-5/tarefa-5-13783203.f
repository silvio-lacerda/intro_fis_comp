        program Log
        real*8 x, aLnFortran, aLnSerie
        real*8 logNatty
        
        write(*,*) "Digite um número: "
        read(*,*) x
        aLnFortran = dLog(x)

        if (x .GT. 1) then 
          aLnSerie = - logNatty(1.0d0/x)
        else
          aLnSerie = logNatty(x)
        end if

        write(*,*) "Log do fortran: ", aLnFortran
        write(*,*) "Log da série  : ", aLnSerie

        end program

        real*8 function logNatty(a)
        real*8 a, aux, eperc
        real*8 aLnS
        i = 1
        eperc = 1e-15
        aLnS = 0.0d0
             
        do 
          aux = ((1.0d0-a)**i)/i
          aLnS = aLnS + aux
          i = i + 1
          if (abs(aux) .LT. eperc) then
            exit
          end if
        end do 
        logNatty = -aLnS 
        return
        end function



