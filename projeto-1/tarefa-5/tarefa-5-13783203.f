        program Log
        real*8 x, aLnFortran, aLnSerie, aLn2
        real*8 logNatty
        iLn = 0
        
        write(*,*) "Digite um número: "
        read(*,*) x
        aLnFortran = dLog(x)
        aLnSerie = 0.d0
        aLn2 = 0.d0

        if (x .GT. 2) then 
          aLn2 = logNatty(2.d0)
        end if

        do
          if (x .GT. 2) then
            iLn = iLn+1
            x = x/2.d0
          else
            exit
          end if
        end do
        aLnSerie = logNatty(x) + iLn*aLn2 

        write(*,*) "Log do fortran: ", aLnFortran
        write(*,*) "Log por série: ", aLnSerie

        end program

        real*8 function logNatty(a)
        real*8 a, aux, eperc
        real*8 aLlnS
        i = 1
        eperc = 1e-7
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



