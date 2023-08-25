        program Log
        real*8 x
        real*8 lnSerie, lnFortran, ln2
        real*8 logNatty
        iLn = 0
        
        write(*,*) "Digite um número: "
        read(*,*) x
        lnFortran = dLog(x)
        lnSerie = 0.d0
        ln2 = 0.d0

        if (x .GT. 2) then 
          ln2 = logNatty(2.d0)
        end if

        do
          if (x .GT. 2) then
            iLn = iLn+1
            x = x/2.d0
          else
            exit
          end if
        end do
        lnSerie = logNatty(x) + iLn*ln2 

        write(*,*) "Log do fortran: ", lnFortran
        write(*,*) "Log por série: ", lnSerie

        end program

        real*8 function logNatty(a)
        real*8 a, aux, eperc
        real*8 lnS
        i = 1
        eperc = 1e-7
        lnS = 0.0d0
             
        do 
          aux = ((1.0d0-a)**i)/i
          lnS = lnS + aux
          i = i + 1
          if (abs(aux) .LT. eperc) then
            exit
          end if
        end do 
        logNatty = -lnS 
        return
        end function



