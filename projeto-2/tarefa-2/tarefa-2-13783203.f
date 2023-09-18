        program Andarilho
        real path


        n_passos = 1000
        aMedia = 0.0e0
        aMediaQ = 0.0e0

        write(*,*) "Digite a quantidade de andarilhos"
        read(*,*) m

        do j=1, m
          aMedia = aMedia + path(n_passos)
          aMediaQ = aMediaQ + (path(n_passos)**2)
        end do
        aMedia = aMedia/real(m)
        aMediaQ = aMediaQ/real(m)

        write(*,*) "Valor esperado de <x>: ", aMedia
        write(*,*) "Valor espeerado de <x^2>: ", aMediaQ

        end program

        real function path(n)
        integer n
        logical goFoward

        p_foward = 1.e0/2.e0 ! essa parte é alterada conforme a necessidade
        goFoward = .FALSE.
        i_path = 0
        
        do i=1, n
          goFoward = rand() .LE. p_foward

          if (goFoward .EQV. .FALSE.) then
            i_path = i_path - 1
          else
            i_path = i_path + 1
          end if
        end do
        path = real(i_path)
        return
        end function
