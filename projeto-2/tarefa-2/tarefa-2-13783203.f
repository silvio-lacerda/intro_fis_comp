        program Andarilho
        integer i_path
        parameter (n_passos=1000)
        dimension vec_hist(-n_passos:n_passos) 

        vec_hist = 0.0e0
        aMedia = 0.0e0
        aMediaQ = 0.0e0

        write(*,*) "Digite a quantidade de andarilhos"
        read(*,*) m

        do j=1, m
          i_x = i_path(n_passos)
          aMedia = aMedia + real(i_x)
          aMediaQ = aMediaQ + (real(i_x)**2)
          
          vec_hist(i_x) = vec_hist(i_x) + 1 
        end do

                
        open(unit=1, file="saida-2-13783203.dat", status="new")
        do i=-n_passos, n_passos
          write(1,*) i, vec_hist(i)
        end do
        close(unit=1)
        
        aMedia = aMedia/real(m)
        aMediaQ = aMediaQ/real(m)

        write(*,*) "Valor esperado de <x>: ", aMedia
        write(*,*) "Valor espeerado de <x^2>: ", aMediaQ

        end program

        function i_path(n)
        logical goFoward

        p_foward = 1.e0/2.e0 ! essa parte é alterada conforme a necessidade
        goFoward = .FALSE.
        i_path = 0
        nd = 0
        ne = 0
        
        do i=1, n
          goFoward = rand() .LE. p_foward

          if (goFoward .EQV. .FALSE.) then
            i_path = i_path - 1
          else
            i_path = i_path + 1
          end if
        end do
        return
        end function
