        program MonteCarlo
        integer d, n
        real*8 n_dentro, n_total, vol
        real dist
        dist = 0.d0 
        n_dentro = 0.0d0
        n_total = 0.0d0

        write(*,*) "Digite o número de dimensões da esfera"
        read(*,*) d
        write(*,*) "Digite o número de partículas"
        read(*,*) n

        do i=1, n
          dist = 0.0d0
          do j=1, d
            a = rand()
            dist = dist + (a-0.5d0)**(2.0d0)
          end do
          if (SQRT(dist) .LE. 0.5) then
            n_dentro = n_dentro + 1.0d0 
          end if
          n_total = n_total + 1.0d0
        end do
        vol = n_dentro/n_total
        write(*,*) "O volume da esfera em ", d, " dimensões é ", vol
        end program
