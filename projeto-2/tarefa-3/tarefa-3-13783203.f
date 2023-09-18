        program Andarilh2D
        parameter (n_andarilhos=1000)
        parameter (n_passos=1000)
        dimension vec_x(n_andarilhos)
        dimension vec_y(n_andarilhos)
        dimension i_r(2)
        dimension i_path(2)

        vec_x = 0
        vec_y = 0

        p_walk = 1.e0/4.e0

        aMedia = 0.0e0
        aMediaQ = 0.0e0

        do j=1, n_andarilhos
          i_r = 0

          do i=1, n_passos
            prob = rand()
            if (prob .LE. p_walk) then
              i_r(1) = i_r(1) - 1
            else if (prob .GT. p_walk .AND. prob .LE. p_walk*2) then
              i_r(1) = i_r(1) + 1
            else if (prob .GT. p_walk*2 .AND. prob .LE. p_walk*3) then
              i_r(2) = i_r(2) - 1
            else
              i_r(2) = i_r(2) + 1
            end if
          end do

          r = SQRT(real(i_r(1))**2 + real(i_r(2))**2)
          aMedia = aMedia + r
          aMediaQ = aMediaQ + r**2

          vec_x(j) = i_r(1)
          vec_y(j) = i_r(2)
        end do


        open(unit=1, file="saida-3-13783203.dat", status="new")
        do i=1,lim
          write(1,*) vec_x(i), vec_y(i)
        end do
        close(unit=1)

        aMedia = aMedia/real(m)
        aMediaQ = aMediaQ/real(m)

        write(*,*) "Valor esperado de <r>: ", aMedia
        write(*,*) "Valor espeerado de <r^2>: ", aMediaQ

        end program
