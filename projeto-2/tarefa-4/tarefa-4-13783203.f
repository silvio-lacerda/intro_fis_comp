        program Entropia
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

        i_quadrado = 2
        i_qtd_quadrados = n_passos // i_quadrado
        
        do i=-i_qtd_quadrados, i_qtd_quadrados
         
        end do
        end program
