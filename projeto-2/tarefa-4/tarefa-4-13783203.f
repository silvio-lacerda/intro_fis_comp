        program Entropia
        parameter (n_andarilhos=1000)
        parameter (n_passos=1000)
        dimension vec_x(n_andarilhos)
        dimension vec_y(n_andarilhos)
        
        p_walk = 1.e0/4.e0
        vec_x = 0
        vec_y = 0
        i_s = 50
        
        open(unit=1, file="saida-4-13783203.dat")
        do m=1, n_passos
          do n=1, n_andarilhos
            prob = rand()
            if (prob .LE. p_walk) then
              vec_x(n) = vec_x(n) - 1
            else if (prob .GT. p_walk .AND. prob .LE. p_walk*2) then
              vec_x(n) = vec_x(n) + 1
            else if (prob .GT. p_walk*2 .AND. prob .LE. p_walk*3) then
              vec_y(n) = vec_y(n) - 1
            else
              vec_y(n) = vec_y(n) + 1
            end if
          end do

          S = 0
          do i=-n_passos, n_passos, i_s
            do j=-n_passos, n_passos, i_s
              n_inside = 0
              do k=1, n_andarilhos
                if (vec_x(k) .GT. i .AND. vec_x(k) .LT. i+i_s) then
                  if (vec_y(k) .GT. j .AND. vec_y(k) .LT. j+i_s) then
                    n_inside = n_inside + 1
                  end if
                end if
              end do

              p = real(n_inside)/real(n_andarilhos)
              if (p .GT. 0.0e0) then
                S = S - (p * log(p))
              end if
            end do
          end do
          write(1,*) S
        end do
        close(unit=1)
        end program