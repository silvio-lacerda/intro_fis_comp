        program Entropia
        parameter (n_andarilhos=1000)
        parameter (n_passos=1000)
        dimension m_x(n_andarilhos)
        dimension m_y(n_andarilhos)
        integer iMax
        integer iMin

        p_walk = 1.e0/4.e0
        m_x = 0
        m_y = 0
        i_s = 10
        
        open(unit=1, file="saida-4-13783203.dat")
        do m=1, n_passos
          do n=1, n_andarilhos
            prob = rand()
            if (prob .LE. p_walk) then
              m_x(n) = m_x(n) - 1
            else if (prob .GT. p_walk .AND. prob .LE. p_walk*2) then
              m_x(n) = m_x(n) + 1
            else if (prob .GT. p_walk*2 .AND. prob .LE. p_walk*3) then
              m_y(n) = m_y(n) - 1
            else
              m_y(n) = m_y(n) + 1
            end if
          end do

          S = 0

          imin_x = iMin(m_x, n_andarilhos)
          imax_x = iMax(m_x, n_andarilhos)
          imin_y = iMin(m_y, n_andarilhos)
          imax_y = iMax(m_y, n_andarilhos)

          do i=imin_x, imax_x, i_s
            do j=imin_y, imax_y, i_s
              n_inside = 0
              do k=1, n_andarilhos
                if (m_x(k) .GT. i .AND. m_x(k) .LT. i+i_s) then
                  if (m_y(k) .GT. j .AND. m_y(k) .LT. j+i_s) then
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

        function iMax(mm, nn)
        dimension mm(nn)
        i_m = 0

        do ii=1, nn
          if (mm(ii) .GT. i_m) then
            i_m = mm(ii)
          end if
        end do
        iMax = i_m
        return
        end function

        function iMin(mm, nn)
        dimension mm(nn)
        i_m = 0

        do ii=1, nn
          if (mm(ii) .LT. i_m) then
            i_m = mm(ii)
          end if
        end do
        iMin = i_m
        return
        end function
