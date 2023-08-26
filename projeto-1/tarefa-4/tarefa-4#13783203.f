        program NumerosPrimos
        write(*,*) "Digite um número inteiro"
        read(*,*) n  

        open(unit=1, file="saida-4#13783203.txt", status="new")
        if (n .GT. 1) then
          j = 2
1         if (j .LE. n) then
            do i=2, j-1, 1
              if (MOD(j, i) .EQ. 0) then
                j = j+1
                goto 1
              end if
            end do
            write(1,*) j
            j = j+1
            goto 1
          end if
        else
          write(*,*) "Não há primos"
        end if
        close(unit=1)
        end program
