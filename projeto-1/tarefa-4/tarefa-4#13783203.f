        program NumerosPrimos
                integer*8 temp
                write(*,*) "Digite um número inteiro"
                read(*,*) n  

                open(unit=1, file="saida-4#13783203.txt", status="new")
                if (n .GT. 1) then
                        temp = 2
1                       if (temp .LE. n) then
                                do i=2, temp-1, 1
                                        if (MOD(temp, i) .EQ. 0) then
                                                temp = temp+1
                                                goto 1
                                        end if
                                end do
                                write(1,*) temp
                                temp = temp+1
                                goto 1
                        end if
                else
                        write(*,*) "Não há primos"
                end if
                close(unit=1)
        end program
