        program OrdenaNumeros

        dimension bNum(100000)
        iQtdLinha = 0

        write(*,*) "Digite quantos números devem ser imprimidos"
        read(*,*) m

        open(unit=1, file="entrada-3#13783203.txt")
        do
          read(1,*, end=5) bNum(iQtdLinha+1)
          iQtdLinha = iQtdLinha + 1
        end do
5       close(unit=1)

        open(unit=2, file="saida-3#13783203.txt", status="new")
        do i=1, m
          auxMenorNum = bNum(1)
          do j=1, iQtdLinha
            if (bNum(j) < auxMenorNum) then
              aTemp = auxMenorNum
              auxMenorNum = bNum(j)
              bNum(j) = aTemp
            else
              continue
            end if
          end do
          write(2,*) auxMenorNum
        end do
        close(unit=2)

        end program
