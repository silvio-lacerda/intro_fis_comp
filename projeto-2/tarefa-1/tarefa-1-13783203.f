        program ValorEsperado
        n = 1000000
        aMedia = 0.0e0
        do i=1, n
          aMedia = aMedia + rand()
        end do
        aMedia = aMedia/real(n)

        write(*,*) "Média de ", n, " números aleatórios: "
        write(*,*) aMedia
        end program    
