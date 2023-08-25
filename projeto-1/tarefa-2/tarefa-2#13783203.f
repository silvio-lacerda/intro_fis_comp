        program TRIIANGULO

        DIMENSION v1(3), v2(3)
        REAL*8 area

        WRITE(*,*) "Escreva as coordenadas do primeiro vetor"
        READ(*,*) v1
        WRITE(*,*) "Escreva as coordenadas do segundo vetor"
        READ(*,*) v2

        area = ABS(DOT_PRODUCT(v1,v2))

        WRITE(*,*) "A área do triângulo é: ", area   

        endprogram
