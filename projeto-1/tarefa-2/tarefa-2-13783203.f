        program Triangulo

        dimension v1(3), v2(3)
        dimension vet(3)

        write(*,*) "Escreva as coordenadas do primeiro vetor"
        read(*,*) v1
        write(*,*) "Escreva as coordenadas do segundo vetor"
        read(*,*) v2

        vet(1) = v1(2)*v2(3) - v2(2)*v1(3) 
        vet(2) = v1(3)*v2(1) - v2(3)*v1(1)
        vet(3) = v1(1)*v2(2) - v2(1)*v1(2)

        area = 0.5e0*SQRT(vet(1)**2 + vet(2)**2 + vet(3)**2)

        write(*,*) "A área do triângulo é: ", area   

        endprogram
