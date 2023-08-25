        program VolumeEsfera
        real*8 g, vol
        real*8 r, PI
        real*8 aux1, aux2
        integer d

        PI = 4.0d0*ATAN(1.0)
        
        write(*,*) "Digite quantas dimensões tem a esfera"
        read(*,*) d
        write(*,*) "Digite o raio da esfera"
        read(*,*) r

        open(unit=1,file="dimensoes-esferas.dat", status="new")
        do i=2, d
          d_i = real(i,8)
          aux1 = g(d_i/2.0d0 + 1.0d0)
          aux2 = ((PI**(d_i/2.0d0))*(r**d_i))
 
          vol = aux2/aux1
          write(1,*) i, vol
        end do
        close(unit=1)
        end program

        recursive real*8 function g(x) result(seq)
        real*8 x, PI
        PI = 4.0d0*ATAN(1.0)

        if (x .EQ. 0.5d0) then
          seq = SQRT(PI)
        else if (x .EQ. 1.0d0) then
          seq = 1.0d0
        else
          seq = (x-1.0d0)*g(x-1.0d0)
        end if
        return
        end function g
