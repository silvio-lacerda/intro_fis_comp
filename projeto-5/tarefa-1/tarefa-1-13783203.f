        program VerletOrbita
        implicit real*8 (a-h,o-z)
        parameter(t=2)
        
        dt = 1e-5

        pi = 4d0*atan(1d0)
        GMs = 4d0*(pi**2)
        raio_tab = 1d0
        e = 0.017d0

        x0 = raio_tab
        y0 = 0d0

        v0x = 0d0
        v0y = sqrt(GMs/x0)

        auxx = x0
        auxy = y0

        x = x0 + v0x*dt
        y = y0 + v0y*dt

        open(1,file="saida-1-13783203.dat")

        n = t/dt
        
        do i=1, n
            r = sqrt(x**2 + y**2)

            x = 2d0*x - auxx - (dt**2)*GMs*x/(r**3)
            auxx = x

            r = sqrt(x**2 + y**2)

            y = 2d0*y - auxy - (dt**2)*GMs*y/(r**3)
            auxy = y
            write(1,*) x, y
        end do

        close(1)
        end program
