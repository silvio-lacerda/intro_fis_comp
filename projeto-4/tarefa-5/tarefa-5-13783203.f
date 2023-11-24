        program Poincare
        implicit real*8 (a-h,o-z)
        parameter(dt=0.04d0)
        parameter(g=9.8d0)
        parameter(al=9.8d0)
        parameter(n=1000000)
        parameter(gama=0.05d0)
        parameter(omega=2d0/3d0) 
        parameter(f0=0.5d0)

        pi = 4.0d0*atan(1.0d0)
        
        wi = 0.0d0
        w = 0.0d0
        thetai = pi/3d0
        theta = 0.0d0
        t = 0
        open(1, file='saida-5-13783203.dat')
        
        do i=1,n,1
            t=i*dt
            w=w-(g/al)*dsin(theta)*dt-gama*w*dt+f0*dsin(omega*t)*dt
            theta=mod(theta+w*dt, 2.0d0*pi)

            if (mod(omega*t,2d0*pi) .lt. dt/2.0d0) then
                write(1,*) theta, w 
            end if 
        end do

        close(1)
        end program