        program PenduloCaotico
        implicit real*8 (a-h,o-z)
        parameter(dt=0.04d0)
        parameter(g=9.8d0)
        parameter(al=9.8d0)
        parameter(n=5000)
        parameter(gama=0.05d0)
        parameter(omega=2d0/3d0) 
        parameter(f0=0.3d0) !1.2

        pi = 4.0d0*atan(1.0d0)
        
        wi = 0.0d0
        w = 0.0d0
        wi2 = 0.0d0
        w2 = 0.0d0
        thetai = 1d0
        thetai2 = thetai + 0.001d0
        theta = 0.0d0
        theta2 = 0.0d0
        t = 0
        open(1, file='saida-3-13783203-03.dat')
        
        do i=1,n,1
            t=i*dt
            w=wi-(g/al)*sin(theta)*dt-gama*w*dt+f0*sin(omega*t)*dt
            theta=mod(thetai+w*dt, 2.0d0*pi)


            t=i*dt
            w2=wi2-(g/al)*sin(theta2)*dt-gama*w2*dt+f0*sin(omega*t)*dt
            theta2=mod(thetai2+w2*dt, 2.0d0*pi)

            write(1,*) t, dlog(abs(theta-theta2)) 

            wi=w
            thetai=theta

            wi2=w2
            thetai2=theta2
        end do

        close(1)
        end program