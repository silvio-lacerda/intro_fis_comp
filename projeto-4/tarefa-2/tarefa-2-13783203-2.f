        program PeriodoPenduloAmortecido
        implicit real*8 (a-h,o-z)
        parameter(dt=1e-2)
        parameter(g=9.8d0)
        parameter(al=9.8d0)
        parameter(n=5000)
        parameter(gama=0.05d0)
        parameter(omega=2d0/3d0)
        character*50 fnametheta
        character*50 fnamew
        dimension vec_f(3)
        
        vec_f = [0d0,0.5d0,1.2d0]

        do j=1,3,1
        pi = 4.0d0*atan(1.0d0)
    
        wi = 0.0d0
        w = 0.0d0
        thetai = pi/3d0
        theta = 0.0d0
        t = 0
        f0 = vec_f(j)

        write(fnametheta, 100) j
100    format('saida-2-13783203-theta-',I0,'.dat')

        write(fnamew, 101) j
101     format('saida-2-13783203-w-',I0,'.dat')

        open(j, file=fnametheta)
        open(j+10, file=fnamew)
        
        do i=1,n,1
            t=i*dt
            w=wi-(g/al)*sin(theta)*dt-gama*w*dt+f0*sin(omega*t)*dt
            theta=mod(thetai+w*dt, 2.0d0*pi)
            write(j,*) t, theta
            write(j+10,*) t, w
            
            wi=w
            thetai=theta
        end do

        close(j)
        close(j+10)
        end do
        end program