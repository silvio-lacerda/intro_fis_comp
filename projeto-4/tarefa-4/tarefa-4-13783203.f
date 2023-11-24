        program OmegaCaotico
        implicit real*8 (a-h,o-z)
        dimension f0(2)
        dimension thetas(3)
        character*26 fn
        
        pi = 4.0d0*atan(1.0d0)

        f0 = [0.5d0,1.2d0]
        thetas = [pi/3, pi/10, pi/30]
        
        do jj=1,3
        do ii=1,2
            write(fn, 102) jj,ii
102         format('saida-4-13783203-',I0,'-3-',I0,'.dat')

            call run(thetas(jj),f0(ii),fn, ii)
        end do
        end do
        end program

        subroutine run(theta0,f0,name,indice)
        implicit real*8 (a-h,o-z)
        parameter(dt=0.04d0)
        parameter(g=9.8d0)
        parameter(al=9.8d0)
        parameter(n=5000)
        parameter(gama=0.5d0)
        parameter(omega=2d0/3d0) 
        
        character*26 name
        
        pi = 4.0d0*atan(1.0d0)
        
        wi = 0.0d0
        w = 0.0d0
        thetai = theta0
        theta = 0.0d0
        t = 0

        open(indice,file=name)
        do i=1,n,1
            t=i*dt
            w=wi-(g/al)*sin(theta)*dt-gama*w*dt+f0*sin(omega*t)*dt
            theta=thetai+w*dt

            write(indice,*) theta, w 

            wi=w
            thetai=theta
        end do
        close(indice)
        return
        end