      program EDOAngulo
      implicit real*8 (a-h,o-z)
        
      parameter(ite=2000)
      parameter(dt=1e-2)
      parameter(g=9.8d0)
      parameter(al=9.8d0)
      parameter(m=1.0d0)

      dimension t(ite), w(ite), e(ite), theta(ite)
      dimension w_ec(ite), e_ec(ite), theta_ec(ite)
      theta0 = 0.14 ! aproximadamente 8 graus	
	   open(1, file="saida-theta-t.dat")
		open(2, file="saida-theta-t_ec.dat")
		open(3, file="saida-e-t.dat")
		open(4, file="saida-e-t_ec.dat")

      PI = 4.0d0*atan(1.0d0)
      theta(1) = theta0
      theta_ec(1) = theta0
      w(1) = 0.0d0
      w_ec(1) = 0.0d0
      e(1) = m*g*al*(1-cos(theta0))
      e_ec(1) = m*g*al*(1-cos(theta0))
      do i=1,ite-1,1
         t(i+1) = i*dt
         w(i+1) = w(i) - (g/al)*theta(i)*dt
         w_ec(i+1) = w_ec(i) - (g/al)*theta_ec(i)*dt

         theta(i+1) = mod(theta(i) + w(i)*dt, 2.0d0*pi)
         theta_ec(i+1) = mod(theta_ec(i) + w_ec(i+1)*dt, 2.0d0*pi)

         e(i+1) = m*g*al*(1-cos(theta(i+1))) + (m/2)*(w(i+1)*al)**2
         e_ec(i+1) = m*g*al*(1-cos(theta_ec(i+1))) +
     &(m/2)*(w_ec(i+1)*al)**2
			write(1,*) t(i+1), theta(i+1)
			write(2,*) t(i+1), theta_ec(i+1)
			write(3,*) t(i+1), e(i+1)
			write(4,*) t(i+1), e_ec(i+1)
      end do
      close(1)
		close(2)
		close(3)
		close(4)
		end program
   
