!Subrutina metodo runge-kutta de orden cuatro, puesto que nos dara un resultado mas preciso que el de orden dos.

subroutine metodoRK(tsistema,beta,w,gama,y,t,h,y2,n,func)
implicit none
integer :: n,tsistema
real*8 :: ttemp,omega,beta,w,gama
real*8, dimension(n) :: k1,k2,k3,k4,ktemp,ytemp
real*8, intent(in) :: h,t
real*8, intent(in), dimension(n) :: y
real*8, intent(out), dimension(n) :: y2

omega=1d0

!k1
call func(tsistema,beta,w,gama,t,y,ktemp,n)
k1=h*ktemp

!k2
ttemp=t+h/2.
ytemp=y+k1/2.
call func(tsistema,beta,w,gama,ttemp,ytemp,ktemp,n)
k2=h*ktemp

!k3
ttemp=t+h/2.
ytemp=y+k2/2.
call func(tsistema,beta,w,gama,ttemp,ytemp,ktemp,n)
k3=h*ktemp

!k4
ttemp=t+h
ytemp=y+k3
call func(tsistema,beta,w,gama,ttemp,ytemp,ktemp,n)
k4=h*ktemp

y2=y+(1/6.)*(k1+2*k2+2*k3+k4)

end subroutine
