!En esta subrutina introducimos los diferentes sistemas de ecuaciones.

subroutine sistemaecuaciones(tsistema,beta,w,gama,t,y,k,n)
implicit none
integer :: n,tsistema
real*8 :: omega,beta,w,gama
real*8, intent(in):: t
real*8, intent(in), dimension(n):: y
real*8, intent(out), dimension(n):: k

omega=1d0

if (tsistema .eq. 1) then
k(1)=y(2)
k(2)=-omega*y(1)
end if

if (tsistema .eq. 2) then
k(1)=y(2)
k(2)=-2.*beta*y(2)-omega*y(1)
end if

if (tsistema .eq. 3) then
k(1)=y(2)
k(2)=-2.*beta*y(2)-omega*(y(1)-gama*cos(w*t))
end if 

if (tsistema .eq. 4) then
k(1)=y(2)
k(2)=-omega*sin(y(1))
end if

if (tsistema .eq. 5) then
k(1)=y(2)
k(2)=-2.*beta*y(2)-omega*sin(y(1))
end if

if (tsistema .eq. 6) then
k(1)=y(2)
k(2)=-2.*beta*y(2)-omega*(sin(y(1))-gama*cos(w*t))
end if

end subroutine
