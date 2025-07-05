!En esta subrutina calculamos la energía en cada iteracion.

subroutine Energia(opcion,y,E)
implicit none
integer:: opcion
real*8, dimension(2) :: y
real*8 :: E, omega

omega = 1d0

if (opcion .eq. 1) then
E = 0.5*y(2)*y(2)+ 0.5*omega*y(1)*y(1)
return
end if

if (opcion .eq. 2) then
E = omega*(1.-cos(y(1)))+0.5*y(2)*y(2)
return
end if 

end subroutine
