!Aitor Garcia Blanco.
Program Oscilador
implicit none
integer :: i,j,l,g,n,m,reserva,op,tsistema
real*8 :: h,t,x0,xmax,E,wo,w,beta,gama
real*8, allocatable, dimension(:) :: ptos, ptos_n
external :: sistemaecuaciones

!Le pedimos que sistema quiere estudiar, para que posteriormente nos lo escriba en el archivo correspondiente
write(*,*) '1=Oscilador armonico simple'
write(*,*) '2=Oscilador armonico con amortiguamiento'
write(*,*) '3=Oscilador armonico amortiguado forzado'
write(*,*) '4=Pendulo simple sin amortiguamiento'
write(*,*) '5=Pendulo simple con amortiguamiento'
write(*,*) '6=Pendulo simple amortiguado forzado'
read(*,*) tsistema

!Que tipo de energia nos ha de calcular
write(*,*) '1=Energia oscilador armonico, 2=Energia pendulo'
read(*,*) op

!Condiciones iniciales,generalmente hemos usado xmax=30 salvo para PSconAmortiguamiento y Ps forzado con amortiguamiento dondexmax=100
n=2
h=0.0001d0
x0=0d0
xmax=30d0

!Numero de pasos
m=(xmax-x0)/h

!Numero de variables del sistema.
allocate (ptos(n),ptos_n(n))

!Valores iniciales y constantes.En nustro caso la omega siempre vale 1, por lo que no necesitare pasarla y la definire en el resto de subrutinas.
ptos(1) = -0.12d0
ptos(2) = 0.22d0
wo=1.00d0
beta=0.03d0
w=1.05d0
gama=1.25d0

print*, 'Calculando...'

!Iniciamos archivos antes del bucle principal.
t=0d0
open(1,file='Datos1.txt')

!Imponemos estas dos condiciones para que nos escriba la primera linea de datos.
if (op .eq. 1) then
    call Energia(op,ptos(:),E)
    write(1,*) t,ptos(1),ptos(2),E
    else
    call Energia(op,ptos(:),E)
    write(1,*) t,ptos(1),ptos(2),E
end if

do i=0,m-1
    t=t+h
    call metodoRK(tsistema,beta,w,gama,ptos(:),t,h,ptos_n(:),n,sistemaecuaciones)
    !Recolocamos los puntos
    ptos = ptos_n
    call Energia(op,ptos(:),E)
   
    !Escribimos los resultados en el archivo.
    if (tsistema .eq. 1 .and. op .eq. 1) then
        write(1,*) t, ptos(:), E 
    else if (tsistema .eq. 2 .and. op .eq. 1) then
        write(1,*) t, ptos(:), E
    else if (tsistema .eq. 3 .and. op .eq. 1) then
        write(1,*) t, ptos(:), E
    else if (tsistema .eq. 4 .and. op .eq. 2) then
        write(1,*) t, ptos(:), E
    else if (tsistema .eq. 5 .and. op .eq. 2) then
        write(1,*) t, ptos(:), E
    else if (tsistema .eq. 6 .and. op .eq. 2) then
        write(1,*) t, ptos(:), E
    end if

end do
close(1)
close(2)

write(*,*) 'Proceso finalizado'

end program
