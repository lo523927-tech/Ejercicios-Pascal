program GeneradorContrasena
    implicit none
    integer :: i, longitud, indice
    character(len=200) :: caracteres
    character(len=50) :: password
    character(len=*), parameter :: minusculas="abcdefghijklmnopqrstuvwxyz"
    character(len=*), parameter :: mayusculas="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    character(len=*), parameter :: numeros="0123456789"
    character(len=*), parameter :: simbolos="!@#$%^&*()-_=+[]{};:,.<>?"
    character :: resp
    real :: r

    ! Pedir longitud
    print *, "Ingrese la longitud de la contrase�a (8-16):"
    read *, longitud

    ! Inicializar caracteres con min�sculas
    caracteres = minusculas

    ! Preguntar si usar may�sculas
    print *, "�Incluir may�sculas? (s/n):"
    read *, resp
    if (resp == 's' .or. resp == 'S') caracteres = trim(caracteres)//mayusculas

    ! Preguntar si usar n�meros
    print *, "�Incluir n�meros? (s/n):"
    read *, resp
    if (resp == 's' .or. resp == 'S') caracteres = trim(caracteres)//numeros

    ! Preguntar si usar s�mbolos
    print *, "�Incluir s�mbolos? (s/n):"
    read *, resp
    if (resp == 's' .or. resp == 'S') caracteres = trim(caracteres)//simbolos

    ! Inicializar contrase�a
    password = ""

    ! Generar contrase�a
    call random_seed()  ! Inicializa la semilla aleatoria
    do i = 1, longitud
        call random_number(r)
        indice = 1 + int(len_trim(caracteres) * r)
        password(i:i) = caracteres(indice:indice)
    end do

    print *, "Contrase�a generada: ", trim(password)
    pause
end program GeneradorContrasena


