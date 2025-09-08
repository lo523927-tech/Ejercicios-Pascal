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
    print *, "Ingrese la longitud de la contraseña (8-16):"
    read *, longitud

    ! Inicializar caracteres con minúsculas
    caracteres = minusculas

    ! Preguntar si usar mayúsculas
    print *, "¿Incluir mayúsculas? (s/n):"
    read *, resp
    if (resp == 's' .or. resp == 'S') caracteres = trim(caracteres)//mayusculas

    ! Preguntar si usar números
    print *, "¿Incluir números? (s/n):"
    read *, resp
    if (resp == 's' .or. resp == 'S') caracteres = trim(caracteres)//numeros

    ! Preguntar si usar símbolos
    print *, "¿Incluir símbolos? (s/n):"
    read *, resp
    if (resp == 's' .or. resp == 'S') caracteres = trim(caracteres)//simbolos

    ! Inicializar contraseña
    password = ""

    ! Generar contraseña
    call random_seed()  ! Inicializa la semilla aleatoria
    do i = 1, longitud
        call random_number(r)
        indice = 1 + int(len_trim(caracteres) * r)
        password(i:i) = caracteres(indice:indice)
    end do

    print *, "Contraseña generada: ", trim(password)
    pause
end program GeneradorContrasena


