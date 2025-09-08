program MultiplicarMatrices
    implicit none
    integer, parameter :: filas=2, columnas=2
    integer :: A(filas,columnas), B(filas,columnas), C(filas,columnas)
    integer :: i, j, k

    ! Leer matriz A
    print *, "Ingrese los elementos de la matriz A (", filas*columnas, " numeros):"
    do i = 1, filas
        do j = 1, columnas
            read *, A(i,j)
        end do
    end do

    ! Leer matriz B
    print *, "Ingrese los elementos de la matriz B (", filas*columnas, " numeros):"
    do i = 1, filas
        do j = 1, columnas
            read *, B(i,j)
        end do
    end do

    ! Inicializar matriz C
    C = 0

    ! Multiplicación
    do i = 1, filas
        do j = 1, columnas
            do k = 1, columnas
                C(i,j) = C(i,j) + A(i,k)*B(k,j)
            end do
        end do
    end do

    ! Mostrar resultado
    print *, "Matriz resultante C:"
    do i = 1, filas
        write(*,'(2I6)') (C(i,j), j=1,columnas)
    end do
    print *, "Presione ENTER para salir..."
read(*,'(A)')  ! lee una línea vacía para detener el programa

end program MultiplicarMatrices

