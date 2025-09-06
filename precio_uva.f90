program precio_uva
    implicit none
    real :: Pinicial, Pfinal, kilos, total
    character :: tipo
    integer :: tamano

    print *, 'Ingrese el precio inicial por kilo de uva:'
    read *, Pinicial
    print *, 'Ingrese el tipo de uva (A o B):'
    read *, tipo
    print *, 'Ingrese el tama¤o de la uva (1 o 2):'
    read *, tamano
    print *, 'Ingrese la cantidad de kilos entregados:'
    read *, kilos

    select case (tipo)
    case ('A','a')

        if (tamano == 1) then
            Pfinal = Pinicial + 0.20
        else if (tamano == 2) then
            Pfinal = Pinicial + 0.30
        else
            print *, 'Tama¤o invalido.'
            stop
        end if
    case ('B','b')
        if (tamano == 1) then
           Pfinal = Pinicial - 0.30
        else if (tamano == 2) then
            Pfinal = Pinicial - 0.50
        else
            print *, 'Tama¤o invalido.'
            stop
        end if
    case default
        print *, 'Tipo invalido.'
        stop
    end select

    total = Pfinal * kilos

    print *, 'El precio final por kilo es: ', Pfinal
    print *, 'El productor recibirá en total: ', total

    pause
end program precio_uva

