program main
    use urcl_ld
    implicit none

    type(string), allocatable :: inputs(:)
    type(replacement), allocatable :: replacements(:)
    character(len=:), allocatable :: outputname
    character(len=256) :: argument
    character(len=:), allocatable :: result
    integer :: numargs, i, rnum, inum, r_option, numinputs, numreplacements, unit
    logical :: o_option, ascii

    outputname = ''
    o_option = .false.
    r_option = 0
    numinputs = 0
    numreplacements = 0

    numargs = command_argument_count()

    !pass 1
    do i=1,numargs
        call get_command_argument(i,argument)
        if (o_option) then
            outputname = trim(argument)
            o_option = .false.
            cycle
        end if

        if (r_option==2) then
            r_option = 1
            cycle
        end if

        if (r_option==1) then
            r_option = 0
            cycle
        end if

        select case (argument)
        case ('--ascii')
        case ('-o')
            o_option = .true.
        case ('-ri','-rp')
            r_option = 2
            numreplacements = numreplacements + 1
        case default
            numinputs = numinputs + 1
        end select
    end do

    allocate(inputs(numinputs),replacements(numreplacements))
    rnum = 0
    inum = 0

    !pass 2
    do i=1,numargs
        call get_command_argument(i,argument)

        if (o_option) then
            o_option = .false.
            cycle
        end if

        if (r_option==2) then
            replacements(rnum)%originalValue = trim(argument)
            r_option = 1
            cycle
        end if

        if (r_option==1) then
            replacements(rnum)%replacementValue = trim(argument)
            r_option = 0
            cycle
        end if

        select case (argument)
        case ('--ascii')
            ascii = .true.
        case ('-o')
            o_option = .true.
        case ('-ri')
            rnum = rnum + 1
            r_option = 2
            replacements(rnum)%type = REPLACE_INST
        case ('-rp')
            rnum = rnum + 1
            r_option = 2
            replacements(rnum)%type = REPLACE_PORT
        case default
            inum = inum + 1
            inputs(inum)%value = trim(argument)
        end select
    end do

    result = linkurcl(inputs,replacements,ascii)

    if (outputname=='') then
        write(*,'(A)',advance='no') result
    else
        open(newunit=unit,file=outputname)
        write(unit,'(A)',advance='no') result
    end if
end program main
