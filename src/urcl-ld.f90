module urcl_ld
    implicit none
    private

    public :: linkurcl

    type, public :: string
        character(len=:), allocatable :: value !filename
    end type

    integer, parameter, public :: REPLACE_INST = 0
    integer, parameter, public :: REPLACE_PORT = 1

    type, public :: replacement
        integer :: type
        character(len=:), allocatable :: originalValue
        character(len=:), allocatable :: replacementValue
    end type

contains
    function getline(end,unit) result(line)
        logical, intent(out) :: end
        integer, intent(in) :: unit
        character(len=:), allocatable :: line
        character(len=256) :: readline
        line = ''
    2   read (unit, '(A)', advance='no', eor=3, end=999) readline
        line = line//readline
        goto 2
    3   line = line//readline
        end = .false.
        return
    999 end = .true.
    end

    function toupper(str) result(result)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: result
        integer :: i
        
        allocate(character(len=len(str)) :: result)

        do i=1,len(str)
            if (str(i:i)>='a'.and.str(i:i)<='z') then
                result(i:i) = achar(iachar(str(i:i))-iachar('a')+iachar('A'))
            else
                result(i:i) = str(i:i)
            end if
        end do
    end

    integer function findreplace(replacements,value,type) result(location)
        type(replacement), intent(in) :: replacements(:)
        character(len=*), intent(in) :: value
        integer, intent(in) :: type

        do location=1,size(replacements)
            associate (current=>replacements(location))
                if (current%type==type .and. toupper(current%originalValue)==value) return
            end associate
        end do
        location = 0
    end function

    integer function escapechar(character,fname,lnum) result(char)
        character(1), intent(in) :: character
        character(len=*), intent(in) :: fname
        integer, intent(in) :: lnum
        select case (character)
        case ('n')
            char = 10
        case ('r')
            char = 13
        case ('t')
            char = 9
        case ('b')
            char = 8
        case ('f')
            char = 12
        case ('v')
            char = 11
        case ('0')
            char = 0
        case ('''')
            char = iachar('''')
        case ('"')
            char = iachar('"')
        case ('\')
            char = iachar('\')
        case default
            write(*,'(A)',advance='no') 'error: unknown escape sequence in '//fname//' on line '
            write(*,'(I0)') lnum
        end select
    end function

    pure function itoa(i)
        integer, intent(in) :: i
        character(len=:), allocatable :: itoa
        itoa = repeat(' ',12)
        write(itoa,'(I12)') i
        itoa = trim(adjustl(itoa))
    end function

    function linkurcl(inputs,replacements,ascii) result(linked)
        character(len=:), allocatable :: linked
        type(string), intent(in) :: inputs(:)
        type(replacement), intent(in) :: replacements(:)
        logical, intent(in) :: ascii

        integer :: i, j
        
        character(len=:), allocatable :: defines, headers, line, instruction
        character(len=:), allocatable :: lineend, substr
        character(len=256) :: fname
        logical :: end
        integer :: unit, replacenum, idx, status, lnum, tmpi

        type(string), allocatable :: symbols(:)
        type(string), allocatable :: association(:)
        type(string), allocatable :: tmp(:)
        integer :: symbolptr

        symbolptr = 0
        allocate(symbols(64))
        allocate(association(64))

        defines = ''
        headers = ''
        linked = ''

        outer: do i=1,size(inputs)
            open(newunit=unit,file=inputs(i)%value,status='old')

            idx = max(index(inputs(i)%value,'/',.true.),index(inputs(i)%value,'\',.true.))
            fname = inputs(i)%value(idx+1:)
            if (index(fname,'.')/=0) fname = fname(:index(fname,'.')-1)
            do while (index(trim(fname),' ')/=0)
                idx = index(trim(fname),' ')
                fname(idx:idx) = '-'
            end do
            lnum = 0
            do
                line = trim(adjustl(getline(end,unit)))
                lnum = lnum + 1

                ! replace chars and strings
                if (ascii) then
                    j = 1
                    do while (j<=len(line))
                        if (j<len(line)) then
                            if (line(j:j+1)=='//') then
                                exit
                            end if
                        end if
                        if (line(j:j)=='''') then
                            j = j + 1
                            if (line(j:j)=='\') then
                                tmpi = escapechar(line(j+1:j+1),fname,lnum)
                                line(j-1:j-1) = ' '
                                if (tmpi<100) then
                                    line(j:j) = ' '
                                else
                                    line(j:j) = achar(tmpi/100+48)
                                end if
                                line(j+1:j+1) = achar(mod(tmpi,100)/10+48)
                                line(j+2:j+2) = achar(mod(tmpi,10)+48)
                                j = j + 3
                            else
                                tmpi = iachar(line(j:j))
                                if (tmpi<100) then
                                    line(j-1:j-1) = ' '
                                else
                                    line(j-1:j-1) = achar(tmpi/100+48)
                                end if
                                line(j:j) = achar(mod(tmpi,100)/10+48)
                                line(j+1:j+1) = achar(mod(tmpi,10)+48)
                                j = j + 2
                            end if
                        else if (line(j:j)=='"') then
                            lineend = line(:j-1) !ironic
                            j = j + 1
                            substr = ''
                            do while (line(j:j)/='"')
                                if (line(j:j)=='\') then
                                    j = j + 1
                                    tmpi = escapechar(line(j:j),fname,lnum)
                                else
                                    tmpi = iachar(line(j:j))
                                end if
                                substr = substr//itoa(tmpi)//' '
                                j = j + 1
                            end do
                            line = lineend//substr//line(j+1:)
                            j = len(lineend)+len(substr)+1
                        else
                            j = j + 1
                        end if
                    end do
                end if

                if (end) cycle outer
                if (line(:1)=='!') then
                    !symbol declaration
                    symbolptr = symbolptr + 1
                    if (symbolptr>size(symbols)) then
                        allocate(tmp(size(symbols)+64))
                        tmp(:size(symbols)) = symbols(:)
                        call move_alloc(tmp,symbols)

                        allocate(tmp(size(association)+64))
                        tmp(:size(association)) = association(:)
                        call move_alloc(tmp,association)
                    end if
                    symbols(symbolptr)%value = line
                    line = trim(adjustl(getline(end,unit)))
                    lnum = lnum + 1

                    if (end) then
                        print'(A)','error: end of file following symbol declaration'
                        stop
                    end if
                    if (line(:1)/='.') then
                        print'(A)','error: symbol declaration not immediately followed by a label'
                        stop
                    end if
                    line = '.'//trim(fname)//'_'//line(2:)
                    association(symbolptr)%value = line

                    linked = linked//line//achar(10)
                else if (line(:1)=='.') then
                    linked = linked//'.'//trim(fname)//'_'//line(2:)//achar(10)
                else
                    !replace instruction
                    idx = index(line,' ')
                    if (idx==0) idx = len(line)+1
                    instruction = toupper(line(:idx-1))
                    line = line(idx:)

                    replacenum = findreplace(replacements,instruction,REPLACE_INST)
                    if (replacenum /= 0) then
                        instruction = replacements(replacenum)%replacementValue
                    end if

                    !apply other replacements
                    !replace ports
                    if (index(line,'%')/=0) then
                        if (index(line,'//')==0.or.index(line,'//')>index(line,'%')) then
                            lineend = line(index(line,'%'):)
                            
                            idx = index(lineend,' ')
                            if (idx == 0) idx = len(lineend)+1
                            substr = toupper(lineend(2:idx-1))
                            lineend = lineend(idx:)
                            replacenum = findreplace(replacements,substr,REPLACE_PORT)
                            if (replacenum/=0) then
                                line = line(:index(line,'%')-1)//'%'//replacements(replacenum)%replacementValue//lineend
                            end if
                        end if
                    end if

                    !replace labels
                    do while (index(line,'.')/=0)
                        ! check if in string or comment
                        idx = index(line,'.')
                        status = 0
                        j = 1
                        do while (j<idx)
                            if (status==2) then ! inside single quote
                                if (line(j:j)=='\') then
                                    status = 2
                                else
                                    status = 1
                                end if
                            else if (status==1) then
                                if (line(j:j)/='''') then
                                    write(*,'(A)',advance='no') 'error: missing closing single quote in '//trim(fname)//' on line '
                                    write(*,'(I0)') lnum
                                end if
                                status = 0
                            else if (status==4) then
                                if (line(j:j)=='"') then
                                    status = 0
                                endif
                            else if (status==0) then
                                if (line(j:j)=='"') then
                                    status = 4
                                else if (line(j:j)=='''') then
                                    status = 2
                                else if (line(j:j+1)=='//') then
                                    status = 5
                                    exit
                                end if
                            end if
                            j = j + 1
                        end do
                        if (status==0) then
                            lineend = line(idx+1:)
                            line = line(:idx-1)
                            line = line//achar(17)//trim(fname)//'_'//lineend
                        else
                            line(idx:idx) = achar(17)
                        end if
                    end do
                    do while (index(line,achar(17))/=0)
                        idx = index(line,achar(17))
                        line(idx:idx) = '.'
                    end do


                    select case (instruction)
                    case ('@DEFINE')
                        defines = defines//instruction//line//achar(10)
                    case ('BITS','MINHEAP','MINREG','MINSTACK','RUN')
                        headers = headers//instruction//line//achar(10)
                    case default
                        ! output
                        linked = linked//instruction//line//achar(10)
                    end select
                end if
            end do
        end do outer

        linked = headers//defines//linked
        ! replace symbol references
        do i=1,symbolptr
            associate (symbol=>symbols(i)%value)
                do while (index(linked,symbol//' ')/=0.or.index(linked,symbol//achar(10))/=0)
                    idx = max(index(linked,symbol//' '),index(linked,symbol//achar(10)))
                    linked = linked(:idx-1)//association(i)%value//linked(idx+len(symbol):)
                end do
            end associate
        end do
    end function
end module urcl_ld
