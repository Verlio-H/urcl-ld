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

    function linkurcl(inputs,replacements) result(linked)
        character(len=:), allocatable :: linked
        type(string), intent(in) :: inputs(:)
        type(replacement), intent(in) :: replacements(:)

        integer :: i
        
        character(len=:), allocatable :: defines, line, instruction
        character(len=:), allocatable :: lineend, substr
        logical :: end
        integer :: unit, replacenum, idx

        type(string), allocatable :: symbols(:)
        type(string), allocatable :: association(:)
        type(string), allocatable :: tmp(:)
        integer :: symbolptr

        symbolptr = 0
        allocate(symbols(64))
        allocate(association(64))

        defines = ''
        linked = ''

        outer: do i=1,size(inputs)
            open(newunit=unit,file=inputs(i)%value)
            do
                line = trim(adjustl(getline(end,unit)))

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
                    if (end) then
                        print'(A)','error: end of file following symbol declaration'
                        stop
                    end if
                    if (line(:1)/='.') then
                        print'(A)','error: symbol declaration not immediately followed by a label'
                        stop
                    end if
                    association(symbolptr)%value = line

                    linked = linked//line//achar(10)
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

                    ! output
                    linked = linked//instruction//line//achar(10)
                end if
            end do
        end do outer

        ! replace symbol references
        do i=1,symbolptr
            associate (symbol=>symbols(i)%value)
                do while (index(linked,symbol)/=0)
                    idx = index(linked,symbol)
                    linked = linked(:idx-1)//association(symbolptr)%value//linked(idx+len(symbol):)
                end do
            end associate
        end do
    end function
end module urcl_ld
