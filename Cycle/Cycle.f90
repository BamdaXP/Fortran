program CycleProgram
    implicit none
    integer :: n,m
    integer :: P,C

    n = 12
    m = 8

    call Calculate(n,m,P,C)

    print "(a,i8)","n:",n
    print "(a,i8)","m:",m
    print "(a,i8)","P:",P
    print "(a,i8)","C:",C

end program CycleProgram

subroutine Calculate(n,m, P,C)
    implicit none
    integer,intent(in) :: n,m
    integer,intent(out) :: P,C

    integer :: i

    P = 1
    C = 1
    do i=m,n
        P = P*i
    end do
    C = P
    do i=1,n-m
        C = C/i
    end do
end subroutine Calculate