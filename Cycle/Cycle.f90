program CycleProgram
    implicit none
    integer :: n,m
    real*8 :: P,C

    n = 12
    m = 8

    call Calculate(n,m,P,C)

    print "(a,i8)","n:",n
    print "(a,i8)","m:",m
    print "(a,f16.3)","P:",P
    print "(a,f16.3)","C:",C

end program CycleProgram

subroutine Calculate(n,m, P,C)
    implicit none
    integer,intent(in) :: n,m
    real*8,intent(out) :: P,C

    integer :: i

    P = 1
    C = 1
    do i=m+1,n
        C = C*i
    end do
    do i=1,n-m
        C = C/i
    end do
    P = C
    do i=1,m
        P = P*i
    end do
end subroutine Calculate