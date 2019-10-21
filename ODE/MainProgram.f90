
program MainProgram
    
    do i=1,4
        call Euler0(0.1/dble(i))
        call Euler5(0.1/dble(i))
        call RK4(0.1/dble(i))
    end do
end program MainProgram

subroutine Euler0(step)
    
    !TODO_add_body
    use ODE_Solver
    use IO


    real*8,intent(in)  :: step
    real*8 :: x,y,y_next
    character(len=64) :: path
    procedure (func), pointer :: f_ptr => null()
    external f
    f_ptr => f
    x=0
    y=3
    write (path,"(a,f4.3,a)")"./data/euler0_",step,".txt"
    call ClearFile(path)
    do while(x<=1.5)
        call EulerSolver(x,y,f_ptr,step,dble(0),y_next)!0 for explicit Euler solver
        call WriteNumToFile(path,y_next,.true.)
        y=y_next
        x=x+step
    end do
end subroutine Euler0

subroutine Euler5(step)
    
    !TODO_add_body
    use ODE_Solver
    use IO

    real*8,intent(in)  :: step
    real*8 :: x,y,y_next
    character(len=64) :: path
    procedure (func), pointer :: f_ptr => null()
    external f
    f_ptr => f
    x=0
    y=3
    
    write (path,"(a,f4.3,a)")"./data/euler5_",step,".txt"

    call ClearFile(path)
    
    do while(x<=1.5)
        call EulerSolver(x,y,f_ptr,step,dble(0.5),y_next)!0 for explicit Euler solver
        call WriteNumToFile(path,y_next,.true.)
        y=y_next
        x=x+step
    end do
end subroutine Euler5

subroutine RK4(step)
    
    !TODO_add_body
    !TODO_add_body
    use ODE_Solver
    use IO

    real*8,intent(in)  :: step
    real*8 :: x,y,y_next
    character(len=64) :: path
    procedure (func), pointer :: f_ptr => null()
    external f
    f_ptr => f
    x=0
    y=3

    write (path,"(a,f4.3,a)")"./data/rk4_",step,".txt"

    call ClearFile(path)
    do while(x<=1.5)
        call RungeKutta4(x,y,f_ptr,step,y_next)!0 for explicit Euler solver
        call WriteNumToFile(path,y_next,.true.)
        y=y_next
        x=x+step
    end do
end subroutine RK4

function f (x,y) 
    real*8, intent (in) :: x,y
    real*8 :: f   
    f=-(x**2)*(y**2)   
end function f

