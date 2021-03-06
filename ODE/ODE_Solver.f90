module ODE_Solver
    implicit none
    abstract interface 
        !Function form definition of function f to define the function pointer
        function func (x,y) 
            real*8, intent (in) :: x,y
            real*8 :: func         
        end function func 
    end interface
contains
    subroutine EulerSolver(x_in,y_in,f_ptr,step,theta,y_out)
        !y_in:The current inputted y value 
        !x_in:Thecurrent x value
        !f_ptr:The function pointer to f(x,y)
        !step:The step interval
        !theta:0 for the explicit euler function,1 for the implicit euler function
        !y_out:The out put of the next y value
        procedure (func), pointer,intent(in) :: f_ptr
        real*8,intent(in) :: y_in,x_in,step
        real*8,optional::theta
        real*8,intent(out) :: y_out
        if (.not.present(theta))then
            theta = 0.5
        end if

        y_out=y_in+step*((dble(1)-theta)*f_ptr(x_in,y_in)+theta*f_ptr(x_in+step,y_in+step*f_ptr(x_in,y_in)))

    end subroutine

    subroutine RungeKutta4(x_in,y_in,f_ptr,step,y_out)
        !y_in:The current inputted y value 
        !x_in:Thecurrent x value
        !f_ptr:The function pointer to f(x,y)
        !step:The step interval
        !y_out:The out put of the next y value
        procedure (func), pointer :: f_ptr
        real*8,intent(in) :: y_in,x_in,step
        real*8,intent(out) :: y_out
        !Local vars
        real*8 :: K(4)
        !Setup K values
        K(1)=f_ptr(x_in,y_in)
        K(2)=f_ptr(x_in+step/2,y_in+step/2*K(1))
        K(3)=f_ptr(x_in+step/2,y_in+step/2*K(2))
        K(4)=f_ptr(x_in+step,y_in+step*K(3))

        y_out=y_in+step/6*(K(1)+2*K(2)+2*K(3)+K(4))
        
    end subroutine
end module ODE_Solver
