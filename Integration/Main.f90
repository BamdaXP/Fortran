program Main
    use Integrate
    real*8 :: simpson_result,trapezoid_result,real_result,f_origin
    real*8 :: step
    external f
    procedure(func),pointer ::  f_ptr=>null()
    
    f_ptr => f
    step =0.01
    call Simpson(f_ptr,dble(1.0),dble(5.0),step,simpson_result)
    call Trapezoid(f_ptr,dble(1.0),dble(5.0),step,trapezoid_result)

    real_result = f_origin(dble(5.0))-f_origin(dble(1.0))

    print "(a,es20.10)","Real result",real_result
    print "(a,es20.10)","Simpson Integration:",simpson_result
    print "(a,es20.10)","Simpson Error:",abs(simpson_result-real_result)
    print "(a,es20.10)","Trapezoid Integration:",trapezoid_result
    print "(a,es20.10)","Trapezoid Error:",abs(trapezoid_result-real_result)
    

end program Main

function f(x)
    real*8,intent(in) :: x
    real*8 :: f
    !TODO_add_body
    f=sin(x)
end function f

function f_origin(x)
    real*8 :: x
    real*8 :: f_origin
    !TODO_add_body

    f_origin=-cos(x)
    
end function f_origin