module MyTypes
    implicit none
    type :: Ray
        real*8 :: origin(3)
        real*8 :: direction(3)
    end type

    type :: Sphere
        real*8 :: position(3)
        real*8 :: radius(3)
        real*8 :: color(3)
        real*8 :: emmition

        integer :: type!1 for Diffuse 2 for Specular 3 for Refractive
    end type


contains

    subroutine Clamp(x, )
        real*8,intent(inout)  :: x
        !TODO_add_body  
        if(x>1)then
            x=1
        else if(x<0) then
            x=0
        end if
    end subroutine Clamp

    subroutine ToInteger(x,x_int)
        real*8,intent(in)  :: x
        integer,intent(out) ::  x_int
        !TODO_add_body
        
    end subroutine ToInteger
end module MyTypes