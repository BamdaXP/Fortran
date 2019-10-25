module MyTypes
    implicit none
    type :: Ray
        real*8 :: origin(3)
        real*8 :: direction(3)
    end type

    type :: Sphere
        real*8 :: position(3)
        real*8 :: radius
        real*8 :: color(3)
        real*8 :: emmition

        !1 for Diffuse 
        !2 for Specular
        !3 for Refractive 
        integer :: type
          
    end type


contains

    subroutine Clamp(x,clamped_x)
        real*8,intent(in)  :: x
        real*8,intent(out) :: clamped_x
        !TODO_add_body  
        if(x>1)then
            clamped_x=1
        else if(x<0) then
            clamped_x=0
        end if
    end subroutine Clamp

    subroutine Normalize(vector,out_vec)
        real*8,intent(out) :: out_vec(3)
        real*8,intent(in) :: vector(3)
        out_vec = vector/sqrt(vector(1)**2+vector(2)**2+vector(3)**2)
    end subroutine

    subroutine ToInteger(x,x_int)
        real*8,intent(in)  :: x
        integer,intent(out) ::  x_int
        real*8 :: clamped_x
        !TODO_add_body
        call Clamp(x,clamped_x)
        x_int = int(clamped_x**(1.0/2.2)*255+0.5)
    end subroutine ToInteger

    subroutine SphereIntersect(sph,r,distance)
        type(Sphere),intent(in)  :: sph
        type(Ray),intent(in) :: r
        real*8,intent(out) :: distance
        
        !Local vars
        real*8 :: op(3),eps,b,det

        op=sph%position - r%origin
        eps=1e-4
        b=dot_product(op,r%direction)
        det = b**2-dot_product(op,op)+(sph%radius)**2
        if ( det<0 ) then
            distance=0
            return
        else
            det = sqrt(det)
        end if
        distance=b-det
        if (distance>eps) then
            return
        else
            distance = b+det
            if (distance>eps) then
                return
            else 
                distance = 0
                return
            end if 
        end if
    end subroutine SphereIntersect

    FUNCTION cross(a, b)
        INTEGER :: cross(3)
        INTEGER, INTENT(IN) :: a(3), b(3)

        cross(1) = a(2) * b(3) - a(3) * b(2)
        cross(2) = a(3) * b(1) - a(1) * b(3)
        cross(3) = a(1) * b(2) - a(2) * b(1)
    END FUNCTION cross
end module MyTypes