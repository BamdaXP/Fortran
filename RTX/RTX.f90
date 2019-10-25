program RTX
    use MyTypes
    real*8 :: w,h,
    
end program RTX

subroutine Intersected(spheres,r,distance,id,result)
    use MyTypes
    logical,intent(out) :: result

    type(Sphere),intent(in) :: spheres(9)
    type(Ray),intent(inout)  :: r
    real*8,intent(inout) :: distance
    integer,intent(inout) :: id
    !TODO_add_body
    !Local vars
    real*8 :: d,inf
    d=1e20
    inf=1e20
    do i = 1, 9
        call SphereIntersect(spheres(i),r,d)
        if ( d>0 .and. d<distance ) then
            distance=d
            id=i
        end if
    end do
    result = t<inf
end subroutine Intersected

recursive function Radiance(spheres,r,depth,xi) result(color)
    use MyTypes

    type(Sphere),intent(in) :: spheres(9)
    type(Ray),intent(inout) :: r
    integer,intent(in) :: depth
    real*8 ::  xi
    real*8,intent(out) :: color(3)
    !Local vars
    real*8 :: t,x(3),n(3),nl(3),f(3),p,w(3),u(3),v(3),d(3),r1,r2,r2s,tdir
    real*8 :: a,nc,nt,nnt,cos2t,ddn,Re,R0,RP,TP,Tr
    type(Ray) :: refray
    integer :: id=0,tmp_depth
    logical :: result,into
    call Intersected(spheres,r,t,id,result)
    if ( .not. result) then
        color=(/0,0,0/)
        return
    end if
    x=r%origin + r%direction*t
    call Normalize(x-spheres(id)%position,n)
    if ( dot_product(n,r%direction)<0 ) then
        nl = n
    else
        nl = n*-1
    end if
    f=spheres(id)%color
    p=maxval(f)

    depth_tmp=depth+1
    if ( depth_tmp>5 ) then
        call random_number(xi)
        if ( xi<p ) then
            f=f*(1/p)
        else
            color=spheres(id)%emmition
            return
        end if
    end if

    if (spheres(id)%type==1)then!Diffusion
        call random_number(xi)
        r1 = 2*3.1415926*xi
        call random_number(xi)
        r2 = xi
        r2s = sqrt(r2)
        w=nl

        call Normalize(abs(w(1))>0.1?(/0,1,0/):cross(/1,0,0/),u)
        v = cross(w,u)
        call Normalize(u*cos(r1)+v*sin(r1)*r2s+w*sqrt(1-r2),d)
        color=spheres(i)%emmition +f*Radiance(spheres,Ray(x,d),tmp_depth,xi)
    else if (spheres(id)%type==2)then!Specular
        color=spheres(id)%emmition+f*(Radiance(spheres,
        Ray(x,r%direction-n*2*dot_product(r%direction,n)),tmp_depth,xi))
    end if

    refray%origin = x
    refray%direction = r%direction-n*2*dot_product(n,r%direction)
    into = dot_product(n,nl)>0
    nc=1
    nt=1.5
    if ( into ) then
        nnt=nc/nt
    else
        nnt=nt/nc
    end if    
    ddn=dot_product(r%direction,nl)
    cos2t=1-nnt**2*(1-ddn**2)
    if ( cos2t<0 ) then
        !Total reflection
        color=spheres(id)%emmition+f*Radiance(spheres,refray,tmp_depth,xi)
        return
    end if
    a=nt-nc
    b=nt+nc
    Ro=a**2/b**2
    if ( into ) then
        tdir=r%direction*nnt-n*(ddn*nnt+sqrt(cos2t))
    else
        tdir=r%direction*nnt+n*(ddn*nnt+sqrt(cos2t))
    end if
    

    if ( into ) then
        c=1+ddn
    else
        c=dot_product(tdir,n)
    end if
    Re=Ro+(1-R0)*c**4
    Tr=1-Re
    P=0.25+0.5*Re
    RP=Re/P
    TP=Tr/(1-P)    
    if ( depth>2 ) then
        call random_number(xi)
        
        if(xi<P)then
            color=Radiance(spheres,refray,tmp_depth,xi)*RP
            return
        else
            color=Radiance(spheres,Ray(x,tdir),tmp_depth,xi)*TP
        end if
        color=Radiance(spheres,refray,tmp_depth,xi)*Re+
        Radiance(spheres,Ray(x,tdir),tmp_depth,xi)*Tr
        return
    end if
end function Radiance