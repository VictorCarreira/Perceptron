module fbench
    implicit none
    integer, parameter::dp = kind(1.0d0)
    integer,parameter::sp=kind(1.0e0)
    public

contains

    real(kind=dp) function sgn(x)
        integer, parameter::dp = kind(1.0d0)
        real(kind=dp), intent(in):: x

        if(x >= 0.0 ) then
            sgn = +1.0   
        else if (x < 0.0) then
            sgn = -1.0 
        end if
    end function sgn

    real(kind=dp) function lincomb(A, B, n)
        integer,parameter::sp=kind(1.0e0)
        integer, parameter::dp = kind(1.0d0)

        integer(kind=sp) :: i,j
        integer(kind=sp), intent(in):: n
        real(kind=DP), intent(in) :: A(n,n)
        real(kind=DP), intent(in) :: B(n,n)

        lincomb = 0
        do i=1,n
            do j=1,n
            ! lincomb = lincomb + dot_product(A(:,i),B(i,:))
            lincomb = lincomb + (A(j,i)*B(i,j))
            end do
        end do

    end function lincomb

    real(kind=dp) function functional_test(n)
        integer, parameter :: dp = kind(1.0d0)
        integer,parameter :: sp = kind(1.0e0)

        integer(kind=sp), intent(in) :: n
        integer(kind=sp) :: i, j
        real(kind=dp), allocatable, dimension(:,:) :: x, w, wt   

        ALLOCATE(wt(n,n),w(n,n),x(n,n))

        do i=1,n
            do j=1,n
                w(j,i) = 2*rand(0)-1
                x(j,i) = 2*rand(0)-1
            end do
        end do

        wt = transpose(w)
        functional_test = sgn(lincomb(wt, x, n))
        deallocate(wt,w,x)
    end function functional_test

end module fbench
