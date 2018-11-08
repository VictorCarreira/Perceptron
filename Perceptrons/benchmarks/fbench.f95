module fbench
    implicit none
    integer, parameter::dp = selected_real_kind(12,100)
    public

contains

    real(kind=dp) function sgn(x)
        integer, parameter::dp = selected_real_kind(12,100)
        real(kind=dp), intent(in):: x

        if(x >= 0.0 ) then
            sgn = +1.0   
        else if (x < 0.0) then
            sgn = -1.0 
        end if
    end function sgn

    real(kind=dp) function lincomb(A, B, n)
        integer, parameter :: sp = selected_int_kind(r=8)
        integer, parameter :: dp = selected_real_kind(12,100)

        integer(kind=sp) :: i
        integer(kind=sp), intent(in):: n
        real(kind=DP), intent(in) :: A(n,n)
        real(kind=DP), intent(in) :: B(n,n)

        lincomb = 0
        do i=1,n
            lincomb = lincomb + dot_product(A(:,i),B(i,:))
        end do
    end function lincomb

    real(kind=dp) function functional_test(n)
        integer, parameter::dp = selected_real_kind(12,100)
        integer, parameter::sp = selected_int_kind(r=8)

        integer(kind=sp), intent(in):: n
        integer(kind=sp):: i, j
        real(kind=dp), allocatable, dimension(:,:):: x, w, wt   

        ALLOCATE(wt(n,n),w(n,n),x(n,n))

        do i=1,n
            do j=1,n
                w(i,j) = 2*rand(0)-1
                x(i,j) = 2*rand(0)-1
            end do
        end do

        wt = transpose(w)
        functional_test = sgn(lincomb(wt, x, n))
    end function functional_test

end module fbench
