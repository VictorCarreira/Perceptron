PROGRAM signal_test
 USE sub_perceptron 
 IMPLICIT NONE
  !INTEGER, PARAMETER::SSP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DDP = SELECTED_REAL_KIND(12,100)
  INTEGER(KIND=SP):: i
  REAL(KIND=DDP):: aa, bb
  REAL(KIND=DDP), ALLOCATABLE, DIMENSION(:,:):: xx, w, wt   

  ALLOCATE(wt(3,3),w(3,3),xx(3,3))
  
  !teste de validação. 

  w=0.0
  xx=0.0

  w(1,1)=1.0
  w(1,2)=2.0
  w(1,3)=3.0

  w(2,1)=-4.0
  w(2,2)=-5.0
  w(2,3)=-6.0

  w(3,1)=7.0
  w(3,2)=8.0
  w(3,3)=9.0

  xx(1,1)=1.0
  xx(1,2)=2.0
  xx(1,3)=3.0

  xx(2,1)=4.0
  xx(2,2)=5.0
  xx(2,3)=6.0

  xx(3,1)=-1.0
  xx(3,2)=-1.0
  xx(3,3)=-1.0

  wt = transpose(w)
  
  !PRINT*,wt

  DO i=1,3
    aa = aa + dot_product(wt(:,i),xx(i,:))
  END DO
  !aa=-aa
  PRINT*,aa

  bb = bin(aa)
  
  PRINT*,bb



END PROGRAM signal_test
