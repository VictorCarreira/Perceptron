PROGRAM signal_test
 USE mod  
 IMPLICIT NONE
  !INTEGER, PARAMETER::SSP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DDP = SELECTED_REAL_KIND(12,100)
  REAL(KIND=DDP):: aa, bb
  REAL(KIND=DDP), ALLOCATABLE, DIMENSION(:,:):: xx, w, wt   

  ALLOCATE(wt(3,3),w(3,3),xx(3,3))
  
  !teste de validação. 

  w(1,1)=1.0
  w(1,2)=2.0
  w(1,3)=3.0

  xx(1,1)=4.0
  xx(1,2)=5.0
  xx(1,3)=6.0

  CALL transpostaV(3,w)
  
  aa = dot_product(w,xx)
  
  PRINT*,aa

  !CALL Signal(aa,bb) 
  
  !PRINT*,bb



END PROGRAM signal_test
