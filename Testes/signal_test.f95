PROGRAM signal_test
 USE sub_perceptron 
 IMPLICIT NONE
  !INTEGER, PARAMETER::SSP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DDP = SELECTED_REAL_KIND(12,100)
  INTEGER(KIND=SP):: i
  REAL(KIND=DDP):: aa, bb, vi, vf, dltv
  REAL(KIND=DDP), ALLOCATABLE, DIMENSION(:,:):: xx, w, wt   

  ALLOCATE(wt(3,3),w(3,3),xx(3,3))
  
  !teste de validação. 

  CALL cpu_time(vi) 
  
  w=0.0
  xx=0.0

  w(1,1)=1.0
  w(1,2)=2.0
  w(1,3)=3.0

  w(2,1)=43.0
  w(2,2)=23.0
  w(2,3)=35.0

  w(3,1)=12.0
  w(3,2)=23.0
  w(3,3)=34.0

  xx(1,1)=4.0
  xx(1,2)=5.0
  xx(1,3)=6.0

  xx(2,1)=76.0
  xx(2,2)=76.0
  xx(2,3)=8.0

  xx(3,1)=45.0
  xx(3,2)=56.0
  xx(3,3)=67.0


 

  wt = transpose(w)
  
  PRINT*,wt

  DO i=1,3
    aa = aa + dot_product(wt(:,i),xx(i,:))
  END DO
  !aa=-aa
  PRINT*,aa

  bb = bin(aa) !Função sinal binária
  
  PRINT*,bb

 CALL cpu_time(vf)

 dltv = vf - vi

 PRINT*,dltv

END PROGRAM signal_test
