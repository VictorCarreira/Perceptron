MODULE activation
IMPLICIT NONE
  PUBLIC
  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:)::pp
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: xxi, w
  REAL(KIND=DP):: x, aa, n, eta

  
CONTAINS

REAL(KIND=DP) FUNCTION bin(x)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10) 
REAL(KIND=DP), INTENT(IN):: x

  IF(x >= 0.0 ) THEN
   bin = +1.0   
  ELSE IF (x < 0.0) THEN
   bin = -1.0 
  END IF

END FUNCTION bin

SUBROUTINE treinamento(xxi,aa,eta,epoch,w)
!Rotina de treinamento para somente um padrão
IMPLICIT NONE
INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(IN):: xxi
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT):: w
REAL(KIND=DP), INTENT(IN):: aa, eta
INTEGER(KIND=SP), INTENT(IN):: epoch
INTEGER(KIND=SP):: i

!Critério de parada (BRUTAL EM 100 PELO +oo)
!IF(aa>100)THEN
!  epoch=0
! DO WHILE (aa>100)
!   epoch=epoch+1
! END DO
!END IF

DO i=1,epoch
 !Atualização do w para somente um padrão 
  IF(aa>0) THEN 
    w(i+1,i+1)=w(i,i)
  ELSE IF(aa<=0) THEN
    w(i+1,i+1)=w(i,i)+eta*xxi(i+1,i+1) 
  END IF
END DO 


END SUBROUTINE treinamento



SUBROUTINE treinamentoP(xxi,aa,eta,epoch,pp)
!Rotina de treinamento para somente um padrão
IMPLICIT NONE
INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(IN):: xxi
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:), INTENT(INOUT):: pp
REAL(KIND=DP), INTENT(IN):: aa, eta
INTEGER(KIND=SP), INTENT(IN):: epoch
INTEGER(KIND=SP):: i


DO i=1,epoch
 !Atualização do w para somente um padrão 
  IF(aa>0) THEN 
    pp(i+1)=pp(i)
  ELSE IF(aa<=0) THEN
    pp(i+1)=pp(i)+eta*xxi(i+1,i+1) 
  END IF
END DO 


END SUBROUTINE treinamentoP


END MODULE activation
