MODULE sub_perceptron
IMPLICIT NONE
  PUBLIC
  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)
  INTEGER(KIND=SP):: n
  REAL(KIND=DP)::inicio,final, TM, c, x
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: A, bt

  
CONTAINS

REAL FUNCTION bin(x)
 
REAL(KIND=DP), INTENT(IN):: x

  IF(x >= 0.0 ) THEN
   bin = -1   
  ELSE IF (x < 0.0) THEN
   bin = +1 
  END IF

END FUNCTION bin

 SUBROUTINE transpostaM(n,A)
  IMPLICIT NONE
  INTEGER(KIND=SP), INTENT(IN):: n  !n, dimensão da matriz
  INTEGER(KIND=SP):: i, j
  REAL(KIND=DP), DIMENSION(n,n), INTENT(INOUT):: A !A é a matriz de entrada. Ela vai ser reescrita no processo


    DO i=1,n
      DO j=1,n
        A(j,i) = A(i,j)
      ENDDO
    ENDDO

END SUBROUTINE transpostaM

SUBROUTINE transpostaV(n,A)
IMPLICIT NONE
INTEGER(KIND=SP), INTENT(IN):: n  !n, dimensão da matriz
INTEGER(KIND=SP):: i
REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT):: A !A é a matriz de entrada. Ela vai ser reescrita no processo
!REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE:: A

!ALLOCATE(A(1,n))!,At(n,1))

  DO i=1,n
      A(:,i) = A(i,:)
  ENDDO

END SUBROUTINE transpostaV

END MODULE sub_perceptron
