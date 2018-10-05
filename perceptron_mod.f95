MODULE perceptron_mod
IMPLICIT NONE
  PUBLIC
  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)
  INTEGER(KIND=SP):: n
  REAL(KIND=DP)::inicio,final, TM, c, x, y
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: A, bt

  
CONTAINS

REAL FUNCTION Signal(x,y)
 
REAL(KIND=DP), INTENT(IN):: x
REAL(KIND=DP), INTENT(OUT):: y 

  IF(x >= 0.0 ) THEN
   y = -1   
  ELSE IF (x < 0.0) THEN
   y = +1 
  END IF

END FUNCTION Signal

 SUBROUTINE transpostaM(n,A)
  IMPLICIT NONE
  INTEGER(KIND=SP), INTENT(IN):: n  !n, dimensão da matriz
  INTEGER(KIND=SP):: i, j
  REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT):: A !A é a matriz de entrada. Ela vai ser reescrita no processo

  ALLOCATE(A(n,n))

    DO i=1,n
      DO j=1,n
        A(j,i) = A(i,j)
      ENDDO
    ENDDO

END SUBROUTINE transpostaM

SUBROUTINE transpostaV(n,At)
IMPLICIT NONE
INTEGER(KIND=SP), INTENT(IN):: n  !n, dimensão da matriz
INTEGER(KIND=SP):: i
REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT):: At !A é a matriz de entrada. Ela vai ser reescrita no processo
REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE:: A

ALLOCATE(A(1,n),At(n,1))

  DO i=1,n
      At(:,i) = A(i,:)
  ENDDO

END SUBROUTINE transpostaV

END MODULE perceptron_mod
