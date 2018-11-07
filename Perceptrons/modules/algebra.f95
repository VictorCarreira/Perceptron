MODULE algebra
IMPLICIT NONE
  PUBLIC
  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
  INTEGER(KIND=SP):: n
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: A, bt
  REAL(KIND=DP):: c,nn, tau, etaO
  
CONTAINS

REAL(KIND=DP) FUNCTION Robbins(c,nn)
!Taxa de aprendizagem calculada por Robbins and Monro(1951).
!c é uma constante e nn é o número de iterações.
 IMPLICIT NONE
 INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10) 
 REAL(KIND=DP), INTENT(IN):: c,nn

 Robbins=c/nn

END FUNCTION Robbins

REAL(KIND=DP) FUNCTION Darken(etaO,nn,tau)
!Taxa de aprendizagem calculada por Darken and Moody(1992)
!Evita que o eta dispare para valores altos de c e n.
!Onde eta0 e tau são constantes definidas pelo usuário.
!c = tau.etaO
 IMPLICIT NONE
 INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10) 
 REAL(KIND=DP), INTENT(IN):: etaO,nn,tau

 Darken=etaO/(1+(nn/tau))

END FUNCTION Darken

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

END MODULE algebra
