MODULE algebra
IMPLICIT NONE
  PUBLIC
  INTEGER, PARAMETER::PS = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::PD = SELECTED_REAL_KIND(8,10)
  INTEGER(KIND=PS):: n
  REAL(KIND=PD), ALLOCATABLE, DIMENSION(:,:):: A, bt
  REAL(KIND=PD):: c,nn, tau, etaO
  
CONTAINS

REAL(KIND=PD) FUNCTION Robbins(c,nn)
!Taxa de aprendizagem calculada por Robbins and Monro(1951).
!c é uma constante e nn é o número de iterações.
 IMPLICIT NONE
  !INTEGER, PARAMETER::PS = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::PD = SELECTED_REAL_KIND(8,10) 
  INTEGER(KIND=PD), INTENT(IN):: nn
  REAL(KIND=PD), INTENT(IN):: c
  REAL(KIND=PD):: nnn
 
  nnn = DFLOAT(nn) ! converte o inteiro em um real de dupla precisão

  Robbins=c/nnn

END FUNCTION Robbins

REAL(KIND=PD) FUNCTION Darken(etaO,nn,tau)
!Taxa de aprendizagem calculada por Darken and Moody(1992)
!Evita que o eta dispare para valores altos de c e n.
!Onde eta0 e tau são constantes definidas pelo usuário.
!c = tau.etaO
 IMPLICIT NONE
  INTEGER, PARAMETER::PD = SELECTED_REAL_KIND(8,10) 
  !INTEGER, PARAMETER::PS = SELECTED_INT_KIND(r=4)
  INTEGER(KIND=PD), INTENT(IN):: nn
  REAL(KIND=PD), INTENT(IN):: etaO,tau
  REAL(KIND=PD):: nnn
 
  nnn = DFLOAT(nn)


 Darken=etaO/(1+(nnn/tau))

END FUNCTION Darken

 SUBROUTINE transposta(n,m,A)
  !Nesta subrotina é preciso fornecer o número de linhas totais n.
  !E o número de colunas totais m.

  IMPLICIT NONE
  INTEGER(KIND=PS), INTENT(IN):: n,m  !n, linhas e m, colunas
  INTEGER(KIND=PS):: i, j
  REAL(KIND=PD), DIMENSION(n,m), INTENT(INOUT):: A !A é a matriz de entrada. Ela vai ser reescrita no processo


    DO i=1,n
      DO j=1,m
        A(j,i) = A(i,j)
      END DO
    END DO

END SUBROUTINE transposta


END MODULE algebra
