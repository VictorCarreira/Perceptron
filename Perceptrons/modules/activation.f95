MODULE activation
IMPLICIT NONE
  PUBLIC
  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
  INTEGER(KIND=SP):: epoch  
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:)::pp
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: xxi,xxi1,xxi2, w
  REAL(KIND=DP):: x, aa, n, eta, a1, a2, SC1, SC2
 

CONTAINS

!------------------------------------------------------
REAL(KIND=DP) FUNCTION degrau(x)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10) 
REAL(KIND=DP), INTENT(IN):: x

  IF(x >= 0.0 ) THEN
   degrau = +1.0   
  ELSE IF (x < 0.0) THEN
   degrau = -1.0 
  END IF

END FUNCTION degrau

!-------------------------------------------------------

SUBROUTINE pesos(m,n,x,peso)
!Define valor constante para a matriz de pesos, 
!bem como a sua dimensão do vetor de pesos. Caso
!"n" seja 1 omega assumirá posição de vetor coluna.
!"m" é o número de linhas de omega. e "x" é o preen-
!chimento de valores contantes da matriz.
IMPLICIT NONE 
INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
INTEGER(KIND=SP), INTENT(IN)::m, n
REAL(KIND=DP), INTENT(IN)::x
REAL(KIND=DP), DIMENSION(m,n), INTENT(INOUT):: peso
INTEGER(KIND=SP):: i,j 


  DO i=1,m
   DO j=1,n
     peso(i,j)=x
   END DO 
  END DO 

END SUBROUTINE pesos

!-------------------------------------------------------

SUBROUTINE synaptic(xxi1,xxi2,eta,epoch,w)
IMPLICIT NONE
INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
INTEGER(KIND=SP), INTENT(IN):: epoch
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(IN):: xxi1,xxi2
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT):: w
REAL(KIND=DP), INTENT(IN):: eta
REAL(KIND=DP), DIMENSION(1,8):: wT
INTEGER(KIND=SP):: i,j,k
REAL(KIND=DP)::a1, a2, SC1, SC2

a1=0.0
a2=0.0
SC1=0.0
SC2=0.0
wT=0.0

wT=transpose(w)

DO j=1,4
 DO i=1,8
  a1 = a1 + wT(1,i)*xxi1(i,j)
  a2 = a2 + wT(1,i)*xxi2(i,j)
 END DO 
END DO 


SC1= degrau(a1)
SC2= degrau(a2)


DO k=1,epoch !épocas
 DO i=1,8 !número de linhas da matriz de entrada
  DO j=1,4 ! número de propriedades da matriz de entrada 

    IF(a1 < 0d0 .or. SC1 >= 0d0)THEN 
     w(i,1)=w(i,1)+eta*xxi1(i,j) 
    ELSE
     w(i,1)=w(i,1)  
    END IF

    IF(a2 >= 0d0 .or. SC2 < 0d0)THEN
     w(i,1)=w(i,1)-eta*xxi2(i,j) 
    ELSE
     w(i,1)=w(i,1)  
    END IF

  END DO 
 END DO
END DO  

END SUBROUTINE synaptic


SUBROUTINE classification(dado,w,i,aval)
!Faz uso matriz omega ajustada em associação com o sinal 
!de entradada na fase de classificação
IMPLICIT NONE
INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(IN)::dado
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(IN) :: w
CHARACTER(LEN=30),INTENT(OUT)::aval
INTEGER(KIND=SP), INTENT(OUT):: i
REAL(KIND=DP), DIMENSION(1,8):: wT
INTEGER(KIND=SP):: j
REAL(KIND=DP)::a, c

 wT= 0.0d00
 wT = transpose(w)


 DO i=1,8
   DO j=1,4
     a=a+wT(1,i)*dado(i,j)
   END DO

  c=degrau(a)


  IF(a>=0d0)THEN
    aval= 'Pertence a subclasse'
   ELSE
    aval= 'Não pertence a subclasse'
  END IF

  WRITE(*,*)i, 'Avaliação', '', aval

 END DO

END SUBROUTINE classification 


!------SUBROTINAS NÃO UTILIZADAS---!

SUBROUTINE treinamento(xxi,aa,eta,epoch,w)
!Rotina de treinamento para somente um padrão sendo W uma matriz de pesos
!Este conceito pode ser aproveitado no futuro para rede multicamadas. 
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

END MODULE activation
