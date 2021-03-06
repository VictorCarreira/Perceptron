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

  IF(x >= 0.0d0 ) THEN
   degrau = +1.0d0   
  ELSE IF (x < 0.0d0) THEN
   degrau = -1.0d0 
  END IF

END FUNCTION degrau

!-------------------------------------------------------------------------

SUBROUTINE Etraining(xi1, xi2) 
!Esta subrotina visa automatizar a entrada de dados do perceptron.
!As duas primeiras informações a serem fornecidas são as matrizes
!que armazenam as informações de propriedades físicas das rochas. 
!Estas entram vazias e retornam preenchidas.

IMPLICIT NONE
INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
INTEGER(KIND=SP):: i
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT):: xi1, xi2
REAL(KIND=DP)::a1, a2, a3, a4
REAL(KIND=DP):: icod, iprof
CHARACTER(LEN=80):: cab
CHARACTER(LEN=20):: linha(4)

OPEN(UNIT=1, FILE='inputs/folhelho.txt')

ALLOCATE(xi1(8,4),xi2(8,4))

!Zera variáveis
xi1=0.0d0 !Treinamento de um único tipo litológico (subclasse1) 
xi2=0.0d0 !Treinamento de multipadrões litológicos (subclasse2) 

READ(1,FMT=15) cab 
READ(1,FMT=15) cab 

 DO i =1,8
  READ(1,FMT=*) linha(1), icod, iprof, a1, a2, a3, a4
    xi1(i,1)=a1
    xi1(i,2)=a2
    xi1(i,3)=a3
    xi1(i,4)=a4
  END DO 


READ(1,FMT=15) cab 
READ(1,FMT=15) cab 

 DO i =1,8
  READ(1,FMT=*) linha(2), icod, iprof, a1, a2, a3, a4
    xi2(i,1)=a1
    xi2(i,2)=a2
    xi2(i,3)=a3
    xi2(i,4)=a4
 END DO 

DO i=1,8
  WRITE(6,FMT=16) xi1(i,1), xi1(i,2), xi1(i,3), xi1(i,4)
END DO 
  WRITE(6,FMT=*) '--------------------------------------'
DO i=1,8
  WRITE(6,FMT=16) xi2(i,1), xi2(i,2), xi2(i,3), xi2(i,4)
END DO 


!FORMATOS UTILIZADOS
15 FORMAT(A71)
16 FORMAT(4(ES9.2E2,2x))

END SUBROUTINE Etraining 


!-------------------------------------------------------

SUBROUTINE Eclassification(csi,nd)
IMPLICIT NONE 
INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
INTEGER(KIND=SP):: i, icod, iprof!,ilinha
INTEGER(KIND=SP),INTENT(OUT):: nd
CHARACTER(LEN=80):: cab, litologia
REAL(KIND=DP)::a1, a2, a3, a4
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT)::csi

  OPEN(3,FILE='inputs/dados_sint_c1.txt')

 !Leitura do arquivo de dados a ser classificado
 
  READ(3,FMT=15) cab !Cabeçalho
   WRITE(6,FMT=15) cab
  
  READ(3,FMT=15) cab !Espaço em branco em baixo do cabeçalho
   WRITE(6,FMT=15) cab
  
  i=1
   DO WHILE (.TRUE.) ! Lê entrada até que existam linhas 
      READ(3,*,END=10) litologia, icod, iprof, a1, a2, a3, a4
       i=i+1
       !csi(i,1)=a1
       !csi(i,1)=a1
       !csi(i,1)=a1
       !csi(i,1)=a1
   END DO
10 CONTINUE 
   !CLOSE(3)  

   nd=i-1 !linhas do arquivo de entrada e dimensão da matriz csi

    WRITE(6,FMT=*)"Quantidade de dados a serem classificados=", nd

  ALLOCATE(csi(nd,4)) !Dimensiona csi com a dimensão do dado 

   !nd=ilinha

 !DO i =1,nd  ! Armazena os dados de propriedades na matriz csi
   !READ(3,FMT=*) litologia, icod, iprof, a1, a2, a3, a4
    !csi(i,1)=a1
    !csi(i,2)=a2
    !csi(i,3)=a3
    !csi(i,4)=a4
 !END DO 

DO i=1,nd
  WRITE(6,FMT=16) csi(i,1), csi(i,2), csi(i,3), csi(i,4)
END DO 
 
!FORMATOS UTILIZADOS
15 FORMAT(A71)
16 FORMAT(4(ES9.2E2,2x))


END SUBROUTINE Eclassification


!-------------------------------------------------------

SUBROUTINE weight(m,n,x,peso)
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


  DO i=1,m ! Linhas. No percetron é sempre 1. 
   DO j=1,n! número de propriedades 
     peso(i,j)=x
   END DO 
  END DO 

END SUBROUTINE weight

!-------------------------------------------------------

!SUBROUTINE synaptic(xxi1,xxi2,eta,epoch,w) !sem critério de parada
SUBROUTINE synaptic(xxi1,xxi2,eta,w) !com ritério de parada
IMPLICIT NONE
INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
INTEGER(KIND=DP):: epoch
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(IN):: xxi1,xxi2
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT):: w
REAL(KIND=DP), INTENT(IN):: eta
INTEGER(KIND=SP):: i,j!,k
REAL(KIND=DP)::a1, a2, SC1, SC2

a1=0.0d0
a2=0.0d0
SC1=0.0d0
SC2=0.0d0
epoch=0

DO i=1,8
 DO j=1,4 !chute inicial
  a1 = a1 + w(j,1)*xxi1(i,j)
  a2 = a2 + w(j,1)*xxi2(i,j)
 END DO 
END DO 

!DO k=1,epoch !épocas

DO WHILE (a1 < 0d0 .or. a2 > 0d0)

epoch=epoch+1 ! Conta quantas épocas são necessárias para finalizar o treinamento (automatização)

DO i=1,8

SC1= degrau(a1)
SC2= degrau(a2)



 !DO i=1,8 !número de linhas da matriz de entrada
  DO j=1,4 ! número de propriedades da matriz de entrada 

    IF(SC1 < 0d0)THEN 
     w(j,1)=w(j,1)+eta*xxi1(i,j) 
    ELSE
     w(j,1)=w(j,1)  
    END IF

    IF(SC2 > 0d0)THEN
     w(j,1)=w(j,1)-eta*xxi2(i,j) 
    ELSE
     w(j,1)=w(j,1)  
    END IF

  END DO 

a1=0d0
a2=0d0

 DO j=1,4 !atualização dos a's
  a1 = a1 + w(j,1)*xxi1(i,j)
  a2 = a2 + w(j,1)*xxi2(i,j)
 END DO 

END DO !laço das amostras

!print*,'------------------'
!print*,'a1=',a1
!print*,'a2=',a2

END DO !Final do laço do treinamento 

print*,'número de épocas=',epoch

END SUBROUTINE synaptic


SUBROUTINE classification(dado,w,nd,i,aval)
!Faz uso matriz omega ajustada em associação com o sinal 
!de entradada na fase de classificação
IMPLICIT NONE
INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(IN)::dado
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(IN) :: w
CHARACTER(LEN=30),INTENT(OUT)::aval
INTEGER(KIND=SP), INTENT(OUT):: i
INTEGER(KIND=SP), INTENT(IN)::nd
!REAL(KIND=DP), DIMENSION(1,4):: wT
INTEGER(KIND=SP):: j
REAL(KIND=DP)::a, c

 a=0.0d0
 !wT= 0.0d00
 !wT = transpose(w)

 DO i=1,nd
   DO j=1,4
     a=a+w(j,1)*dado(i,j)
   END DO

  c=degrau(a)


  IF(c > 0d0)THEN
    aval= 'pertence a subclasse'
   ELSE
    aval= 'não pertence a subclasse'
  END IF

  WRITE(*,*)i, 'Avaliação: ', ' ', aval

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
