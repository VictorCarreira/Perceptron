PROGRAM perceprton

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !Criação da subrotina do cálculo de distância por Mahalanobis                !
  !Orientador: Cosme Ferreira da Ponte Neto                                    !
  !Aluno: Victor Ribeiro Carreira                                              !
  !Este programa visa simular um perceptron de Hosenblat                       !
  !Para usar compilação com flags utilize:                                     !
  !gfortran -fbounds-check -fbacktrace -Wall -Wextra -pedantic                 !
  !"pasta/subpasta/nomedopragrama.f95" -o nomedoexecutável                     !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!



 IMPLICIT NONE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!! DECLARAÇÃO DAS VARIÁVEIS GLOBAIS !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)

  INTEGER(KIND=SP):: i,m=10
  REAL(KIND=DP)::b=10.0, eta, c, n, Delta
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:):: w, x, v


  ALLOCATE(w(2), x(2), v(2))

  ! Taxa de aprendizado por aproximação estocástica (Robin,1958)

  eta = c/n

!Atualização dos pesos sinápticos

  DO i=1,m
  w(i+1)=w(i)+ eta * Delta * x(i)
  END DO

! Taxa de aprendizado



! Saída da rede com BIAS (viés) associado
  DO i=1,m
    v(i)=w(i)+b
  ENDDO

CONTAINS

! Esta subrotina faz a multiplicação de duas matrizes quadradas (A * B)
!
! Parâmetros:
! A e B Matrizes de entrada
! C Matriz que armazenará o produto de a por b
! n Dimensão da matriz (apenas matrizes quadradas)

!SUBROUTINE produto_matricial (A, B, C)

!IMPLICIT NONE

!INTEGER :: n, i, j, k
!REAL, DIMENSION(:,:),ALLOCATABLE, INTENT(IN):: A, B
!REAL, DIMENSION(:,:),ALLOCATABLE, INTENT(OUT):: C

!n=size(A, dim=1)
!n=size(C, dim=1)

!C=0.0             !zera os elementos da matriz.
!  do i=1,n
!    do j=1,n
!      do k=1,n
!        C(i,j) = C(i,j)+ A(i,k)∗B(k,j)  !Realiza o produto matricial conforme
!      end do
!    end do
!  end do

!END SUBROUTINE produto_matricial


!Esta subrotina cria a transposta de uma matriz
!
!Parâmetros:
!A Matriz de entrada
!n Dimensão da matriz (apenas matrizes quadradas)
!
!AVISO: A matriz a será modificada no processo

SUBROUTINE matriz_transposta (At, n)

INTEGER:: n,i,j
REAL:: At(n,n), temp

  do i=1,n
    do j=1,n
      if (i<j) then
        temp = At(i,j)
        At(i,j) = At(j,i)                                                         !Troca o índice i com o índice j
        At(j,i) = temp
      end if
    end do
  end do

END SUBROUTINE matriz_transposta


ENDPROGRAM perceprton
