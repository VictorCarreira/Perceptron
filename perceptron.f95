PROGRAM LAmodule

IMPLICIT NONE
!REAL(KIND=8), INTENT(IN), DIMENSION(:,:)::A, B
!REAL(KIND=8), INTENT(OUT), DIMENSION(:,:)::E

  CONTAINS

! Esta subrotina faz a multiplicação de duas matrizes quadradas (A * B)
!
! Parâmetros:
! A e B Matrizes de entrada
! C Matriz que armazenará o produto de a por b
! n Dimensão da matriz (apenas matrizes quadradas)

SUBROUTINE produto_matricial (A, B, C)

IMPLICIT NONE

INTEGER :: n, i, j, k
REAL, DIMENSION(:,:),ALLOCATABLE, INTENT(IN):: A, B
REAL, DIMENSION(:,:),ALLOCATABLE, INTENT(OUT):: C

n=size(A, dim=1)

C=0.0                                                                           !zera os elementos da matriz.
  do i=1,n
    do j=1,n
      do k=1,n
        C(i,j) = C(i,j)+A(i,k)∗B(k,j)                                           !Realiza o produto matricial conforme
      end do
    end do
  end do

END SUBROUTINE produto_matricial


!Esta subrotina cria a transposta de uma matriz
!
!Parâmetros:
!A Matriz de entrada
!n Dimensão da matriz (apenas matrizes quadradas)
!
!AVISO: A matriz a será modificada no processo

SUBROUTINE matriz_transposta (At, n)

IMPLICIT NONE

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


ENDPROGRAM LAmodule
