PROGRAM perceptron

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !Criação de um Perceptron simples para análise de uma litologia              !
  !Aluno: Victor Ribeiro Carreira                                              !
  !Este programa visa simular um perceptron de Hosenblat                       !
  !Para usar compilação com flags utilize: make                                !
  !Para usar o comando de limpeza digite: make clean                           !
  !Para usar o comando de limpeza do executável digite: rm "nome do executével"!
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MODULOS UTILIZADOS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

USE activation
USE algebra

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!! DECLARAÇÃO DAS VARIÁVEIS GLOBAIS !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IMPLICIT NONE 
  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
  INTEGER(KIND=SP):: i
  REAL(KIND=DP):: eta
  REAL(KIND=DP):: aa, bb, vi, vf, dltv
  REAL(KIND=DP),PARAMETER::cc=1.0,nnn=2.0, eetaO=1.0, taau=3.0
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: xx, w, wt   

  ALLOCATE(wt(3,3),w(3,3),xx(3,3))

  OPEN(1,FILE='outputs/saida.txt')
  
  !teste de validação. 

  CALL cpu_time(vi) 
  
  w=0.0
  xx=0.0

  !Matriz de pesos
  w(1,1)=1.0
  w(1,2)=2.0
  w(1,3)=4.0

  w(2,1)=5.0
  w(2,2)=1.0
  w(2,3)=4.0

  w(3,1)=6.0
  w(3,2)=4.0
  w(3,3)=8.0

  !sinal de entrada
  xx(1,1)=1.0
  xx(1,2)=3.0
  xx(1,3)=5.0

  xx(2,1)=2.0
  xx(2,2)=6.0
  xx(2,3)=1.0

  xx(3,1)=-13.0
  xx(3,2)=-3.0
  xx(3,3)=3.0

  WRITE(*,*)'Matriz w'
  WRITE(*,FMT=11)w

  WRITE(1,*)'Matriz w'
  WRITE(1,FMT=11)w


  wt = transpose(w)
  
  WRITE(*,*)'Matriz wt'
  WRITE(*,FMT=11)wt

  WRITE(1,*)'Matriz wt'
  WRITE(1,FMT=11)wt


  DO i=1,3
    aa = aa + dot_product(wt(:,i),xx(i,:))
  END DO

  !aa=-aa
  
  WRITE(*,*)'Domínio da função sinal'
  WRITE(*,FMT=11)aa

  bb = bin(aa) !Função sinal binária
  
  WRITE(*,*)'Imagem da função sinal'
  WRITE(*,FMT=12)bb


  ! Taxa de aprendizado por aproximação estocástica (Robbins,1958)
  
  eta = Robbins(cc,nnn)

  WRITE(*,*)'Taxa de aprendizado de Robbins'
  WRITE(*,FMT=12)eta

 ! Taxa de aprendizado por procura e convergência de Darken(1992)
  
  eta = Darken(eetaO,nnn,taau)

  WRITE(*,*)'Taxa de aprendizado de Darken'
  WRITE(*,FMT=12)eta

!Fase de Treinamento (Atualização dos pesos)

  !DO i=1,nn
  !  w(i+1)=w(i)+ etaR * Delta * x(i)
  !END DO

! Taxa de aprendizado


! Saída da rede com BIAS (viés) associado
!  DO i=1,m
!    v(i)=w(i)+b
!  ENDDO

  CALL cpu_time(vf)

  dltv = vf - vi

  PRINT*,'------------------------'
  PRINT*,'tempo de máquina=',dltv,'segundos'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!! FORMATO DOS ARQUIVOS DE SAÍDA !!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

11 FORMAT(ES12.2,2x,ES12.2,2x,ES12.2)
12 FORMAT(F12.2)

END PROGRAM perceptron