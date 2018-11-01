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
  INTEGER, PARAMETER::SSP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DDP = SELECTED_REAL_KIND(12,100)
  INTEGER(KIND=SSP):: i
  REAL(KIND=DDP):: etaR, etaD 
  REAL(KIND=DDP):: aa, bb, vi, vf, dltv
  REAL(KIND=DDP),PARAMETER::cc=1.0,nnn=2.0, eetaO=1.0, taau=3.0
  REAL(KIND=DDP), ALLOCATABLE, DIMENSION(:,:):: xx, w, wt   

  ALLOCATE(wt(3,3),w(3,3),xx(3,3))
  
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
  xx(1,1)=-46.0
  xx(1,2)=56.0
  xx(1,3)=63.0

  xx(2,1)=-65.0
  xx(2,2)=2.0
  xx(2,3)=43.0

  xx(3,1)=50.0
  xx(3,2)=-83.0
  xx(3,3)=78.0

  WRITE(*,*)'Matriz w'
  WRITE(*,FMT=11)w

  wt = transpose(w)
  
  WRITE(*,*)'Matriz wt'
  WRITE(*,FMT=11)wt

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
  
  etaR = Robbins(cc,nnn)

  WRITE(*,*)'Taxa de aprendizado de Robbins'
  WRITE(*,FMT=12)etaR

 ! Taxa de aprendizado por procura e convergência de Darken(1992)
  
  etaD = Darken(eetaO,nnn,taau)

  WRITE(*,*)'Taxa de aprendizado de Darken'
  WRITE(*,FMT=12)etaD

!Atualização dos pesos sinápticos

!  DO i=1,m
!  w(i+1)=w(i)+ eta * Delta * x(i)
!  ENDDO

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