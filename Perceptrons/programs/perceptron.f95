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
  INTEGER, PARAMETER::SSP = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::DDP = SELECTED_REAL_KIND(8,10)
  INTEGER(KIND=SSP):: i, epoca
  REAL(KIND=DDP):: etaR, etaD
  REAL(KIND=DDP):: theta, Fativ , vi, vf, dltv
  REAL(KIND=DDP),PARAMETER::cc=1.0,nnn=2.0, eetaO=1.0, taau=3.0
  REAL(KIND=DDP), ALLOCATABLE, DIMENSION(:,:):: xi, omega, omegaT

  ALLOCATE(omegaT(3,3),omega(3,3),xi(3,3))

  OPEN(1,FILE='outputs/saida.txt')
  
  !teste de validação. 

  CALL cpu_time(vi) 
  
  omega=0.0
  xi=0.0

  !Matriz de pesos
  omega(1,1)=1.0
  omega(1,2)=2.0
  omega(1,3)=4.0

  omega(2,1)=5.0
  omega(2,2)=1.0
  omega(2,3)=4.0

  omega(3,1)=6.0
  omega(3,2)=4.0
  omega(3,3)=8.0

  !sinal de entrada
  xi(1,1)=1.0
  xi(1,2)=3.0
  xi(1,3)=5.0

  xi(2,1)=2.0
  xi(2,2)=6.0
  xi(2,3)=1.0

  xi(3,1)=-13.0
  xi(3,2)=-3.0
  xi(3,3)=3.0

  !Número de épocas
  epoca = 2

  WRITE(*,*)'Matriz w'
  WRITE(*,FMT=11)omega

  WRITE(1,*)'Matriz w'
  WRITE(1,FMT=11)omega


  omegaT = transpose(omega)
  
  WRITE(*,*)'Matriz wt'
  WRITE(*,FMT=11)omegaT

  WRITE(1,*)'Matriz wt'
  WRITE(1,FMT=11)omegaT


  DO i=1,3
    theta = theta + dot_product(omegaT(:,i),xi(i,:))
  END DO

  !aa=-aa
  
  WRITE(*,*)'Domínio da função sinal'
  WRITE(*,FMT=11)theta

  Fativ = bin(theta) !Função sinal binária
  
  WRITE(*,*)'Imagem da função sinal'
  WRITE(*,FMT=12)Fativ


  ! Taxa de aprendizado por aproximação estocástica (Robbins,1958)
  
  etaR = Robbins(cc,nnn)

  WRITE(*,*)'Taxa de aprendizado de Robbins'
  WRITE(*,FMT=12)etaR

 ! Taxa de aprendizado por procura e convergência de Darken(1992)
  
  etaD = Darken(eetaO,nnn,taau)

  WRITE(*,*)'Taxa de aprendizado de Darken'
  WRITE(*,FMT=12)etaD

!Fase de Treinamento (Atualização dos pesos)

CALL treinamento(xi,theta,etaD,epoca,omega)

WRITE(*,*)'Matriz w atualizada'
WRITE(*,FMT=11)omega

!Fase de Classificação ()

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
