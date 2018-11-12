PROGRAM perceptron

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !Criação de um Perceptron simples para análise de uma litologia              !
  !Aluno: Victor Ribeiro Carreira                                              !
  !Este programa visa simular um perceptron de Hosenblat                       !
  !Para usar compilação com flags utilize: make                                !
  !Para usar o comando de limpeza digite: make clean                           !
  !Para usar o comando de limpeza do executável digite: rm "nome do executável"!
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
  REAL(KIND=DDP), ALLOCATABLE, DIMENSION(:,:):: xi, omega, omegaT, csi

  ALLOCATE(omegaT(8,4),omega(4,8),xi(4,8),csi(4,8))

  OPEN(1,FILE='outputs/saida.txt')
  
  !teste de validação. 

  CALL cpu_time(vi) 
  
  omega=0.0d00
  xi=0.0d00

  !Matriz de pesos
  omega(1,1)=1.0d00
  omega(2,1)=2.0d00
  omega(3,1)=1.8d00
  omega(4,1)=0.5d00

  omega(1,2)=7.0d00
  omega(2,2)=2.5d00
  omega(3,2)=1.0d00
  omega(4,2)=0.8d00

  omega(1,3)=0.2d00
  omega(2,3)=0.6d00
  omega(3,3)=3.0d00
  omega(4,3)=1.0d00
  
  omega(1,4)=1.2d00
  omega(2,4)=5.9d00
  omega(3,4)=0.1d00
  omega(4,4)=2.0d00

  omega(1,5)=1.0d00
  omega(2,5)=0.2d00
  omega(3,5)=0.8d00
  omega(4,5)=0.2d00

  omega(1,6)=5.0d00
  omega(2,6)=2.0d00
  omega(3,6)=1.0d00
  omega(4,6)=0.2d00

  omega(1,7)=1.0d00
  omega(2,7)=1.1d00
  omega(3,7)=7.0d00
  omega(4,7)=4.0d00

  omega(1,8)=8.0d00
  omega(2,8)=1.2d00
  omega(3,8)=5.0d00
  omega(4,8)=2.0d00


  !sinal de entrada
  
  !Amazonas (Colunas:densidade,gama,resistividade,velocidade)
  !Classe 1 folhelho

  xi(1,1)=2.57d00
  xi(2,1)=9.69d01
  xi(3,1)=9.29d03
  xi(4,1)=2.87d00

  xi(1,2)=2.52d00
  xi(2,2)=1.05d02
  xi(3,2)=1.02d04
  xi(4,2)=2.84d00

  xi(1,3)=2.55d00
  xi(2,3)=9.51d01
  xi(3,3)=9.93d03
  xi(4,3)=2.96d00

  xi(1,4)=2.53d00
  xi(2,4)=9.71d01
  xi(3,4)=9.93d03
  xi(4,4)=3.15d00

  !Classe 2: Diabásio
  xi(1,5)=2.84d00 
  xi(2,5)=3.23d01
  xi(3,5)=1.59d08
  xi(4,5)=5.66d00

  xi(1,6)=2.87d00
  xi(2,6)=3.21d01
  xi(3,6)=1.53d08
  xi(4,6)=5.44d00

  xi(1,7)=2.90d00
  xi(2,7)=2.95d01
  xi(3,7)=1.43d08
  xi(4,7)=5.12d00

  xi(1,8)=2.92d00
  xi(2,8)=3.18d01
  xi(3,8)=1.51d08
  xi(4,8)=6.10d00

  !Número de épocas
  epoca = 3 !Precisa ser menor do que a dimensão da matriz omega

  WRITE(*,*)'Matriz w'
  WRITE(*,FMT=13)omega

  WRITE(1,*)'Matriz w'
  WRITE(1,FMT=13)omega

  WRITE(*,*)'Sinal de treinamento (União de todas as classes)'
  WRITE(*,FMT=13)xi

  omegaT = transpose(omega)
  
  WRITE(*,*)'Matriz wt'
  WRITE(*,FMT=11)omegaT

  WRITE(1,*)'Matriz wt'
  WRITE(1,FMT=11)omegaT


  DO i=1,8
    theta = theta + dot_product(omegaT(i,:),xi(:,i))
  END DO

  !aa=-aa
  
  WRITE(*,*)'Domínio da função sinal'
  WRITE(*,FMT=*)theta

  Fativ = bin(theta) !Função sinal degrau
  
  WRITE(*,*)'Imagem da função sinal'
  WRITE(*,FMT=*)Fativ


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
  WRITE(*,FMT=13)omega

!Fase de Classificação (Usa a matriz w atualizada)

!Input (Conjunto de classificação)

csi=0.0d00

!Classe folhelho
csi(1,1)=2.52d00
csi(2,1)=9.83d01
csi(3,1)=1.07d04
csi(4,1)=3.06d00

csi(1,2)=2.63d00
csi(2,2)=9.85d01
csi(3,2)=1.07d04
csi(4,2)=3.39d00

csi(1,3)=2.57d00
csi(2,3)=9.28d01
csi(3,3)=9.62d03
csi(4,3)=2.91d00

csi(1,4)=2.56d00
csi(2,4)=9.63d01
csi(3,4)=9.64d03
csi(4,4)=2.78d00

!Classe Diabásio

csi(1,5)=3.00d00
csi(2,5)=3.01d01
csi(3,5)=1.52d08
csi(4,5)=5.19d00

csi(1,6)=2.94d00
csi(2,6)=2.81d01
csi(3,6)=1.44d08
csi(4,6)=5.65d00

csi(1,7)=2.93d00
csi(2,7)=3.19d01
csi(3,7)=1.27d08
csi(4,7)=5.51d00

csi(1,8)=2.94d00
csi(2,8)=3.18d01
csi(3,8)=1.56d08
csi(4,8)=5.58d00

WRITE(*,*)'Sinal de classificação (união de todas as classes)'
WRITE(*,FMT=13)csi

 omegaT=transpose(omega)

 DO i=1,8
   theta = theta + dot_product(omegaT(i,:),csi(:,i))
 END DO

!Classificação Binária

Fativ=bin(theta)

  CALL cpu_time(vf)

  dltv = vf - vi

  PRINT*,'------------------------'
  PRINT*,'tempo de máquina=',dltv,'segundos'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!! FORMATO DOS ARQUIVOS DE SAÍDA !!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

11 FORMAT(8(ES12.2,1x))
12 FORMAT(F12.2)
13 FORMAT(4(ES12.2,1x))
14 FORMAT(D12.12)

END PROGRAM perceptron

