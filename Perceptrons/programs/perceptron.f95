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
  REAL(KIND=DDP), ALLOCATABLE, DIMENSION(:)::p
  REAL(KIND=DDP), ALLOCATABLE, DIMENSION(:,:):: xi1, xi2, omega, omegaT, csi

  ALLOCATE(omegaT(8,4),omega(4,8),xi1(4,8),xi2(4,8),csi(4,8),p(8))

  OPEN(1,FILE='outputs/saida.txt')
  
  !teste de validação. 

  CALL cpu_time(vi) 
  
  !Zera variáveis
  omega=0.0d00 !Peso sináptico
  xi1=0.0d00  !Treinamento de um único tipo litológico
  xi2=0.0d00  !Treinamento de multi-padrões litológicos

  !Matriz de pesos
  omega(1,1)=1.0d00
  omega(2,1)=1.0d00
  omega(3,1)=1.0d00
  omega(4,1)=1.0d00

  omega(1,2)=1.0d00
  omega(2,2)=1.0d00
  omega(3,2)=1.0d00
  omega(4,2)=1.0d00

  omega(1,3)=1.0d00
  omega(2,3)=1.0d00
  omega(3,3)=1.0d00
  omega(4,3)=1.0d00
  
  omega(1,4)=1.0d00
  omega(2,4)=1.0d00
  omega(3,4)=1.0d00
  omega(4,4)=1.0d00

  omega(1,5)=1.0d00
  omega(2,5)=1.0d00
  omega(3,5)=1.0d00
  omega(4,5)=1.0d00

  omega(1,6)=1.0d00
  omega(2,6)=1.0d00
  omega(3,6)=1.0d00
  omega(4,6)=1.0d00

  omega(1,7)=1.0d00
  omega(2,7)=1.0d00
  omega(3,7)=1.0d00
  omega(4,7)=1.0d00

  omega(1,8)=1.0d00
  omega(2,8)=1.0d00
  omega(3,8)=1.0d00
  omega(4,8)=1.0d00

 !Vetor pesos:

  DO i=1,8
    p(i)=1.0d00
  END DO 

!Sinal de entrada para treinamento classe 1 (Folhelho)
  
  !Amazonas (Colunas:densidade,gama,resistividade,velocidade)
  !Classe 1 folhelho
  !                                _   
  xi1(1,1)=2.57d00  !densidade      |
  xi1(2,1)=9.69d01  !gama           |===> primeira linha da matriz xi
  xi1(3,1)=9.29d03  !resistividade  |
  xi1(4,1)=2.87d00  !velocidade    _|

  xi1(1,2)=2.52d00  
  xi1(2,2)=1.05d02
  xi1(3,2)=1.02d04
  xi1(4,2)=2.84d00

  xi1(1,3)=2.55d00
  xi1(2,3)=9.51d01
  xi1(3,3)=9.93d03
  xi1(4,3)=2.96d00

  xi1(1,4)=2.53d00
  xi1(2,4)=9.71d01
  xi1(3,4)=9.93d03
  xi1(4,4)=3.15d00

  xi1(1,5)=2.58d00 
  xi1(2,5)=9.49d01
  xi1(3,5)=1.03d04
  xi1(4,5)=2.96d00

  xi1(1,6)=2.51d00
  xi1(2,6)=9.94d01
  xi1(3,6)=9.94d03
  xi1(4,6)=2.83d00

  xi1(1,7)=2.51d00
  xi1(2,7)=1.06d02  
  xi1(3,7)=1.04d04
  xi1(4,7)=3.01d00

  xi1(1,8)=2.58d00
  xi1(2,8)=1.07d02
  xi1(3,8)=1.01d04
  xi1(4,8)=3.14d00

!Sinal de entrada para treinamento Classe 2 (multi-rochas)
!                                  _
  xi2(1,1)=2.70d00  !densidade      |
  xi2(2,1)=8.16d00  !gama           |===> primeira linha da matriz xi2 (dolomita)
  xi2(3,1)=3.54d03  !resistividade  |
  xi2(4,1)=6.39d00  !velocidade    _|
!                                  _
  xi2(1,2)=2.92d00  !densidade      |
  xi2(2,2)=3.07d01  !gama           |===> segunda linha da matriz xi2 (diabasio)
  xi2(3,2)=1.58d08  !resistividade  |
  xi2(4,2)=5.56d00  !velocidade    _|
!                                  _                    
  xi2(1,3)=2.30d00  !densidade      |
  xi2(2,3)=9.45d01  !gama           |===> terceira linha da matiz xi2 (conglomerado)
  xi2(3,3)=5.85d03  !resistividade  |
  xi2(4,3)=1.99d00  !velocidade    _|
!                                  _
  xi2(1,4)=2.83d00  !densidade      |
  xi2(2,4)=6.86d-01 !gama           |===> quarta linha da matriz xi2 (embasamento)
  xi2(3,4)=1.40d06  !resistividade  |
  xi2(4,4)=5.26d00  !velocidade    _|
!                                  _
  xi2(1,5)=2.26d00  !densidade      | 
  xi2(2,5)=9.32d01  !gama           |===> quinta linha da matriz xi2 (cong+emb1)
  xi2(3,5)=1.26d04  !resistividade  |
  xi2(4,5)=2.10d00  !velocidade    _|
!                                  _
  xi2(1,6)=2.47d00  !densidade      |
  xi2(2,6)=7.36d01  !gama           |===> sexta linha da matriz xi2 (cong+emb2)
  xi2(3,6)=3.00d05  !resistividade  |
  xi2(4,6)=2.60d00  !velocidade    _|
!                                  _
  xi2(1,7)=2.53d00  !densidade      |
  xi2(2,7)=5.33d01  !gama           |===> sétima linha da matriz xi2 (cong+emb3)
  xi2(3,7)=6.11d05  !resisitividade |
  xi2(4,7)=3.27d00  !velocidade    _|              
!                                  _
  xi2(1,8)=2.66d00  !densidade      | 
  xi2(2,8)=2.42d01  !gama           |===> oitava linha da matriz xi2 (cong+emb4)
  xi2(3,8)=1.06d06  !resistividade  | 
  xi2(4,8)=4.14d00  !velocidade    _|
 

!Número de épocas
  epoca = 3 !Precisa ser menor do que a dimensão da matriz omega


  WRITE(*,*)'Entrada do treinamento subclasse 1 (Folhelho):'
  WRITE(*,FMT=13)xi1


  WRITE(*,*)'Entrada do treinamento subclasse 2 (Multi-rochas):'
  WRITE(*,FMT=13)xi2 
   

  !WRITE(*,*)'Matriz w:'
  !WRITE(*,FMT=13)omega

  !WRITE(1,*)'Matriz w:'
  !WRITE(1,FMT=13)omega


  omegaT = transpose(omega)
  
  !WRITE(*,*)'Matriz wt'
  !WRITE(*,FMT=11)omegaT

  !WRITE(1,*)'Matriz wt'
  !WRITE(1,FMT=11)omegaT


  !DO i=1,8
  !  theta = theta + dot_product(omegaT(i,:),xi1(:,i))
  !END DO

  DO i=1,8
    theta = theta + p(i)*xi1(1,i)
  END DO 
  
  WRITE(*,*)'Domínio da função sinal'
  WRITE(*,FMT=11)theta

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

CALL treinamento(xi1,theta,etaD,epoca,omega)

CALL treinamentoP(xi1,theta,etaD,epoca,p)

  !WRITE(*,*)'Matriz w atualizada'
  !WRITE(*,FMT=13)omega

  WRITE(*,*)'Vetor p atualizado'
  WRITE(*,FMT=13)p

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

 !omegaT=transpose(omega)

 !DO i=1,8
 !  theta = theta + dot_product(omegaT(i,:),csi(:,i))
 !END DO

  DO i=1,8
    theta = theta + p(i)*csi(1,i)
  END DO 
    

!Classificação Binária

Fativ=bin(theta)

WRITE(*,*)'Classificação da rede'
WRITE(*,FMT=*)Fativ  

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
!14 FORMAT(D12.12)

END PROGRAM perceptron

