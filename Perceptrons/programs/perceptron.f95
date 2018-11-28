PROGRAM perceptron

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !Criação de um Perceptron simples para análise de uma litologia              !
  !Aluno: Victor Ribeiro Carreira                                              !
  !Este programa visa simular um perceptron de Hosenblat                       !
  !Para usar compilação com flags utilize: make                                !
  !Para usar o comando de limpeza digite: make clean                           !
  !Para usar o comando de limpeza do executável digite: rm "nome do executável"!
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MODULOS UTILIZADOS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

USE activation
USE algebra

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DECLARAÇÃO DAS VARIÁVEIS GLOBAIS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IMPLICIT NONE 
  INTEGER, PARAMETER::SSP = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::DDP = SELECTED_REAL_KIND(8,10)
  INTEGER(KIND=SSP):: i, j
  INTEGER(KIND=SSP)::epoca
  REAL(KIND=DDP):: etaR, etaD
  REAL(KIND=DDP):: theta1, theta2, C1, C2, vi, vf, dltv
  REAL(KIND=DDP),PARAMETER::cc=1.0,nnn=2.0, eetaO=1.0, taau=3.0
  !REAL(KIND=DDP), ALLOCATABLE, DIMENSION(:):: omega
  REAL(KIND=DDP), ALLOCATABLE, DIMENSION(:,:):: xi1, xi2, csi, omega, omegaT

  ALLOCATE(xi1(8,4),xi2(8,4),csi(8,4),omega(8,1),omegaT(1,8))

  OPEN(1,FILE='outputs/saida.txt')
   

  CALL cpu_time(vi) 
  
  !Zera variáveis
  omega=0.0d00 !Peso sináptico
  omegaT=0.0d0 !Peso sináptico
  xi1=0.0d00   !Treinamento de um único tipo litológico (subclasse1)
  xi2=0.0d00   !Treinamento de multi-padrões litológicos (subclasse2)
  epoca=0  !Número de ciclos de treinamento
  theta1=0.0d0
  theta1=0.0d0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ENTRADA DA REDE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! FASE DE TREINAMENTO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


 !Vetor pesos:
   
   DO i=1,8
    DO j=1,1
    omega(i,j)=1.0d00
   END DO 
  END DO 

!Sinal de entrada para treinamento classe 1 (Folhelho)
  
  !Amazonas (Colunas:densidade,gama,resistividade,velocidade)
  !Sub-classe 1 folhelho
  !                                _   
  xi1(1,1)=2.57d00  !densidade      |
  xi1(1,2)=9.69d01  !gama           |==> primeira linha da matriz xi1
  xi1(1,3)=9.29d03  !resistividade  |
  xi1(1,4)=2.87d00  !velocidade    _|
  !                                _
  xi1(2,1)=2.52d00  !densidade      |
  xi1(2,2)=1.05d02  !gama           |==> segunda linha da matriz xi1
  xi1(2,3)=1.02d04  !resistividade  |
  xi1(2,4)=2.84d00  !velocidade    _|
  !                                _
  xi1(3,1)=2.55d00  !dendidade      |
  xi1(3,2)=9.51d01  !gama           |==> terceira linha da matriz xi1 
  xi1(3,3)=9.93d03  !resistividade  | 
  xi1(3,4)=2.96d00  !velocidade    _|
  !                                _
  xi1(4,1)=2.53d00  !densidade      |
  xi1(4,2)=9.71d01  !gama           |==> quarta linha da matriz xi1
  xi1(4,3)=9.93d03  !resistividade  |
  xi1(4,4)=3.15d00  !velocidade    _|
  !                                _
  xi1(5,1)=2.58d00  !densidade      |
  xi1(5,2)=9.49d01  !gama           |==> quinta linha da matriz xi1
  xi1(5,3)=1.03d04  !resistividade  |
  xi1(5,4)=2.96d00  !velocidade    _!
  !                                _
  xi1(6,1)=2.51d00  !densidade      |
  xi1(6,2)=9.94d01  !gama           |==> sexta linha da matriz xi1
  xi1(6,3)=9.94d03  !resistividade  |
  xi1(6,4)=2.83d00  !velocidade    _|
  !                                _
  xi1(7,1)=2.51d00  !densidade      |
  xi1(7,2)=1.06d02  !gama           |==> sétima linha da matriz xi1
  xi1(7,3)=1.04d04  !resistivade    |
  xi1(7,4)=3.01d00  !velocidade    _|
  !                                _
  xi1(8,1)=2.58d00  !densidade      |
  xi1(8,2)=1.07d02  !gama           |==> oitava linha da matriz xi1
  xi1(8,3)=1.01d04  !resistivade    |
  xi1(8,4)=3.14d00  !velocidade    _|

!Sinal de entrada para treinamento sub-classe 2 (multi-rochas)
!                                  _
  xi2(1,1)=2.70d00  !densidade      |
  xi2(1,2)=8.16d00  !gama           |===> primeira linha da matriz xi2 (dolomita)
  xi2(1,3)=3.54d03  !resistividade  |
  xi2(1,4)=6.39d00  !velocidade    _|
!                                  _
  xi2(2,1)=2.92d00  !densidade      |
  xi2(2,2)=3.07d01  !gama           |===> segunda linha da matriz xi2 (diabasio)
  xi2(2,3)=1.58d08  !resistividade  |
  xi2(2,4)=5.56d00  !velocidade    _|
!                                  _                    
  xi2(3,1)=2.30d00  !densidade      |
  xi2(3,2)=9.45d01  !gama           |===> terceira linha da matiz xi2 (conglomerado)
  xi2(3,3)=5.85d03  !resistividade  |
  xi2(3,4)=1.99d00  !velocidade    _|
!                                  _
  xi2(4,1)=2.83d00  !densidade      |
  xi2(4,2)=6.86d-01 !gama           |===> quarta linha da matriz xi2 (embasamento)
  xi2(4,3)=1.40d06  !resistividade  |
  xi2(4,4)=5.26d00  !velocidade    _|
!                                  _
  xi2(5,1)=2.26d00  !densidade      | 
  xi2(5,2)=9.32d01  !gama           |===> quinta linha da matriz xi2 (cong+emb1)
  xi2(5,3)=1.26d04  !resistividade  |
  xi2(5,4)=2.10d00  !velocidade    _|
!                                  _
  xi2(6,1)=2.47d00  !densidade      |
  xi2(6,2)=7.36d01  !gama           |===> sexta linha da matriz xi2 (cong+emb2)
  xi2(6,3)=3.00d05  !resistividade  |
  xi2(6,4)=2.60d00  !velocidade    _|
!                                  _
  xi2(7,1)=2.53d00  !densidade      |
  xi2(7,2)=5.33d01  !gama           |===> sétima linha da matriz xi2 (cong+emb3)
  xi2(7,3)=6.11d05  !resisitividade |
  xi2(7,4)=3.27d00  !velocidade    _|              
!                                  _
  xi2(8,1)=2.66d00  !densidade      | 
  xi2(8,2)=2.42d01  !gama           |===> oitava linha da matriz xi2 (cong+emb4)
  xi2(8,3)=1.06d06  !resistividade  | 
  xi2(8,4)=4.14d00  !velocidade    _|
 

!Número de épocas
  epoca = 250

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! TREINAMENTO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  WRITE(*,*)'Entrada do treinamento subclasse 1 (Folhelho):'
  WRITE(*,FMT=13)xi1


  WRITE(*,*)'Entrada do treinamento subclasse 2 (Multi-rochas):'
  WRITE(*,FMT=13)xi2 
   
  WRITE(*,*)'Vetor de pesos:'
  WRITE(*,FMT=14)omega
  
  omegaT=transpose(omega)
  
  WRITE(*,*)'Vetor de pesos tranposto:'
  WRITE(*,FMT=11)omegaT

  DO j=1,4
   DO i=1,8
     theta1 = theta1 + omegaT(1,i)*xi1(i,j)
     theta2 = theta2 + omegaT(1,i)*xi2(i,j) 
   END DO 
  END DO 
  
  WRITE(*,*)'Domínio da função sinal subclasse 1'
  WRITE(*,FMT=11)theta1

  WRITE(*,*)'Domínio da função sinal subclasse 2'
  WRITE(*,FMT=11)theta2

  C1 = bin(theta1) !Define subclasse 1
  C2 = bin(theta2) !Define subclasse 2 
  
  WRITE(*,*)'Subclasse 1'
  WRITE(*,FMT=*)C1

  WRITE(*,*)'Subclasse 2'
  WRITE(*,FMT=*)C2

  ! Taxa de aprendizado por aproximação estocástica (Robbins,1958)
  
  etaR = Robbins(cc,nnn)

  WRITE(*,*)'Taxa de aprendizado de Robbin'
  WRITE(*,FMT=12)etaR

 ! Taxa de aprendizado por procura e convergência de Darken(1992)
  
  etaD = Darken(eetaO,nnn,taau)

  WRITE(*,*)'Taxa de aprendizado de Darken'
  WRITE(*,FMT=12)etaD

!************************************************!
!************ Atualização Sináptica *************!
!************************************************!



CALL synaptic(xi1,xi2,theta1,theta2,C1,C2,etaD,epoca,omega)



  WRITE(*,*)'Vetor W atualizado'
  WRITE(*,FMT=14)omega


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Fase de Classificação !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! vetor W atualizado !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!Input (Conjunto de classificação)

csi=0.0d00


WRITE(*,*)'Sinal de classificação'
WRITE(*,FMT=13)csi

 !omegaT=transpose(omega)

 !DO i=1,8
 !  theta = theta + dot_product(omegaT(i,:),csi(:,i))
 !END DO

  !DO i=1,8
  !  theta = theta + p(i)*csi(1,i)
  !END DO 
    

!Classificação Binária

!Fativ=bin(theta)

!WRITE(*,*)'Classificação da rede'
!WRITE(*,FMT=*)Fativ  

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
14 FORMAT(ES12.2)

END PROGRAM perceptron

