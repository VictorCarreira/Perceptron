PROGRAM perceptron

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !Criação de um Perceptron simples para análise de uma litologia              !
  !Aluno: Victor Ribeiro Carreira                                              !
  !Este programa visa simular um perceptron de Hosenblat                       !
  !Para usar compilação com flags utilize: make                                !
  !Para usar o comando de limpeza digite: make clean                           !
  !Para usar o comando de limpeza do executável digite: rm "nome do executável"!
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MODULOS UTILIZADOS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

USE activation
USE algebra

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DECLARAÇÃO DAS VARIÁVEIS GLOBAIS !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IMPLICIT NONE 
  INTEGER, PARAMETER::SSP = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::DDP = SELECTED_REAL_KIND(8,10)
  INTEGER(KIND=DDP)::epoca 
  INTEGER(KIND=SSP)::rock
  REAL(KIND=DDP):: etaR, etaD, vi, vf, dltv
  REAL(KIND=DDP),PARAMETER::cc=0.1d0, eetaO=20.d0, taau=30.d0
  REAL(KIND=DDP), ALLOCATABLE, DIMENSION(:,:):: xi1, xi2, csi, omega
  CHARACTER(LEN=30):: evaluation


  ALLOCATE(csi(12,4),omega(4,1))

  OPEN(1,FILE='inputs/folhelho.txt')
  OPEN(2,FILE='outputs/saida.txt')
   

  CALL cpu_time(vi) 
  
  !Zera variáveis
  omega=0.0d00 !Peso sináptico
  !epoca=0  !Número de ciclos de treinamento


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ENTRADA DA REDE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! FASE DE TREINAMENTO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  CALL entrada(xi1, xi2)

 !Vetor pesos:

  CALL pesos(4,1,1.0d00,omega)

!Número de épocas
 ! epoca = 2009 !numero ótimo para o folhelho. Precisa fornecer quando não há critério de parada.



!************************************************!
!******* Definindo conjunto de treinamento ******!
!************************************************!

  WRITE(*,*)'Entrada do treinamento subclasse 1 (Folhelho):'
  WRITE(*,FMT=13)xi1


  WRITE(*,*)'Entrada do treinamento subclasse 2 (Multi-rochas):'
  WRITE(*,FMT=13)xi2 
   

  ! Taxa de aprendizado por aproximação estocástica (Robbins,1958)
  
  etaR = Robbins(cc,epoca)

  WRITE(*,*)'Taxa de aprendizado de Robbin'
  WRITE(*,FMT=12)etaR

 ! Taxa de aprendizado por procura e convergência de Darken(1992)
  
  etaD = Darken(eetaO,epoca,taau)

  WRITE(*,*)'Taxa de aprendizado de Darken'
  WRITE(*,FMT=12)etaD
  
  WRITE(*,*)'Vetor W'
  WRITE(*,FMT=14)omega


!************************************************!
!************ Atualização Sináptica *************!
!************************************************!



!CALL synaptic(xi1,xi2,1.0d-2,epoca,omega) ! Sem critério de parada. Precisa fornecer as épocas.
CALL synaptic(xi1,xi2,etaD,omega) !Com critério de parada. Calcula o número de épocas.




  WRITE(*,*)'Vetor W atualizado'
  WRITE(*,FMT=14)omega
    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Fase de Classificação !!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! vetor W atualizado !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!Input (Conjunto de classificação)

csi=0.0d00


!                                  _
  csi(1,1)=2.47d00  !densidade      | 
  csi(1,2)=1.02d02  !gama           |===> primeira linha da matriz csi (folhelho)
  csi(1,3)=9.93d03  !resistividade  | 
  csi(1,4)=3.19d00  !velocidade    _|
  csi(2,1)=2.53d00  !densidade      | 
  csi(2,2)=9.35d01  !gama           |===> segunda linha da matriz csi (follhelho)
  csi(2,3)=9.84d03  !resistividade  | 
  csi(2,4)=2.87d00  !velocidade    _|
  csi(3,1)=2.56d00  !densidade      | 
  csi(3,2)=1.09d02  !gama           |===> terceira linha da matriz csi (folhelho)
  csi(3,3)=9.28d03  !resistividade  | 
  csi(3,4)=3.21d00  !velocidade    _|
  csi(4,1)=2.98d00  !densidade      | 
  csi(4,2)=3.00d01  !gama           |===> quarta linha da matriz csi (diabásio)
  csi(4,3)=1.54d08  !resistividade  | 
  csi(4,4)=5.51d00  !velocidade    _|
  csi(5,1)=2.93d00  !densidade      | 
  csi(5,2)=2.80d01  !gama           |===> quinta linha da matriz csi (diabásio)
  csi(5,3)=1.56d08  !resistividade  | 
  csi(5,4)=5.20d00  !velocidade    _|
  csi(6,1)=2.70d00  !densidade      | 
  csi(6,2)=8.16d00  !gama           |===> sexta linha da matriz csi (dolomita)
  csi(6,3)=3.54d03  !resistividade  | 
  csi(6,4)=6.39d00  !velocidade    _|
  csi(7,1)=2.23d00  !densidade      | 
  csi(7,2)=8.79d01  !gama           |===> sétima linha da matriz csi (conglomerado)
  csi(7,3)=5.92d03  !resistividade  | 
  csi(7,4)=1.97d00  !velocidade    _|
  csi(8,1)=2.32d00  !densidade      | 
  csi(8,2)=9.48d01  !gama           |===> oitava linha da matriz csi (cong+emb1)
  csi(8,3)=3.62d04  !resistividade  | 
  csi(8,4)=2.06d00  !velocidade    _|
  csi(9,1)=2.42d00  !densidade      | 
  csi(9,2)=6.44d01  !gama           |===> nona linha da matriz csi (cong+emb2)
  csi(9,3)=4.36d05  !resistividade  | 
  csi(9,4)=2.86d00  !velocidade    _|
  csi(10,1)=2.60d00  !densidade     | 
  csi(10,2)=3.97d01  !gama          |===> décima linha da matriz csi (cong+emb3)
  csi(10,3)=7.42d05  !resistividade | 
  csi(10,4)=3.71d00  !velocidade   _|
  csi(11,1)=2.74d00  !densidade     | 
  csi(11,2)=1.19d01  !gama          |===> décima primeira linha da matriz csi (cong+emb4)
  csi(11,3)=1.23d06  !resistividade | 
  csi(11,4)=4.42d00  !velocidade   _|
  csi(12,1)=2.81d00  !densidade     | 
  csi(12,2)=7.30d-01  !gama          |===> décima segunda linha da matriz csi (embasamento)
  csi(12,3)=1.32d06  !resistividade | 
  csi(12,4)=5.49d00  !velocidade   _|







WRITE(*,*)'Sinal de classificação'
WRITE(*,FMT=13)csi


WRITE(*,*)'=============================================='
 CALL classification(csi,omega,rock,evaluation)


CALL cpu_time(vf)

  dltv = vf - vi

  PRINT*,'-----------------------------------------------'
  PRINT*,'tempo de máquina=',dltv,'segundos'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!! FORMATO DOS ARQUIVOS DE SAÍDA !!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!11 FORMAT(8(ES12.2,1x))
12 FORMAT(F16.12)
13 FORMAT(4(ES12.2,1x))
14 FORMAT(ES12.2)

END PROGRAM perceptron

