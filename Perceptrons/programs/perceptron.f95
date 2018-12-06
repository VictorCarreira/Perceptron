PROGRAM perceptron

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !Criação de um Perceptron para análise de uma litologia                      !
  !Criador: Victor Ribeiro Carreira                                            !
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
  INTEGER, PARAMETER::SSP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DDP = SELECTED_REAL_KIND(8,10)
  INTEGER(KIND=DDP)::epoca 
  INTEGER(KIND=SSP)::rock, nd
  REAL(KIND=DDP):: etaR, etaD, vi, vf, dltv
  REAL(KIND=DDP),PARAMETER::cc=0.1d0, eetaO=20.d0, taau=30.d0
  REAL(KIND=DDP), ALLOCATABLE, DIMENSION(:,:):: xi1, xi2, csi, omega
  CHARACTER(LEN=30):: evaluation


  ALLOCATE(omega(4,1))

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


  CALL Etraining(xi1, xi2)

 !Vetor pesos:

  CALL weight(4,1,1.0d00,omega)

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
  
  WRITE(*,*)'Vetor w'
  WRITE(*,FMT=14)omega


!************************************************!
!************ Atualização Sináptica *************!
!************************************************!



!CALL synaptic(xi1,xi2,1.0d-2,epoca,omega) ! Sem critério de parada. Precisa fornecer as épocas.
CALL synaptic(xi1,xi2,etaD,omega) !Com critério de parada. Calcula o número de épocas.




  WRITE(*,*)'Vetor w atualizado'
  WRITE(*,FMT=14)omega
    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Fase de Classificação !!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! vetor W atualizado !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!Input (Conjunto de classificação)


CALL Eclassification(csi,nd)


WRITE(*,*)'Sinal de classificação'
WRITE(*,FMT=13)csi


WRITE(*,*)'=============================================='

 CALL classification(csi,omega,nd,rock,evaluation)




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!! FINAL DO PERCEPTRON !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
