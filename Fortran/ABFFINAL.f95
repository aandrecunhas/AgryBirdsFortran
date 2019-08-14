! AngryBirds fortran Versão 1.0
MODULE angry
  IMPLICIT NONE
  !Definição de restrição
  PRIVATE velEixoY, tempoSubida, tempoTotal, velEixoX
  PUBLIC passaro, porco, lancamento, radiano
  ! Declarações das variáveis que serão usadas no programa
  REAL, PARAMETER :: pi = 3.1415927, grav = 10. !Numero PI e Aceleração da Gravidade
  REAL :: tolerancia
  INTEGER :: dificuldade, fase = 1
  CHARACTER :: continua ! Dependendo do que for recebido, a fase é reiniciada ou não.
  !Tipo Passaro
  TYPE passaro
    REAL :: velInicial = 0., angulo = 0., posicao = 0.
  END TYPE passaro
  !Tipo Porco
  TYPE porco
    REAL :: distanciaAlvo
  END TYPE porco
  
  CONTAINS
  !Funções usadas no programa
  !Converte o angulo dado em graus em radianos
  REAL FUNCTION radiano(grau)
    REAL :: grau
    radiano = grau * pi / 180.
  END FUNCTION radiano
  !Decompõe a velocidade no eixo Y
  REAL FUNCTION velEixoY(this)
    TYPE(passaro), INTENT(IN) :: this
    velEixoY = sin(this%angulo) * this%velInicial
  END FUNCTION velEixoY
  !Calcula o tempo até atingir a altura máxima (tempo de subida)
  REAL FUNCTION tempoSubida(this)
    TYPE(passaro), INTENT(IN) :: this
    tempoSubida = (velEixoY(this) - this%velInicial) / ((-1) * grav) !aceleração decrescente
  END FUNCTION tempoSubida
  !Soma o tempo de subida ao tempo de descida (são iguais)
  REAL FUNCTION tempoTotal(this)
    TYPE(passaro), INTENT(IN) :: this
    tempoTotal = tempoSubida(this) * 2
  END FUNCTION tempoTotal
  !Decompõe a velocidade no eixo X
  REAL FUNCTION velEixoX(this)
    TYPE(passaro), INTENT(IN) :: this
    velEixoX = cos(this%angulo) * this%velInicial
  END FUNCTION velEixoX
  !Calcula a posição atingida no eixo X
  FUNCTION lancamento(this) result(alcance)
    TYPE(passaro), INTENT(IN) :: this
    REAL :: alcance
    alcance = 0. + velEixoX(this) * tempoTotal(this)
  END FUNCTION lancamento  
END MODULE angry

PROGRAM angrybirds
  USE angry
  IMPLICIT NONE
  ! Programa
  TYPE(passaro) :: pas
  TYPE(porco) :: por
  pas = passaro()
  por = porco(58.5)
  
    PRINT *, '========================== Angry Birds Fortran =========================='
    PRINT *, ''
    PRINT *, '************************* Escolha a dificuldade *************************'
    PRINT *, ''
    PRINT *, '1 - Facil / 2 - Intermediario / 3 - Dificil'
    PRINT *, ''
    READ *, dificuldade
	!De acordo com a dificuldade escolhida pelo jogador, estipula-se uma tolerância para acerto do alvo.
    IF(dificuldade == 1) THEN
       tolerancia = 20
    ELSE IF (dificuldade == 2) THEN
        tolerancia = 10
      ELSE IF (dificuldade == 3) THEN
         tolerancia = 2.5
    END IF
    PRINT *, ''
    DO 
      PRINT *, '******************************** FASE',fase, '********************************' 
      PRINT *, ''
      PRINT *, 'O alvo se encontra a ',por%distanciaAlvo,' metros'
      PRINT *, ''
      PRINT *, ''
      PRINT *, '   _---_                       ________'
      PRINT *, '  -     -                    _(        )_              ____ '
      PRINT *, '   -___-                    (____________)          __(    )  ' 
      PRINT *, '                                                   (________)'
      PRINT *, '' 
      PRINT *, ''
      PRINT *, ''
      PRINT *, '     ___\\  // '
      PRINT *, '   _/    \\//                                                    () ()'
      PRINT *, "  ( >     ||                                                    ( 'o' )"
      PRINT *, ' (__)     ||                                                    (_____)~'
      PRINT *, ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
      PRINT *, ''
      PRINT *, '_________________________________________________________________', por%distanciaAlvo, 'm'
      PRINT *, ''
      PRINT *, ''     
      PRINT *, 'Insira uma velocidade inicial de lancamento (m/s)'
      PRINT *, '' 
      READ *, pas%velInicial 
      PRINT *, '' 
      PRINT *, 'Insira o angulo de lancamento (graus)'
      PRINT *, '' 
      READ *, pas%angulo
      PRINT *, '' 
      !Calcula o angulo em radianos
      pas%angulo = radiano(pas%angulo)
      !Exibe o alcance do lançamento
      pas%posicao = lancamento(pas)
      PRINT *, 'Voce atingiu o alcance: ',pas%posicao
      PRINT *, '' 
	  !Verifica se o alcance atingiu o alvo
      IF(pas%posicao <= por%distanciaAlvo + tolerancia .AND. pas%posicao >= por%distanciaAlvo - tolerancia) THEN
          PRINT *, ''
          PRINT *, '' 
          PRINT *, '********************* O alvo foi atingido! PARABENS! *********************'
          PRINT *, ''                                                                 
          PRINT *, '                                  _____   '     	
          PRINT *, '                                 (_____)' 
          PRINT *, ''     
          PRINT *, '                                 ()   ()'
          PRINT *, '                                ( X o X ) '
          PRINT *, '                                (_______)~ '
          PRINT *, ''
          PRINT *, ''
          fase = fase + 1
          IF (fase == 2) THEN
            por%distanciaAlvo = 150.
           CYCLE ! inicia a segunda fase do jogo.
          ELSE IF (fase == 3) THEN
            por%distanciaAlvo = 346.
            CYCLE ! inicia a terceira fase do jogo.
          ELSE IF (fase == 4) THEN
            por%distanciaAlvo = 839.
            CYCLE ! inicia a quarta fase do jogo.
          ELSE IF (fase == 5) THEN
            por%distanciaAlvo = 2391.
            CYCLE ! inicia a quinta fase do jogo.
          ELSE
            PRINT *, '============================= FIM DE JOGO ================================'
            PRINT *, ''
            PAUSE
          END IF
        ELSE IF (pas%posicao >= por%distanciaAlvo + tolerancia) THEN
          PRINT *, '************************* Ultrapassou o alvo *****************************'
        ELSE
          PRINT *, '************************* Nao alcancou o alvo ****************************'
      END IF
      PRINT *, '' 
      PRINT *, '' 
      10 PRINT *, 'Deseja tentar novamente? s/n'
      READ *, continua
      IF(continua == 's' .OR. continua == 'S') THEN
       CALL SYSTEM ('CLS')
         CYCLE ! Volta ao inicio do loop (reinicia a fase)
      ELSE IF (continua == 'n' .OR. continua == 'N') THEN 
         PRINT *,'************************* FIM DE JOGO ****************************'
         EXIT
      ELSE
        GOTO 10 !Comando GOTO caso não seja inserido letra diferente de s/n
      END IF
  END DO   
END PROGRAM angrybirds