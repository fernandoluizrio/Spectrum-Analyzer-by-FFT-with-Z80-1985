;--------------------------------------------------------------
;
; UFRJ_FCL_1984_Analisador_de_Espectro_por_FFT.bas
;
; Autor: Fernando da Cunha Luiz
;
; 1984/1985
;
; Ultima revisao: 11 de outubro de 2021, pelo autor
;
; Este software faz parte do Analisador de Espetro por FFT,
; desenvolvido pelo autor, para obtencao do Titulo de 
; Engenheiro Eletronico da Universidade Federal do Rio de 
; Janeiro, apresentado aos examinadores em 12 de marco de 1985.
;
; Este codigo foi reescrito no ano de 2021. O software original
; foi escrito manualmente, com lapis e papel.
; Foi depurado num microcomputador Z80 compativel com Sinclair
; ZX Spectrum e num microcomputador IBM PC compativel, com a
; linguagem de programacao BASIC.
; Este codigo foi utilizado para auxiliar o desenvolvimento do 
; Analisador de Espetro em Assembly Z80.
; Pode haver alguns erros na redigitacao/transcricao do codigo
; original. As anotações a lapis do codigo original e discos 
; flexiveis foram perdidos com o tempo, mais de 36 anos.
; Neste codigo podem haver erros.
;
;--------------------------------------------------------------

REM -----------------------------------------------------------

REM ------------------  65 DEEL/UFRJ

REM                APÊNDICE 4

REM LISTAGEM DO PROGRAMA DA FFT EM BASIC:

    10  REM  FFT-FAST FOURIER TRANSFORM
    20  DIM A(255) : DIM B(255) : DIM M(255)
    30  PI = 3.141592654

    38  REM OBTEM N E
    39  REM GERA ONDA QUADRADA PARA TESTES, EX.: 256
    40  GOSUB 500

    45  REM PARA N = 256 => M = 8
    50  LET M = LOG (N) / LOG (2)
    60  FOR K = M TO 1 STEP - 1
    65   REM PARA M = 8 => K1 = 128, 64, 32, 16, 8, 4, 2, 1
    70   LET K1 = 2 ^ (K - 1)
    75   REM PARA M = 8 => K2 = 256, 128, 64, 32, 16, 8, 4, 2
    80   LET K2 = 2 * K1
    85   REM PARA M = 8 => K3 = 127, 63, 12, 15, 7, 3, 1, 0
    90   LET K3 = K1 - 1
    95   REM PARA M = 8 => p = 0; 0,128; 0,64,128,192; ...; 0,2,4,6,8,...254
    100  FOR P = 0 TO N - 1 STEP K2
    110   LET X = P / K1

    119   REM REVERTE OS BITS:
    120   GOSUB 390

    130   LET X1 = COS (2 * PI * S / N)
    140   LET Y1 = SIN (2 * PI * S / N)
    150   FOR J = 0 TO K3
    160   LET K4 = J + P
    170   LET K5 = K4 + K1
    180   LET X2 = X1 * A(K5) + Y1 * B(K5)
    190   LET Y2 = Y1 * A(K5) + X1 * B(K5)
    200   LET A(K5) = A(K4) - X2
    210   LET B(K5) = B(K4) - Y2
    220   LET A(K4) = A(K4) + X2
    230   LET B(K4) = B(K4) + Y2
    240   NEXT J
    250  NEXT P
    260 NEXT K

    270 FOR I = 1 TO N - 1
    280  LET X = 1

    289  REM REVERTE OS BITS:
    290  GOSUB 390

    300  IF S <= I THEN GOTO 370
    310  LET X3 = A(I)
    320  LET Y3 = B(I)
    330  LET A(I) = A(S)
    340  LET B(I) = B(S)
    350  LET A(S) = X3
    360  LET B(S) = Y3
    370 NEXT I
    380 GOTO 550 : REM CONTINUA EM 550

    389 REM REVERTE OS BITS:
    390 LET U = 0
    400 FOR L = 1 TO M
    410  LET K5 = INT (X / 2)
    420  LET A = X - 2 * K5
    430  LET K6 = 2 ^ (M - L)
    440  LET U = U + A * K6
    450  LET X = K5
    460  IF X <= 1 THEN GOTO 480
    470 NEXT L

REM ------------------  66 DEEL/UFRJ

    480 LET S = U + X * K6 / 2
    490 RETURN

    499 REM OBTEM N, EX.: 256
    500 INPUT "# DE PONTOS = ";N
    509  REM GERA ONDA QUADRADA PARA TESTES:
    510  FOR I = 0 TO N / 2 - 1
    520   A(I) = 0 : A(I + N / 2) = 255
    530  NEXT
    540 RETURN

    549 REM CONTINUACAO DE 380:
    550 FOR I = 0 TO N - 1
    560  LET M(I) = 2 * SQR (A(I) * A(I) + B(I) * B(I)) / N
    570 NEXT
    575 T = N / 2
    580 M(0) = M(0) / 2 : M(T) = M(T) / 2
    590 STOP
    591 REM FIM DA FFT

    599 REM IMPRIME OS RESULTADOS:
    600 FOR I = 0 TO N - 1
    610  PRINT I; TAB( 10);M(I); TAB(30; INT (M(I) + .5)
    620  PRINT
    630 NEXT
    640 END

REM -----------------------------------------------------------
