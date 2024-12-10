       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAIXA-ELETRONICO.
      *=======================================================
      *== AUTOR: JHONATA *JHOWW_24*                 EMPRESA:XPTO
      *== OBJETIVO: CRIAÇÃO DE U CAIXA ELETRONICO
      *== DATA: 10/12/2024
      *== OBSERVAÇÕES:
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE                 SECTION.

       77  WS-OPCAO            PIC X(1).
       77  WS-SALDO            PIC 9(10)V99 VALUE ZERO.
       77  WS-DEPOSITO         PIC 9(10)V99 VALUE ZERO.
       77  WS-SAQUE            PIC 9(10)V99 VALUE ZERO.
       77  WS-MODULO           PIC X(25).
       77  WS-TECLA            PIC X(1).
       77  WS-MSGERRO          PIC X(30).
       77  WS-TEXT             PIC X(30).

       SCREEN                          SECTION.

       01  TELA.
           05 LIMPA-TELA.
              10 BLANK SCREEN.
              10 LINE 01 COLUMN 01 PIC X(20) ERASE EOL
                 BACKGROUND-COLOR 3 .
              10 LINE 01 COLUMN 25 PIC X(20)
                 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
                       FROM 'CAIXA ELETRONICO'.
              10 LINE 02 COLUMN 01 PIC X(25) ERASE EOL
                 BACKGROUND-COLOR 1 FROM WS-MODULO.



       01  TELAO.
           05 LINE 07 COLUMN 15 VALUE '*****************************'.
           05 LINE 08 COLUMN 15 VALUE '**     CAIXA ELETRONICO    **'.
           05 LINE 09 COLUMN 15 VALUE '*****************************'.

       01  MENU.
           05 LINE 11 COLUMN 15 VALUE '1 - DEPOSITO'.
           05 LINE 12 COLUMN 15 VALUE '2 - SAQUE' .
           05 LINE 13 COLUMN 15 VALUE '3 - CONSULTAR SALDO' .
           05 LINE 14 COLUMN 15 VALUE '4 - SAIR' .
           05 LINE 15 COLUMN 15 USING  WS-OPCAO.

       01  TELA-DEPOSITO.
           05 LINE 11 COLUMN 15 VALUE ' '.
           05 LINE 12 COLUMN 15 VALUE '  DIGITE O VALOR DO DEPOSITO '.
           05 LINE 13 COLUMN 15
                               BACKGROUND-COLOR 3
                               USING WS-DEPOSITO.

       01  TELA-SAQUE.
           05 LINE 11 COLUMN 15 VALUE ' '.
           05 LINE 12 COLUMN 15 VALUE '  DIGITE O VALOR DE SAQUE '.
           05 LINE 13 COLUMN 15
                               BACKGROUND-COLOR 3
                               USING WS-SAQUE.
       01  SALDO.
           05 LINE 11 COLUMN 15 VALUE 'SALDO DISPLONIVEL'.
           05 LINE 16 COLUMN 10 PIC X(30)
                               BACKGROUND-COLOR 3
                               FROM WS-SALDO.
           05 COLUMN PLUS 2 PIC X(01)
                               BACKGROUND-COLOR 3
                               USING WS-TECLA.

       01  MOSTRA-ERRO.
           05 MSG-ERRO.
              10 LINE 16 COLUMN 01 ERASE EOL
                               BACKGROUND-COLOR 3 .
              10 LINE 16 COLUMN 10 PIC X(30)
                               BACKGROUND-COLOR 3
                               FROM WS-MSGERRO.
              10 COLUMN PLUS 2 PIC X(01)
                               BACKGROUND-COLOR 3
                               USING WS-TECLA.
       PROCEDURE DIVISION.




       0001-PRINCIPAL                  SECTION.
       0100-INICIALIZAR                SECTION.

       1100-MONTA-TELA.

           DISPLAY TELA.
             ACCEPT TELAO.
             ACCEPT MENU.


       0200-PROCESSAR                  SECTION.

               EVALUATE WS-OPCAO
                WHEN 1
                  PERFORM 5000-DEPOSITO
                WHEN 2
                  PERFORM 6000-SAQUE
                WHEN 3
                  PERFORM 7000-CONSULTAR-SALDO
                WHEN OTHER
                  IF WS-OPCAO NOT EQUAL 4
                     DISPLAY'ENTRE COM A OPCAO CORRETA'
                  END-IF
               END-EVALUATE.
               PERFORM 1100-MONTA-TELA.


       0300-FINALIZAR                  SECTION.

       5000-DEPOSITO.
               MOVE 'MODO DEPOSITO' TO WS-MODULO.
                 DISPLAY TELA.
                 DISPLAY TELAO.
                   ACCEPT TELA-DEPOSITO.
               COMPUTE WS-SALDO = WS-SALDO + WS-DEPOSITO.
                 MOVE 'VALOR DEPOSITADO' TO WS-MSGERRO.
                   ACCEPT MOSTRA-ERRO.
               PERFORM 1100-MONTA-TELA.

       6000-SAQUE.
               MOVE 'MODO SAQUE' TO WS-MODULO.
                 DISPLAY TELA.
                 DISPLAY TELAO.
                   ACCEPT TELA-SAQUE.
               COMPUTE WS-SALDO = WS-SALDO - WS-SAQUE.
                 MOVE 'SAQUE CONFIRMADO! ' TO WS-MSGERRO.
                   ACCEPT MOSTRA-ERRO.
               PERFORM 1100-MONTA-TELA.

       7000-CONSULTAR-SALDO.
               MOVE 'MODO CONSULTA DE SALDO' TO WS-MODULO.
                 DISPLAY TELA.
                 DISPLAY TELAO.
                   ACCEPT SALDO.
               PERFORM 1100-MONTA-TELA.
