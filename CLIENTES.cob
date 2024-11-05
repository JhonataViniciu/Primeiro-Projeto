       IDENTIFICATION DIVISION.
       PROGRAM-ID.   CLIENTES.
      *==========================================
      *==  OBJETIVO: SISTEMA DE GESTAO DE CLIENTES
      *==  AUTOR:  JHONATA VINICIUS
      *==========================================
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WRK-OPCAO  PIC X(1).
       77  WRK-TITULO PIC X(20).
       77  WRK-MODULO PIC X(25).
       77  WRK-TECLA  PIC X(1).

       SCREEN          SECTION.
       01  TELA.
           05 LIMP-TELA.
              10 BLANK-SCREN.
              10 LINE 01 COLUMN 01 PIC X(20) ERASE EOL
                 BACKGROUND-COLOR 3 .
              10 LINE 01 COLUMN 25 PIC X(20)
                 BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
                       FROM 'SISTEMA DE CLIENTES'.
              10 LINE 02 COLUMN 01 PIC X(25) ERASE EOL
                 BACKGROUND-COLOR 1 FROM WRK-MODULO.

       01  MENU.
           05 LINE 07 COLUMN 15 VALUE '1 - INCLUIR'.
           05 LINE 08 COLUMN 15 VALUE '2 - CONSULTAR'.
           05 LINE 09 COLUMN 15 VALUE '4 - ALTERAR'.
           05 LINE 10 COLUMN 15 VALUE '5 - EXCLUIR'.
           05 LINE 11 COLUMN 15 VALUE '6 - RELATORIO'.
           05 LINE 12 COLUMN 15 VALUE 'X - SAIR'.
           05 LINE 13 COLUMN 15 VALUE 'OPCAO.......: ' .
           05 LINE 13 COLUMN 29 USING  WRK-OPCAO.

       PROCEDURE DIVISION.
       0001-PRINCIPAL  SECTION.
           PERFORM 1000-INICIAR.
           PERFORM 2000-PROCESSAR.
           PERFORM 3000-FINALIZAR.
           STOP RUN.

       1000-INICIAR.
           DISPLAY TELA.
             ACCEPT MENU.
       2000-PROCESSAR.
               EVALUATE WRK-OPCAO
                WHEN 1
                  PERFORM 5000-INCLUIR
                WHEN 2
                  CONTINUE
                WHEN 3
                  CONTINUE
                WHEN 4
                  CONTINUE
                WHEN 5
                  CONTINUE
                WHEN OTHER
                  IF WRK-OPCAO NOT EQUAL 'X'
                     DISPLAY'ENTRE COM A OPCAO CORRETA'
                  END-IF
               END-EVALUATE.



       3000-FINALIZAR.
               CONTINUE.


       5000-INCLUIR.
             MOVE 'MODULO - INCLUSAO ' TO WRK-MODULO.
             DISPLAY TELA.
               ACCEPT WRK-TECLA AT 1620.
