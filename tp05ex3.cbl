      ******************************************************************
      * Author: Pedro Veiga
      * Date: 24/10/2023
      * Purpose: TP05 COBOL
      * Tectonics: cobc
      ******************************************************************
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP05ex3.
       AUTHOR. Pedro Veiga.
       DATE-WRITTEN. 24/10/23.
       DATE-COMPILED. 24/10/23.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. DESKTOP AMD.
       OBJECT-COMPUTER. DESKTOP.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77 wsmenu PIC 9.
       77 wsnum1 PIC 99999.
       77 wsnum2 PIC 99999.
       77 wsresult PIC 99999 value zero.
       77 wsmascresult PIC ZZ.ZZZ,ZZ.

       SCREEN SECTION.
       01 TELA-CALCULADORA.
         05 LINE 02 COL 05 VALUE "Calculadora".
         05 LINE 04 COL 05 VALUE "1 - SOMA".
         05 LINE 05 COL 05 VALUE "2 - SUBTRACAO".
         05 LINE 06 COL 05 VALUE "3 - MULTIPLICACAO".
         05 LINE 07 COL 05 VALUE "4 - DIVISAO".
         05 LINE 08 COL 05 VALUE "0 - SAIR".
         05 LINE 10 COL 05 VALUE "Digite a opcao: ".
         05 TELA1-MENU LINE 10 COL 21 using wsmenu required.

       01 limpatela.
         05 blank screen.

       PROCEDURE DIVISION.
           DISPLAY TELA-CALCULADORA
           ACCEPT TELA1-MENU.
           if wsmenu = 0
               DISPLAY " "
               stop run
           end-if.
           if wsmenu less than 0 or greater than 4 then
               DISPLAY " "
               DISPLAY "Numero inserido invalido"
               stop run
           end-if.

           perform entrada.
           perform operacao.
           perform resultado.
           stop run.

       entrada.
           DISPLAY limpatela.
           DISPLAY "Insira o primeiro numero".
           ACCEPT wsnum1.
           DISPLAY "Insira o segundo numero".
           ACCEPT wsnum2.
           
       operacao.
           if wsmenu = 1
               add wsnum1 to wsnum2 giving wsresult
           end-if
           if wsmenu = 2
               subtract wsnum2 from wsnum1 giving wsresult
           end-if
           if wsmenu = 3
               multiply wsnum1 by wsnum2 giving wsresult
           end-if
           if wsmenu = 4
               divide wsnum1 by wsnum2 giving wsresult.
       resultado.
           DISPLAY "A opcao escolhida foi: " with no advancing.
           DISPLAY wsmenu.
           DISPLAY "O resultado da operacao entre os numeros foi de: "
           with no advancing.
           move wsresult to wsmascresult.
           DISPLAY wsmascresult.
           

       end program TP05ex3.
