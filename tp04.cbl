      ******************************************************************
      * Author: Pedro Veiga
      * Date: 21/09/2023
      * Purpose: TP04 COBOL
      * Tectonics: cobc
      ******************************************************************
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP03.
       AUTHOR. Pedro Veiga.
       DATE-WRITTEN. 25/09/23.
       DATE-COMPILED. 25/09/23.
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

       01 ficha.
           05 nome.
               10 wsmascnm         PIC A(20).
               10 wsmascsobnm      PIC A(30).
           05 wspront              PIC X(9).
           05 salario.
               10 wsbto            PIC 9(5)V99 value zero.
               10 wsliq            PIC 9(5)V99 value zero.
               10 wsref            PIC 9(5)V99 value zero.
               10 wsinss           PIC 9(5)V99 value zero.
               10 wsir             PIC 9(5)V99 value zero.

      *Variaveis de calculo de valor e quantidade
       77 wsvlhr                   PIC 999V99 value zero.
       77 wsqthr                   PIC 9(3).
       77 wsqtdep                  PIC 9(2) value zero.
       77 wsvldep                  PIC 9(3)V99 value zero.
       77 wstemp                   PIC 9(5)V99 value zero.
       77 wspens                   PIC 9(5)V99 value zero.

      *Variaveis mascara de valor e quantidade
       77 wsmascqthr               PIC ZZZ,ZZ.
       77 wsmascqtdep              PIC ZZ.
       77 wsmascvldep              PIC ZZZ,ZZ.
       77 wsmascvlhr               PIC ZZZ,ZZ.
       77 wsmascpens               PIC ZZ.ZZZ,ZZ.

      *Variaveis mascara de salario
       77 wsmascbto                PIC ZZ.ZZZ,ZZ.
       77 wsmascliq                PIC ZZ.ZZZ,ZZ.
       77 wsmascinss               PIC ZZ.ZZZ,ZZ.
       77 wsmascir                 PIC ZZ.ZZZ,ZZ.

      *Variaveis condicionais

       77 menu PIC 9 VALUE ZERO.

      *----------------------------------------------------------------*
       SCREEN SECTION.
       01 LIMPATELA.
         05 blank screen.

       01 TELAENTRADA.

         05 LINE 01 COL 20 VALUE "TP03 - TELA DE ENTRADA".

         05 LINE 03 COL 05 VALUE "Digite seu nome:".
         05 TELA1-NM COL 41 PIC A(20) USING wsmascnm
         BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 REQUIRED
         PROMPT CHARACTER IS "*".

         05 LINE 05 COL 05 VALUE "Digite seu sobrenome:".
         05 TELA1-SOBNM COL 41 PIC A(30) USING wsmascsobnm
         BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 REQUIRED
         PROMPT CHARACTER IS "*".

         05 LINE 07 COL 05 VALUE "Digite seu prontuario: ".
         05 TELA1-PRONT COL 41 PIC X(9) USING wspront
         BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 REQUIRED
         PROMPT CHARACTER IS "*".

         05 LINE 09 COL 05 VALUE "Digite o valor por hora:".
         05 TELA1-VLHR COL 41 PIC ZZZ,ZZ USING wsmascvlhr
         BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 REQUIRED
         PROMPT CHARACTER IS "*".

         05 LINE 11 COL 05 VALUE "Digite qt de horas trabalhadas:".
         05 TELA1-QTHR COL 41 PIC ZZZ,ZZ USING wsmascqthr
         BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 REQUIRED
         PROMPT CHARACTER IS "*".

         05 LINE 13 COL 05 VALUE "Digite qt de dependentes:".
         05 TELA1-QTDEP COL 41 PIC ZZ USING wsmascqtdep
         BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 
         PROMPT CHARACTER IS "*".

         05 LINE 15 COL 05 VALUE "Digite valor de dependentes:".
         05 TELA1-VLDEP COL 41 PIC ZZZ,ZZ USING wsmascvldep
         BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 
         PROMPT CHARACTER IS "*".

         05 LINE 17 COL 05 VALUE "Digite valor da pensao (se houver):".
         05 TELA1-PENS COL 41 PIC ZZZZZ,ZZ USING wsmascpens
         BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 
         PROMPT CHARACTER IS "*".

      *----------------------------------------------------------------*

       01 TELASAIDA.
         05 LINE 01 COL 20 VALUE "TP04 - TELA DE SAIDA".
         05 LINE + 1 COL 20 VALUE " ".
         05 LINE + 1 COL 20 VALUE "FICHA DE DADOS PESSOAIS".
         05 LINE + 1 COL 20 VALUE " ".

         05 LINE + 1 COL 05 VALUE "NOME:"
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 25 VALUE " |".
         05 COL 30 USING wsmascnm. 

         05 LINE + 1 COL 05 VALUE "SOBRENOME"
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 25 VALUE " |".
         05 COL 30 USING wsmascsobnm.

         05 LINE + 1 COL 05 VALUE "PRONTUARIO"
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 25 VALUE " |".
         05 COL 30 USING wspront.

         05 LINE + 1 COL 05 VALUE "VALOR DA HORA"
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 25 VALUE " |".
         05 COL 30 USING wsmascvlhr.

         05 LINE + 1 COL 05 VALUE "QTDE H/TRABALHADAS"
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 25 VALUE " |".
         05 COL 30 USING wsmascqthr.

         05 LINE + 1 COL 05 VALUE "QTDE DE DEP"
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 25 VALUE " |".
         05 COL 30 USING wsmascqtdep.

         05 LINE + 1 COL 05 VALUE " ".
         05 LINE + 1 COL 20 VALUE "FICHA DE VALORES DE SALARIO".
         05 LINE + 1 COL 05 VALUE " ".


         05 LINE + 1 COL 05 VALUE "SALARIO BRUTO"
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 25 VALUE " |".
         05 COL 30 USING wsmascbto.

         05 LINE + 1 COL 05 VALUE "INSS"
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 25 VALUE " |".
         05 COL 30 USING wsmascinss.

         05 LINE + 1 COL 05 VALUE "IRRF"
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 25 VALUE " |".
         05 COL 30 USING wsmascir.

         05 LINE + 1 COL 05 VALUE "SALARIO LIQUIDO"
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 25 VALUE " |".
         05 COL 30 USING wsmascliq.

         05 LINE + 1 COL 05 VALUE "PENSAO" BACKGROUND-COLOR 1
            FOREGROUND-COLOR 7.
         05 COL 25 VALUE " |".
         05 COL 30 USING wsmascpens.

         05 LINE + 1 COL 05 VALUE " ".

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       DISPLAYS.
           DISPLAY TELAENTRADA.
           ACCEPT TELA1-NM.
           ACCEPT TELA1-SOBNM.
           ACCEPT TELA1-PRONT.
       VALORES.
           ACCEPT TELA1-VLHR.
           ACCEPT TELA1-QTHR.
           ACCEPT TELA1-QTDEP.
           ACCEPT TELA1-VLDEP.
           ACCEPT TELA1-PENS.

      *Dados mascara para dados variavel
           MOVE wsmascqthr to wsqthr.
           MOVE wsmascqtdep to wsqtdep.
           MOVE wsmascvldep to wsvldep.
           MOVE wsmascvlhr to wsvlhr.
           MOVE wsmascpens to wspens.
           COMPUTE wsbto = wsvlhr * wsqthr.

           IF wsbto LESS THAN OR EQUAL TO 1320,00
               COMPUTE wsinss = 1320 * 0,075
           ELSE
               IF wsbto GREATER THAN 1320,10 AND LESS THAN 2571,29
                   COMPUTE wsinss = ((wsbto - 1320,00) * 0,09) + 99
               ELSE
                   IF wsbto GREATER THAN 2571,30 AND LESS THAN 3856,94
                       COMPUTE wsinss = ((wsbto - 2571,29) * 0,12) +
                         112,62 + 99
                   ELSE
                       IF wsbto GREATER THAN 3856,95 AND LESS THAN
                         7507,49
                           COMPUTE wsinss = ((wsbto - 3856,94) *
                           0,14) + 154,28 + 112,62 + 99
                       ELSE
                           IF wsbto GREATER THAN 7507,49
                               COMPUTE wsinss = 876,97.


           COMPUTE wsref = (wsbto - wsinss - wspens)-(wsqtdep * 189,59).

           IF wsref LESS THAN OR EQUAL TO 2112,00
               COMPUTE wsir = 0
           ELSE
               IF wsref GREATER THAN 2112,01 AND LESS THAN 2826,65
                   COMPUTE wsir = (wsref * 0,075) - 158,40
               ELSE
                   IF wsref GREATER THAN 2826,66 AND LESS THAN 3751,06
                       COMPUTE wsir = (wsref * 0,15) - 370,40
                   ELSE
                       IF wsref GREATER THAN 3751,07 AND LESS THAN
                       4664,69
                           COMPUTE wsir = (wsref * 0,225) - 651,73
                       ELSE
                           COMPUTE wsir = (wsref * 0,275) - 884,96.

           COMPUTE wsliq = wsref - wsir.
           COMPUTE wstemp = wsvldep * wsqtdep.
           COMPUTE wsliq = wsliq + wstemp.

      *Dados variavel para dados mascara
           MOVE wsqthr to wsmascqthr.
           MOVE wsqtdep to wsmascqtdep.
           MOVE wsvldep to wsmascvldep.
           MOVE wsvlhr to wsmascvlhr.
           MOVE wsbto to wsmascbto.
           MOVE wsliq to wsmascliq.
           MOVE wsinss to wsmascinss.
           MOVE wsir to wsmascir.
           MOVE wspens to wsmascpens.
       
       DISPLAY LIMPATELA.
       DISPLAY TELASAIDA.
       DISPLAY " ".
       DISPLAY "Deseja calcular novamente?(Digite 1, senao, digite 0): "
       at LINE 20 COL 05.
       ACCEPT MENU at LINE 20 COL 62 REQUIRED.
           IF MENU EQUAL TO 1
               DISPLAY LIMPATELA
               DISPLAY TELAENTRADA
               GO TO VALORES
           ELSE
               IF MENU EQUAL TO 0
                   STOP RUN.
       END PROGRAM.
      *----------------------------------------------------------------*