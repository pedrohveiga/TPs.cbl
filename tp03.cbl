      ******************************************************************
      * Author: Pedro Veiga
      * Date: 21/09/2023
      * Purpose: TP03 COBOL
      * Tectonics: cobc
      ******************************************************************
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP03.
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
      *Variaveis mascara de valor e quantidade
       77 wsmascqthr       PIC ZZZ,ZZ.
       77 wsmascqtdep      PIC ZZ.
       77 wsmascvldep      PIC ZZZ,ZZ.
       77 wsmascvlhr       PIC ZZZ,ZZ.

      *Variaveis mascara de salario
       77 wsmascbto        PIC ZZ.ZZZ,ZZ.
       77 wsmascliq        PIC ZZ.ZZZ,ZZ.
       77 wsmascinss       PIC ZZ.ZZZ,ZZ.
       77 wsmascir         PIC ZZ.ZZZ,ZZ.

      *Variaveis mascara de dados fixos
       01 nome.
         05 wsmascnm       PIC A(20).
         05 wsmascsobnm    PIC A(30).
       77 wspront          PIC X(9).

      *Variaveis de calculo de valor e quantidade
       77 wsvlhr           PIC 999V99 value zero.
       77 wsqthr           PIC 9(3).
       77 wsqtdep          PIC 9(2).
       77 wsvldep          PIC 9(3)V99 value zero.
       77 wstemp           PIC 9(5)V99 value zero.

      *Variaveis de calculo de salario
       01 salario.
         05 wsbto          PIC 9(5)V99 value zero.
         05 wsliq          PIC 9(5)V99 value zero.
         05 wsref          PIC 9(5)V99 value zero.
         05 wsinss         PIC 9(5)V99 value zero.
         05 wsir           PIC 9(5)V99 value zero.

       SCREEN SECTION.
       01 LIMPATELA.
         05 blank screen.

       01 TELAENTRADA.

         05 LINE 01 COL 20 VALUE
         "TP03 - TELA DE ENTRADA".

         05 LINE 03 COL 05 VALUE
         "Digite seu nome:".
         05 TELA1-NM LINE 03 COL + 5 PIC A(20) USING wsmascnm 
            BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 REQUIRED PROMPT 
            CHARACTER IS "*".

         05 LINE 05 COL 05 VALUE
         "Digite seu sobrenome:".
         05 TELA1-SOBNM LINE 05 COL + 5 PIC A(30) USING wsmascsobnm
            BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 REQUIRED PROMPT 
            CHARACTER IS "*".

         05 LINE 07 COL 05 VALUE
         "Digite seu prontuario: ".
         05 TELA1-PRONT LINE 07 COL + 5 PIC X(9) USING wspront
            BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 REQUIRED PROMPT 
            CHARACTER IS "*".

         05 LINE 09 COL 05 VALUE
         "Digite o valor por hora:".
         05 TELA1-VLHR LINE 09 COL + 5 PIC ZZZ,ZZ USING wsmascvlhr
            BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 REQUIRED PROMPT 
            CHARACTER IS "*".

         05 LINE 11 COL 05 VALUE
         "Digite qt de horas trabalhadas:".
         05 TELA1-QTHR LINE 11 COL + 5 PIC ZZZ,ZZ USING wsmascqthr
            BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 REQUIRED PROMPT 
            CHARACTER IS "*".

         05 LINE 13 COL 05 VALUE
         "Digite qt de dependentes:".
         05 TELA1-QTDEP LINE 13 COL + 5 PIC ZZ USING wsmascqtdep
            BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 REQUIRED PROMPT 
            CHARACTER IS "*".

         05 LINE 15 COL 05 VALUE
         "Digite valor de dependentes:".
         05 TELA1-VLDEP LINE 15 COL + 5 PIC ZZZ,ZZ USING 
            wsmascvldep BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 REQUIRED 
            PROMPT CHARACTER IS "*".

       01 TELASAIDA.
         05 LINE 01 COL 20 VALUE "TP03 - TELA DE SAIDA".

         05 LINE 03 COL 05 VALUE "NOME: "
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 LINE 03 COL 30 USING wsmascnm REVERSE-VIDEO.

         05 LINE + 1 COL 05 VALUE "SOBRENOME: "
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 30 USING wsmascsobnm REVERSE-VIDEO.

         05 LINE + 1 COL 05 VALUE "PRONTUARIO: "
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 30 USING wspront REVERSE-VIDEO.

         05 LINE + 1 COL 05 VALUE "VALOR DA HORA: "
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 30 USING wsmascvlhr REVERSE-VIDEO.

         05 LINE + 1 COL 05 VALUE "QTDE H/TRABALHADAS: "
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 30 USING wsmascqthr REVERSE-VIDEO.

         05 LINE + 1 COL 05 VALUE "QTDE DE DEP: "
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 30 USING wsmascqtdep REVERSE-VIDEO.

         05 LINE + 1 COL 05 VALUE " ".

         05 LINE + 1 COL 05 VALUE "SALARIO BRUTO: "
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 30 USING wsmascbto REVERSE-VIDEO.

         05 LINE + 1 COL 05 VALUE "INSS: "
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 30 USING wsmascinss REVERSE-VIDEO.

         05 LINE + 1 COL 05 VALUE "IRRF: "
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 30 USING wsmascir REVERSE-VIDEO.

         05 LINE + 1 COL 05 VALUE "SALARIO LIQUIDO: "
         BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
         05 COL 30 USING wsmascliq REVERSE-VIDEO.

         05 LINE + 1 COL 05 VALUE " ".

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           DISPLAY TELAENTRADA.
           ACCEPT TELA1-NM.
           ACCEPT TELA1-SOBNM.
           ACCEPT TELA1-PRONT.
           ACCEPT TELA1-VLHR.
           ACCEPT TELA1-QTHR.
           ACCEPT TELA1-QTDEP.
           ACCEPT TELA1-VLDEP.
       CALCULOS.
      *Mascara para variavel 
           MOVE wsmascqthr to wsqthr.
           MOVE wsmascqtdep to wsqtdep.
           MOVE wsmascvldep to wsvldep.
           MOVE wsmascvlhr to wsvlhr.
     
           COMPUTE wsbto = wsvlhr * wsqthr.
           COMPUTE wsinss = wsbto * 0,14.
           COMPUTE wsir = wsbto * 0,275.
           COMPUTE wsref = wsbto - wsinss.
           COMPUTE wsliq = wsref - wsir.
           COMPUTE wstemp = wsvldep * wsqtdep.
           COMPUTE wsliq = wsliq + wstemp.

      *Variavel para mascara
           MOVE wsqthr to wsmascqthr.
           MOVE wsqtdep to wsmascqtdep.
           MOVE wsvldep to wsmascvldep.
           MOVE wsvlhr to wsmascvlhr.
           MOVE wsbto to wsmascbto.
           MOVE wsliq to wsmascliq.
           MOVE wsinss to wsmascinss.
           MOVE wsir to wsmascir.

           DISPLAY LIMPATELA.
           DISPLAY TELASAIDA.

           STOP "Pressione enter...".
           STOP RUN.

      *----------------------------------------------------------------*
