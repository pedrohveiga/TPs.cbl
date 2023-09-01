      ******************************************************************
      * Author: Pedro Veiga
      * Date: 01/09/2023
      * Purpose: TP02 COBOL
      * Tectonics: cobc
      ******************************************************************
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP02.
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
      *VARIÁVEIS COM MASCARA

       77 wspront PIC X(9).
       77 wsvlhr PIC 999V99.
       77 wsqthr PIC 9(3).
       77 wsqtdep PIC 99.
       77 wsvldep PIC 999V99.
       77 wstemp PIC 99999V99.

       01 nome.
           05 wsnm PIC A(20).
           05 wssobnm PIC A(30).
       01 salario.
           05 wsbto PIC 99999V99.
           05 wsliq PIC 99999V99.
           05 wsref PIC 99999V99.
           05 wsinss PIC 99999V99.
           05 wsir PIC 99999V99.

      *VARIÁVEIS PARA CALCULO


       SCREEN SECTION.
       01 limpatela.
           05 BLANK SCREEN.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       ENTRADA.
       DISPLAY "Digite seu nome: " WITH NO ADVANCING.
       ACCEPT wsnm WITH PROMPT CHARACTER is "-".
       DISPLAY "Digite seu sobrenome: " WITH NO ADVANCING.
       ACCEPT wssobnm.
       DISPLAY "Digite seu prontuario: " WITH NO ADVANCING.
       ACCEPT wspront.
       DISPLAY "Digite o valor hora: R$" WITH NO ADVANCING.
       ACCEPT wsvlhr.
       DISPLAY "Digite qt de horas trabalhadas: " WITH NO ADVANCING.
       ACCEPT wsqthr.
       DISPLAY "Digite a qt de dependentes: " WITH NO ADVANCING.
       ACCEPT wsqtdep.
       DISPLAY "Digite o valor do dependente: " WITH NO ADVANCING.
       ACCEPT wsvldep.
       DISPLAY LIMPATELA.

       CALCULOS.
      *Usados no TP01:
      *MULTIPLY wsvalorhora by wsqthoras GIVING wsbruto.
      *MULTIPLY wsbruto by 0,14 GIVING wsdescontoinss.
      *MULTIPLY wsbruto by 0,275 GIVING wsir.
      *SUBTRACT wsdescontoinss from wsbruto GIVING wsref.
      *SUBTRACT wsir from wsref GIVING wsliquido.
      *MULTIPLY wsvalordependente by wsqtdependentes GIVING wstemp.
      *ADD wsliquido to wstemp GIVING wsliquido.

      *Usados no TP02:
       COMPUTE wsbto = wsvlhr*wsqthr.
       COMPUTE wsinss = wsbto * 0,14.
       COMPUTE wsir = wsbto * 0,275.
       COMPUTE wsref = wsbto - wsinss.
       COMPUTE wsliq = wsref - wsir.
       COMPUTE wstemp = wsvldep * wsqtdep.
       COMPUTE wsliq  = wsliq + wstemp.

       SAIDA.
      *DISPLAY "TP02 - CALCULO DE SALARIO" AT 0230.
      *DISPLAY "NOME: " AT 0405 wsnm AT 0425.
      *DISPLAY "SOBRENOME: " AT 0505 wssobnm AT 0525.
      *DISPLAY "PRONTUARIO: " AT 0605 wspront AT 0625.
      *DISPLAY "VALOR DA HORA: " AT 0705 wsvlhr AT 0725.
      * DISPLAY "QTDE H/TRABALHADAS: " AT 0805 wsqthr AT 0825.
      * DISPLAY "QTDE DE DEP: " AT 0905 wsqtdep AT 0925.
      * DISPLAY " ".
      * DISPLAY "SALARIO BRUTO: " AT 1105 wsbto.
      * DISPLAY "INSS: " AT 1205 wsinss.
      * DISPLAY "IRRF: " AT 1305 wsir.
      * DISPLAY "SALARIO LIQUIDO: " AT 1405 wsliq.
      * DISPLAY " ".
       STOP RUN.
      *----------------------------------------------------------------*
