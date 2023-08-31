      ******************************************************************
      * Author: Pedro Veiga
      * Date: 21/08/2023
      * Purpose: TP01 COBOL
      * Tectonics: cobc
      ******************************************************************
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP01.
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
       77 wsprontuario PIC X(9).
       77 wsvalorhora PIC 999V99.
       77 wsqthoras PIC 9(3).
       77 wsqtdependentes PIC 99.
       77 wsvalordependente PIC 999V99.
       77 wstemp PIC 99999V99.
       01 nome.
       05 wsnome PIC A(20).
       05 wssobrenome PIC A(30).
       01 salario.
       05 wsbruto PIC 99999V99.
       05 wsliquido PIC 99999V99.
       05 wsref PIC 99999V99.
       05 wsdescontoinss PIC 99999V99.
       05 wsir PIC 99999V99.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       ENTRADA.
       DISPLAY "Digite seu nome: " WITH NO ADVANCING.
       ACCEPT wsnome.
       DISPLAY "Digite seu sobrenome: " WITH NO ADVANCING.
       ACCEPT wssobrenome.
       DISPLAY "Digite seu prontuario: " WITH NO ADVANCING.
       ACCEPT wsprontuario.
       DISPLAY "Digite o valor hora: R$" WITH NO ADVANCING.
       ACCEPT wsvalorhora WITH SIZE 3.
       DISPLAY "Digite qt de horas trabalhadas: " WITH NO ADVANCING.
       ACCEPT wsqthoras.
       DISPLAY "Digite a qt de dependentes: " WITH NO ADVANCING.
       ACCEPT wsqtdependentes.
       DISPLAY "Digite o valor do dependente: " WITH NO ADVANCING.
       ACCEPT wsvalordependente.
       DISPLAY " ".

       CALCULOS.
       MULTIPLY wsvalorhora by wsqthoras GIVING wsbruto.
       MULTIPLY wsbruto by 0,14 GIVING wsdescontoinss.
       MULTIPLY wsbruto by 0,275 GIVING wsir.
       SUBTRACT wsdescontoinss from wsbruto GIVING wsref.
       SUBTRACT wsir from wsref GIVING wsliquido.
       MULTIPLY wsvalordependente by wsqtdependentes GIVING wstemp.
       ADD wsliquido to wstemp GIVING wsliquido.

       SAIDA.
       DISPLAY "TP01 - CALCULO DE SALARIO" AT 0230.
       DISPLAY "NOME: " AT 0405 wsnome AT 0430.
       DISPLAY "SOBRENOME: " AT 0505 wssobrenome AT 0530.
       DISPLAY "PRONTUARIO: " AT 0605 wsprontuario AT 0630.
       DISPLAY "VALOR DA HORA: " AT 0705 wsvalorhora AT 0730.
       DISPLAY "QTDE H/TRABALHADAS: " AT 0805 wsqthoras AT 0830.
       DISPLAY "QTDE DE DEP: " AT 0905 wsqtdependentes AT 0930.
       DISPLAY " ".
       DISPLAY "SALARIO BRUTO: " AT 1105 wsbruto.
       DISPLAY "INSS: " AT 1205 wsdescontoinss.
       DISPLAY "IRRF: " AT 1305 wsir.
       DISPLAY "SALARIO LIQUIDO: " AT 1405 wsliquido.
       DISPLAY " ".
       DISPLAY wsnome AT 1601.
       DISPLAY wssobrenome AT 1622.
       DISPLAY wsprontuario AT 1650.
       DISPLAY wsbruto AT 1660.
       DISPLAY wsliquido AT 1668.
       STOP RUN.
      *----------------------------------------------------------------*
