      ******************************************************************
      * Author: Pedro Veiga
      * Date: 17/09/2023
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
      *VARIAVEIS COM MASCARA

       77 wspront PIC X(9).
       77 wsmascqthr PIC ZZZ.
       77 wsmascqtdep PIC ZZ.
       77 wsmascvldep PIC ZZZ,99.
       77 wsmascvlhr PIC ZZZ,99.

       01 nome.
         05 wsmascnm PIC A(20).
         05 wsmascsobnm PIC A(30).
       
      *VARIAVEIS PARA CALCULO

       77 wsvlhr PIC 999V99 value zero.
       77 wsqthr PIC 9(3).
       77 wsqtdep PIC 99.
       77 wsvldep PIC 999V99 value zero.
       77 wstemp PIC 99999V99 value zero.

       01 salario.
         05 wsbto PIC 99999V99 value zero.
         05 wsliq PIC 99999V99 value zero.
         05 wsref PIC 99999V99 value zero.
         05 wsinss PIC 99999V99 value zero.
         05 wsir PIC 99999V99 value zero.

       SCREEN SECTION.
       01 limpatela.
           05 blank screen.


      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       ENTRADA.
           DISPLAY "Digite seu nome: " no advancing.
           ACCEPT wsmascnm at 0118 with prompt character is "_" with 
           required.
           DISPLAY " ".
           DISPLAY "Digite seu sobrenome: " no advancing.
           ACCEPT wsmascsobnm at 0223 with prompt character is "_" with 
           required.
           DISPLAY " ".
           DISPLAY "Digite seu prontuario: " no advancing.
           ACCEPT wspront at 0324 with prompt character is "_" required.
           DISPLAY " ".
           DISPLAY "Digite o valor hora: R$" no advancing
           ACCEPT wsmascvlhr at 0424 with prompt character is "_" with 
           required.
           DISPLAY " ".
           DISPLAY "Digite qt de horas trabalhadas: " no advancing.
           ACCEPT wsmascqthr at 0533 with required.
           DISPLAY " ".
           DISPLAY "Digite a qt de dependentes: " no advancing.
           ACCEPT wsmascqtdep at 0629 with prompt character is "_" with 
           required.
           DISPLAY " ".
           DISPLAY "Digite o valor do dependente: R$" no advancing.
           ACCEPT wsmascvldep at 0733 with prompt character is "_" with 
           required.
           DISPLAY " ".

           MOVE wsmascqthr TO wsqthr.
           MOVE wsmascqtdep to wsqtdep.
           MOVE wsmascvldep to wsvldep.
           MOVE wsmascvlhr to wsvlhr.

       CALCULOS.
           COMPUTE wsbto = wsvlhr * wsqthr.
           COMPUTE wsinss = wsbto * 0,14.
           COMPUTE wsir = wsbto * 0,275.
           COMPUTE wsref = wsbto - wsinss.
           COMPUTE wsliq = wsref - wsir.
           COMPUTE wstemp = wsvldep * wsqtdep.
           COMPUTE wsliq = wsliq + wstemp.

           MOVE wsqthr to wsmascqthr.
           MOVE wsqtdep to wsmascqtdep.
           MOVE wsvldep to wsmascvldep.
           MOVE wsvlhr to wsmascvlhr.
           DISPLAY limpatela.
       SAIDA.
           DISPLAY "TP02 - CALCULO DE SALARIO" at 0123.
           DISPLAY "NOME: " at 0303 wsmascnm at 0323.
           DISPLAY "SOBRENOME: " at 0403 wsmascsobnm at 0423.
           DISPLAY "PRONTUARIO: " at 0503 wspront at 0523.
           DISPLAY "VALOR DA HORA: " at 0603 wsmascvlhr at 0623.
           DISPLAY "QTDE H/TRABALHADAS: " at 0703 wsmascqthr at 0723.
           DISPLAY "QTDE DE DEP: " at 0803 wsmascqtdep at 0823.
           DISPLAY " ".
           DISPLAY "SALARIO BRUTO: " at 1003 wsbto at 1023.
           DISPLAY "INSS: " at 1103 wsinss at 1123.
           DISPLAY "IRRF: " at 1203 wsir at 1223. 
           DISPLAY "SALARIO LIQUIDO: " at 1303 wsliq at 1323.
           DISPLAY " ".
           DISPLAY wsmascnm at 1501.
           DISPLAY wsmascsobnm at 1521.
           DISPLAY wspront at 1546.
           DISPLAY wsbto at 1555.
           DISPLAY wsliq at 1562.
           DISPLAY " ".
           STOP "Pressione enter...".
           STOP RUN.
      *----------------------------------------------------------------*
