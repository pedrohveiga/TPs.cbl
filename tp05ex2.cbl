      ******************************************************************
      * Author: Pedro Veiga
      * Date: 23/10/2023
      * Purpose: TP05ex2 COBOL
      * Tectonics: cobc
      ******************************************************************
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP05ex2.
       AUTHOR. Pedro Veiga.
       DATE-WRITTEN. 16/10/23.
       DATE-COMPILED. 16/10/23.
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
       77 wsnumini PIC 9999.
       77 wsnumfim PIC 9999.
       77 wsinc PIC 9999.
      *----------------------------------------------------------------*
       SCREEN SECTION.

       01 limpatela.
         05 blank screen.

      *----------------------------------------------------------------*

       PROCEDURE DIVISION.
       ENTRADA.
           DISPLAY "Insira o numero inicial: ".
           ACCEPT wsnumini.
           DISPLAY "Insira o numero final: ".
           ACCEPT wsnumfim.
           DISPLAY "Insira o incremento: ".
           ACCEPT wsinc.
           DISPLAY limpatela.
           perform CONTAGEM until wsnumini equal to wsnumfim.
       CONTAGEM.
           if wsnumini less than wsnumfim then
               DISPLAY "Numero da sequencia: "
               DISPLAY wsnumini
               add wsinc to wsnumini.
               if wsnumini equal to wsnumfim then
                   DISPLAY "Numero da sequencia: "
                   DISPLAY wsnumini.

           if wsnumini greater than wsnumfim then
               DISPLAY "Numero da sequencia: "
               DISPLAY wsnumini
               subtract wsinc from wsnumini
               if wsnumini equal to wsnumfim then
                   DISPLAY "Numero da sequencia: "
                   DISPLAY wsnumini.

       END PROGRAM TP05ex2.
