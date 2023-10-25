      ******************************************************************
      * Author: Pedro Veiga
      * Date: 23/10/2023
      * Purpose: TP05ex1 COBOL
      * Tectonics: cobc
      ******************************************************************
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP05ex1.
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

       77 wsloop PIC 9 value 1.
       77 wsloop2 PIC 9 value 1.

      *----------------------------------------------------------------*

       SCREEN SECTION.
      *----------------------------------------------------------------*

       PROCEDURE DIVISION.

       PRINCIPAL.
           perform loop1 until wsloop equal to 3.
       loop1.
           DISPLAY wsloop with no advancing.
           ADD 1 to wsloop.
           MOVE 1 to wsloop2.
           perform loop2 until wsloop2 equal to 6.
       loop2.
           if wsloop2 equal to 6 then
               stop run.
           DISPLAY "  " with no advancing.
           DISPLAY wsloop2 with no advancing.
           ADD 1 to wsloop2.
           if wsloop2 equal to 6 then
               DISPLAY "  ".

       END PROGRAM TP05ex1.
