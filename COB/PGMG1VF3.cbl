      *======================================================*
      *   SOUS-PROGRAMME DE CREATION D UNE CHANSON           *
      *======================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VF3.
       AUTHOR. RBN.
       DATE-WRITTEN. 21/10/23.

      *======================================================*
      *   E N V I R O N M E N T       D I V I S I O N        *
      *======================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.

      *======================================================*
      *           D A T A         D I V I S I O N            *
      *======================================================*

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77 RC-1 PIC S9(8) COMP.
       77 RC-2 PIC S9(8) COMP.
       01 CODE-ERR.
           02 CODE-ERR-1 PIC 9(04).
           02 CODE-ERR-2 PIC 9(04).

       01  W-MAISON-DI.
           05 W-MA-SIREN           PIC X(05).
           05 W-MA-RAISON-SOC      PIC X(30).
           05 W-MA-ADRESSE         PIC X(15).
           05 W-MA-CODEP-DIR       PIC X(04).
           05 W-MA-CODE-SUPP       PIC 9(01).
           05 FILLER               PIC X(25).

      *------------------------------------------------------*
      *   ZONE DE MESSAGE TAMPON POUR LE SEND FROM           *
      *------------------------------------------------------*

       COPY MAISONDI.

      *======================================================*
      *     P R O C E D U R E     D I V I S I O N            *
      *======================================================*

       PROCEDURE DIVISION.

      *    INITIALISATION

           INITIALIZE E-MAISON-DIST
           INITIALIZE W-MAISON-DI

      *    RECEPTION DES DONNEES

           EXEC CICS
               RECEIVE
               INTO (E-MAISON-DIST)
           END-EXEC

           EXEC CICS
               READ
               FILE ('MAISONG1')
               INTO (E-MAISON-DIST)
               RIDFLD (MA-SIREN)
               UPDATE
           END-EXEC

           MOVE E-MAISON-DIST TO W-MAISON-DI

           EXEC CICS
               DELETE
               FILE ('MAISONG1')
               RESP (RC-1)
               RESP2 (RC-2)
           END-EXEC

           EVALUATE RC-1
               WHEN DFHRESP(NORMAL)
                   MOVE 1 TO W-MA-CODE-SUPP
               WHEN DFHRESP(DUPREC)
                   MOVE 2 TO W-MA-CODE-SUPP
               WHEN OTHER
                   MOVE RC-1 TO W-MA-CODE-SUPP
      *            MOVE RC-2 TO CODE-ERR-2
           END-EVALUATE

           EXEC CICS
               RETURN
               INPUTMSG (W-MAISON-DI)
               INPUTMSGLEN (LENGTH OF W-MAISON-DI)
           END-EXEC

           GOBACK
           .
