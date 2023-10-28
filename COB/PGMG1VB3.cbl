      *======================================================*
      *   SOUS-PROGRAMME DE CREATION D UNE CHANSON           *
      *======================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VB3.
       AUTHOR. RBN.
       DATE-WRITTEN. 19/10/23.

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

       01  W-CHANSON.
           05 W-CH-CODEC           PIC X(04).
           05 W-CH-CODEA           PIC X(04).
           05 W-CH-TITREC          PIC X(40).
           05 W-CH-CODE-SUPP       PIC 9(01).
           05 FILLER               PIC X(31).

      *------------------------------------------------------*
      *   ZONE DE MESSAGE TAMPON POUR LE SEND FROM           *
      *------------------------------------------------------*

       COPY CHANSON.

      *======================================================*
      *     P R O C E D U R E     D I V I S I O N            *
      *======================================================*

       PROCEDURE DIVISION.

      *    INITIALISATION

           INITIALIZE E-CHANSON

      *    RECEPTION DES DONNEES

           EXEC CICS
               RECEIVE
               INTO (E-CHANSON)
           END-EXEC

           EXEC CICS
               READ
               FILE ('CHANSOG1')
               INTO (E-CHANSON)
               RIDFLD (CH-CODEC)
               UPDATE
           END-EXEC

           MOVE E-CHANSON TO W-CHANSON

           EXEC CICS
               DELETE
               FILE ('CHANSOG1')
               RESP (RC-1)
               RESP2 (RC-2)
           END-EXEC

           EVALUATE RC-1
               WHEN DFHRESP(NORMAL)
                   MOVE 1 TO W-CH-CODE-SUPP
               WHEN DFHRESP(DUPREC)
                   MOVE 2 TO W-CH-CODE-SUPP
               WHEN OTHER
                   MOVE RC-1 TO W-CH-CODE-SUPP
      *            MOVE RC-2 TO CODE-ERR-2
           END-EVALUATE

           EXEC CICS
               RETURN
               INPUTMSG (W-CHANSON)
               INPUTMSGLEN (LENGTH OF W-CHANSON)
           END-EXEC

           GOBACK
           .
