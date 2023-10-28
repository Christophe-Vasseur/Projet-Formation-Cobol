      *======================================================*
      *   SOUS-PROGRAMME DE MODIFICATION D UNE CHANSON       *
      *======================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VB4.
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

       77 RC-1        PIC S9(8) COMP.
       77 RC-2        PIC S9(8) COMP.
       01 CODE-ERR.
           02 CODE-ERR-1 PIC 9(04).
           02 CODE-ERR-2 PIC 9(04).

       01  W-CHANSON.
           05 W-CH-CODEC           PIC X(04).
           05 W-CH-CODEA           PIC X(04).
           05 W-CH-TITREC          PIC X(40).
           05 W-CH-CODE-MODIF      PIC 9(01).
           05 FILLER               PIC X(31).
       01  W2-CHANSON.
           05 W2-CH-CODEC          PIC X(04).
           05 W2-CH-CODEA          PIC X(04).
           05 W2-CH-TITREC         PIC X(40).
           05 FILLER               PIC X(32).

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
           INITIALIZE W-CHANSON
           INITIALIZE W2-CHANSON

      *    RECEPTION DES DONNEES

           EXEC CICS
               RECEIVE INTO(W-CHANSON)
           END-EXEC

           EVALUATE W-CH-CODE-MODIF
      *    CONSULTATION DU FICHIER VSAM A PARTIR DU CODE CHANSON
               WHEN 0
                   EXEC CICS
                       READ FILE('CHANSOG1')
                       INTO (W2-CHANSON)
                       RIDFLD (W-CH-CODEC)
                       RESP (RC-1)
                       RESP2 (RC-2)
                       UPDATE
                   END-EXEC

                   MOVE W2-CHANSON TO W-CHANSON

                   EVALUATE RC-1
                       WHEN DFHRESP(NORMAL)
                           MOVE 1 TO W-CH-CODE-MODIF
                       WHEN DFHRESP(NOTFND)
                           MOVE 2 TO W-CH-CODE-MODIF
                       WHEN OTHER
                           MOVE 3 TO W-CH-CODE-MODIF
                   END-EVALUATE

                   EXEC CICS
                       RETURN
                       INPUTMSG(W-CHANSON)
                       INPUTMSGLEN(LENGTH OF W-CHANSON)
                   END-EXEC

               WHEN 1
                   EXEC CICS
                       READ FILE('CHANSOG1')
                       RIDFLD (W-CH-CODEC)
                       INTO (W2-CHANSON)
                       UPDATE
                   END-EXEC
      *                RESP (RC-1)
      *                RESP2 (RC-2)

                   MOVE W-CH-CODEC  TO W2-CH-CODEC
                   MOVE W-CH-CODEA  TO W2-CH-CODEA
                   MOVE W-CH-TITREC TO W2-CH-TITREC

                   EXEC CICS
                       REWRITE FILE('CHANSOG1')
                       FROM (W2-CHANSON)
                       LENGTH (LENGTH OF W2-CHANSON)
                       RESP (RC-1)
                       RESP2 (RC-2)
                   END-EXEC

                   EVALUATE RC-1
                       WHEN DFHRESP(NORMAL)
                           MOVE W2-CHANSON TO W-CHANSON
                           MOVE 1 TO W-CH-CODE-MODIF
                       WHEN DFHRESP(NOTFND)
                           MOVE 2 TO W-CH-CODE-MODIF
                       WHEN OTHER
                           MOVE 3 TO W-CH-CODE-MODIF
                   END-EVALUATE

                   EXEC CICS
                       RETURN
                       INPUTMSG (W-CHANSON)
                       INPUTMSGLEN (LENGTH OF W-CHANSON)
                   END-EXEC

           END-EVALUATE

           GOBACK
           .
