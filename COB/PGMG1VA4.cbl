      *======================================================*
      *   SOUS-PROGRAMME DE MODIFICATION D UN ALBUM          *
      *======================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VA4.
       AUTHOR. REMICHRIS.
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
       01  W-ALBUM.
           05 W-AL-CODEA           PIC X(04).
           05 W-AL-TITREA          PIC X(30).
           05 W-AL-CODE-MODIF      PIC 9(01).
           05 FILLER               PIC X(45).
       01  W2-ALBUM.
           05 W2-AL-CODEA          PIC X(04).
           05 W2-AL-TITREA         PIC X(30).
           05 FILLER               PIC X(46).

      *------------------------------------------------------*
      *   ZONE DE MESSAGE TAMPON POUR LE SEND FROM           *
      *------------------------------------------------------*

       COPY ALBUM.

      *======================================================*
      *     P R O C E D U R E     D I V I S I O N            *
      *======================================================*

       PROCEDURE DIVISION.

      *    INITIALISATION

           INITIALIZE E-ALBUM
           INITIALIZE W-ALBUM
           INITIALIZE W2-ALBUM

      *    RECEPTION DES DONNEES

           EXEC CICS
               RECEIVE INTO(W-ALBUM)
           END-EXEC

           EVALUATE W-AL-CODE-MODIF
      *    CONSULTATION DU FICHIER VSAM A PARTIR DU CODE ALBUM
               WHEN 0
                   EXEC CICS
                       READ FILE('ALBUMG1')
                       INTO (W2-ALBUM)
                       RIDFLD (W-AL-CODEA)
                       RESP (RC-1)
                       RESP2 (RC-2)
                       UPDATE
                   END-EXEC

                   MOVE W2-ALBUM TO W-ALBUM

                   EVALUATE RC-1
                       WHEN DFHRESP(NORMAL)
                           MOVE 1 TO W-AL-CODE-MODIF
                       WHEN DFHRESP(NOTFND)
                           MOVE 2 TO W-AL-CODE-MODIF
                       WHEN OTHER
                           MOVE 3 TO W-AL-CODE-MODIF
                   END-EVALUATE

                   EXEC CICS
                       RETURN
                       INPUTMSG(W-ALBUM)
                       INPUTMSGLEN(LENGTH OF W-ALBUM)
                   END-EXEC

               WHEN 1
                   EXEC CICS
                       READ FILE('ALBUMG1')
                       RIDFLD (W-AL-CODEA)
                       INTO (W2-ALBUM)
                       UPDATE
                   END-EXEC
      *                RESP (RC-1)
      *                RESP2 (RC-2)

                   MOVE W-AL-CODEA  TO W2-AL-CODEA
                   MOVE W-AL-TITREA TO W2-AL-TITREA

                   EXEC CICS
                       REWRITE FILE('ALBUMG1')
                       FROM (W2-ALBUM)
                       LENGTH (LENGTH OF W2-ALBUM)
                       RESP (RC-1)
                       RESP2 (RC-2)
                   END-EXEC

                   EVALUATE RC-1
                       WHEN DFHRESP(NORMAL)
                           MOVE W2-ALBUM TO W-ALBUM
                           MOVE 1 TO W-AL-CODE-MODIF
                       WHEN DFHRESP(NOTFND)
                           MOVE 2 TO W-AL-CODE-MODIF
                       WHEN OTHER
                           MOVE 3 TO W-AL-CODE-MODIF
                   END-EVALUATE

                   EXEC CICS
                       RETURN
                       INPUTMSG (W-ALBUM)
                       INPUTMSGLEN (LENGTH OF W-ALBUM)
                   END-EXEC

           END-EVALUATE

           GOBACK
           .
