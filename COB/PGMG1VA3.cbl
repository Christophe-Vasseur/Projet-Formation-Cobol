      *======================================================*
      *   SOUS-PROGRAMME DE CREATION D UNE CHANSON           *
      *======================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VA3.
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

       01  W-ALBUM.
           05 W-AL-CODEA           PIC X(04).
           05 W-AL-TITREA          PIC X(30).
           05 W-AL-CODE-SUPP       PIC 9(01).
           05 FILLER               PIC X(45).

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

      *    RECEPTION DES DONNEES

           EXEC CICS
               RECEIVE
               INTO (E-ALBUM)
           END-EXEC
           EXEC CICS
               READ
               FILE ('ALBUMG1')
               INTO (E-ALBUM)
               RIDFLD (AL-CODEA)
               UPDATE
           END-EXEC

           MOVE E-ALBUM TO W-ALBUM

           EXEC CICS
               DELETE
               FILE ('ALBUMG1')
               RESP (RC-1)
               RESP2 (RC-2)
           END-EXEC

           EVALUATE RC-1
               WHEN DFHRESP(NORMAL)
                   MOVE 1 TO W-AL-CODE-SUPP
               WHEN DFHRESP(DUPREC)
                   MOVE 2 TO W-AL-CODE-SUPP
               WHEN OTHER
                   MOVE RC-1 TO W-AL-CODE-SUPP
      *            MOVE RC-2 TO CODE-ERR-2
           END-EVALUATE

           EXEC CICS
               RETURN
               INPUTMSG (W-ALBUM)
               INPUTMSGLEN (LENGTH OF W-ALBUM)
           END-EXEC

           GOBACK
           .
