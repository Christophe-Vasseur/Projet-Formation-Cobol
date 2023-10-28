      * ////////////////////////////////////////////////////////////// *
      *                                                                *
      *                  ECRAN DE LISTE DES CONTRATS                   *
      *                                                                *
      * ////////////////////////////////////////////////////////////// *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIG1DD3.
       AUTHOR. EDR.
       DATE-WRITTEN. 23/10/23.

      * ============================================================== *
      *                                                                *
      *            E N V I R O N M E N T   D I V I S I O N             *
      *                                                                *
      * ============================================================== *

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      * ============================================================== *
      *                                                                *
      *                   D A T A   D I V I S I O N                    *
      *                                                                *
      * ============================================================== *

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       COPY DFHAID.
       COPY DFHBMSCA.
       COPY MBLG1D3.
       COPY APIG1DWK.

       77 MAP02O   PIC X.
       77 MAP      PIC 9(02) VALUE 1.
       77 TRANS-ID PIC X(04) VALUE 'G1D3'.
       77 MAPSET   PIC X(07) VALUE 'MBLG1D3'.
       77 PGM-NAME PIC X(08) VALUE 'APIG1DD3'.
       77 PF3-PGM  PIC X(08) VALUE 'APIG1DSM'.

      * SOUS-PARAGRAPHES

       COPY CONTRAT.

       01 DROITS-PICX.
           02 DROITS-INT PIC 9(03).
           02 DROITS-DEC PIC 9(02).
       01 DROITS-PIC9 REDEFINES
           DROITS-PICX PIC 9(03)V9(02).
       01 E-RC.
           02 E-RC-1 PIC 9(04).
           02 E-RC-2 PIC 9(04).

      * SCREEN

       77 I PIC 9(02).
       77 J PIC 9(02).
       77 LIST-H PIC 9(02) VALUE 14.

       COPY COLLEC.

       LINKAGE SECTION.

       01 DFHCOMMAREA.
           05 LK-COMM-AREA PIC X(1024).

      * ============================================================== *
      *                                                                *
      *              P R O C E D U R E   D I V I S I O N               *
      *                                                                *
      * ============================================================== *

       PROCEDURE DIVISION.

       P-MAIN.
           MOVE DFHRESP(MAPFAIL) TO RC-MAPFAIL.
           PERFORM P-BRWS-MAIN
           .

       COPY APIG1DPC.

      * -------------------------------------------------------------- *
      *                                                                *
      *                        SOUS-PARAGRAPHES                        *
      *                                                                *
      * -------------------------------------------------------------- *

       P-ON-INIT.
           EXIT
           .

       P-ON-LAND.
           MOVE 1 TO PAGE-NUM
           MOVE LOW-VALUE TO E-CO-KEY
           PERFORM P-CICS-START-BR
           PERFORM P-CICS-READ-NEXT
           EVALUATE TRUE
               WHEN CICS-RESP-NORMAL
                   PERFORM P-DISPLAY-TOP-DOWN
               WHEN CICS-RESP-ENDFILE
                   MOVE ERR-FILE-EMPTY TO MSGFLDO
               WHEN OTHER
                   MOVE ERR-UNKNOWN TO MSGFLDO
           END-EVALUATE
           .

       P-ON-SUBMIT.
           MOVE PGM-NAME TO DEST-PGM
           .

       P-ON-DISPLAY.
           EVALUATE TRUE
               WHEN SCROLL-DOWN
                   IF NEXT-PAGE-EXISTS THEN
                       ADD 1 TO PAGE-NUM
                       MOVE LST-KEY TO E-CO-KEY
                       PERFORM P-DISPLAY-TOP-DOWN
                   ELSE
                       MOVE ERR-LST-PAGE TO MSGFLDO
                   END-IF
               WHEN SCROLL-UP
                   IF PAGE-NUM > 1 THEN
                       SET NEXT-PAGE-EXISTS TO TRUE
                       SUBTRACT 1 FROM PAGE-NUM
                       MOVE FST-KEY TO E-CO-KEY
                       PERFORM P-DISPLAY-BOTTOM-UP
                   ELSE
                       MOVE ERR-FST-PAGE TO MSGFLDO
                   END-IF
               WHEN OTHER
                   PERFORM P-NOTHING
           END-EVALUATE
           .

       P-DISPLAY-TOP-DOWN.
           PERFORM P-ERASE-LIST
           MOVE 0 TO J
           MOVE 0 TO I
           SET NOT-NEXT-PAGE-EXISTS TO TRUE
           PERFORM P-CICS-START-BR
           PERFORM UNTIL
               I = LIST-H - 1 OR
               CICS-RESP-ENDFILE OR
               J > LIST-H - 1
      *    END UNTIL
               ADD 1 TO J
               PERFORM P-CICS-READ-NEXT
               IF NOT CICS-RESP-ENDFILE THEN
                   ADD 1 TO I
                   PERFORM P-DISPLAY-CONTRAT
                   IF I = 1 THEN
                       MOVE E-CO-KEY TO FST-KEY
                   END-IF
                   IF I = LIST-H - 1 THEN
                       MOVE E-CO-KEY TO LST-KEY
                       PERFORM P-CICS-READ-NEXT
                       IF CICS-RESP-NORMAL THEN
                           SET NEXT-PAGE-EXISTS TO TRUE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF J > LIST-H - 1 THEN
               MOVE 'INDEX OVERFLOW§ INFINITE LOOP CANCELED?'
                   TO MSGFLDO
           END-IF
           .

       P-DISPLAY-BOTTOM-UP.
           PERFORM P-ERASE-LIST
           MOVE 0 TO J
           MOVE LIST-H TO I
           PERFORM P-CICS-START-BR
           PERFORM UNTIL
               I = 1 OR
               NOT CICS-RESP-NORMAL
               OR J > LIST-H - 1
      *    END UNTIL
               ADD 1 TO J
               PERFORM P-CICS-READ-PREV
               IF CICS-RESP-NORMAL THEN
                   SUBTRACT 1 FROM I
                   PERFORM P-DISPLAY-CONTRAT
                   IF I = 1 THEN
                       MOVE E-CO-KEY TO FST-KEY
                   END-IF
                   IF I = LIST-H - 1 THEN
                       MOVE E-CO-KEY TO LST-KEY
                   END-IF
               END-IF
           END-PERFORM
           IF J > LIST-H - 1 THEN
               MOVE 'INDEX OVERFLOW§ INFINITE LOOP CANCELED?'
                   TO MSGFLDO
           END-IF
           .

       P-DISPLAY-CONTRAT.
           MOVE E-CO-CODEA TO O-CO-CODEA
           MOVE E-CO-CODEP TO O-CO-CODEP
           MOVE E-CO-SIREN TO O-CO-SIREN
           MOVE E-CO-FONCTION TO O-CO-FONCTION
           EVALUATE I
               WHEN  1 MOVE O-CO-KEY TO ITEM01O
               WHEN  2 MOVE O-CO-KEY TO ITEM02O
               WHEN  3 MOVE O-CO-KEY TO ITEM03O
               WHEN  4 MOVE O-CO-KEY TO ITEM04O
               WHEN  5 MOVE O-CO-KEY TO ITEM05O
               WHEN  6 MOVE O-CO-KEY TO ITEM06O
               WHEN  7 MOVE O-CO-KEY TO ITEM07O
               WHEN  8 MOVE O-CO-KEY TO ITEM08O
               WHEN  9 MOVE O-CO-KEY TO ITEM09O
               WHEN 10 MOVE O-CO-KEY TO ITEM10O
               WHEN 11 MOVE O-CO-KEY TO ITEM11O
               WHEN 12 MOVE O-CO-KEY TO ITEM12O
               WHEN 13 MOVE O-CO-KEY TO ITEM13O
               WHEN 14 MOVE O-CO-KEY TO ITEM14O
               WHEN 15 MOVE O-CO-KEY TO ITEM15O
           END-EVALUATE
           .

       P-ERASE-LIST.
           MOVE SPACES TO
               ITEM01O
               ITEM02O
               ITEM03O
               ITEM04O
               ITEM05O
               ITEM06O
               ITEM07O
               ITEM08O
               ITEM09O
               ITEM10O
               ITEM11O
               ITEM12O
               ITEM13O
               ITEM14O
               ITEM15O
           .

       P-NOTHING.
           EXIT
           .

       P-ON-LEAVE.
           EXIT
           .

      * -------------------------------------------------------------- *
      *                                                                *
      *                         COMMANDES CICS                         *
      *                                                                *
      * -------------------------------------------------------------- *

       P-CICS-START-BR.
           EXEC CICS
               STARTBR
               FILE (F-CONTRAT)
               RIDFLD (E-CO-KEY)
               KEYLENGTH (LENGTH OF E-CO-KEY)
               RESP (W-CO-RC-CICS-1)
               RESP2 (W-CO-RC-CICS-2)
           END-EXEC
           .

       P-CICS-READ-PREV.
           EXEC CICS
               READPREV
               FILE (F-CONTRAT)
               INTO (E-CONTRAT)
               LENGTH (LENGTH OF E-CONTRAT)
               RIDFLD (E-CO-KEY)
               KEYLENGTH (LENGTH OF E-CO-KEY)
               RESP (W-CO-RC-CICS-1)
               RESP2 (W-CO-RC-CICS-2)
           END-EXEC
           .

       P-CICS-READ-NEXT.
           EXEC CICS READNEXT
               FILE (F-CONTRAT)
               INTO (E-CONTRAT)
               LENGTH (LENGTH OF E-CONTRAT)
               RIDFLD (E-CO-KEY)
               KEYLENGTH (LENGTH OF E-CO-KEY)
               RESP (W-CO-RC-CICS-1)
               RESP2 (W-CO-RC-CICS-2)
           END-EXEC
           .

       P-CICS-TIME.
           EXEC CICS ASKTIME
               ABSTIME (DATE-TMP)
           END-EXEC
           EXEC CICS FORMATTIME
               ABSTIME (DATE-TMP)
               DDMMYYYY (DATEFLDO)
               DATESEP ('/')
           END-EXEC
           EXEC CICS FORMATTIME
               ABSTIME (DATE-TMP)
               TIME (TIMEFLDO)
               TIMESEP (':')
           END-EXEC
           .

       P-CICS-XCTL.
           EXEC CICS
               XCTL
               PROGRAM (NEXT-PGM)
               COMMAREA (WK-COMM-AREA)
               LENGTH (LENGTH OF WK-COMM-AREA)
           END-EXEC
           .

       P-CICS-RECV.
           EXEC CICS
               RECEIVE
               MAPSET (MAPSET)
               MAP ('MAP01')
               RESP (RC-1)
           END-EXEC
           .

       P-CICS-SEND.
           IF PREV-PGM = PGM-NAME THEN
               EXEC CICS
                   SEND
                   MAPSET (MAPSET)
                   MAP ('MAP01')
                   FROM (MAP01O)
                   LENGTH (LENGTH OF MAP01O)
               END-EXEC
           ELSE
               EXEC CICS
                   SEND
                   MAPSET (MAPSET)
                   MAP ('MAP01')
                   FROM (MAP01O)
                   LENGTH (LENGTH OF MAP01O)
                   ERASE
               END-EXEC
           END-IF
           .

       P-CICS-WAIT.
           EXEC CICS
               RETURN
               TRANSID (TRANS-ID)
               COMMAREA (WK-COMM-AREA)
               LENGTH (LENGTH OF WK-COMM-AREA)
           END-EXEC
           .

       P-CICS-BBYE.
           EXEC CICS
               SEND
               FROM (GOODBYE)
               LENGTH (LENGTH OF GOODBYE)
               ERASE
           END-EXEC
           .

       P-CICS-QUIT.
           EXEC CICS
               RETURN
           END-EXEC
           .
