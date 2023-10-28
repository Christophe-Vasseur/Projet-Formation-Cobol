       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIG1DMP.
       AUTHOR. EDR.
       DATE-WRITTEN. 21/10/23.

      * ============================================================== *
      *                                                                *
      *                   D A T A   D I V I S I O N                    *
      *                                                                *
      * ============================================================== *

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       COPY DFHAID.
       COPY DFHBMSCA.
       COPY MBLG1MP.
       COPY APIG1DWK.

       77 MAP      PIC 9(02) VALUE 1.
       77 TRANS-ID PIC X(04) VALUE 'G1MP'.
       77 MAPSET   PIC X(07) VALUE 'MBLG1MP'.
       77 PGM-NAME PIC X(08) VALUE 'APIG1DMP'.
       77 PF3-PGM  PIC X(08) VALUE SPACES.

      * SOUS-PARAGRAPHES

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

       P-ON-LAND.
           EXIT
           .

       P-ON-INIT.
           MOVE 0 TO CHOICE-1
           .

       P-ON-DISPLAY.
           EXIT
           .

       P-ON-SUBMIT.
           EVALUATE TRUE
               WHEN CHOICEI = LOW-VALUE
                   PERFORM P-EMPTY-CHOICE
               WHEN CHOICEI = 1 OR 2 OR 3 OR 4 OR 5 OR 6
                   PERFORM P-GOOD-CHOICE
               WHEN CHOICEI = 'X'
                   PERFORM P-CHOICE-X
               WHEN OTHER
                   PERFORM P-BAD-CHOICE
           END-EVALUATE
           .

       P-CHOICE-X.
           MOVE 2 TO MAP
           MOVE PGM-NAME TO DEST-PGM
           .

       P-GOOD-CHOICE.
           MOVE CHOICEI TO CHOICE-1
           MOVE PREFIX TO DEST-PGM
           MOVE 'SM' TO DEST-PGM(7:2)
           .

       P-NOT-AVAILABLE.
           MOVE ERR-NOT-AVAIL TO MSGFLDO
           MOVE PGM-NAME TO DEST-PGM
           .

       P-BAD-CHOICE.
           MOVE ERR-BAD-CHOICE TO MSGFLDO
           MOVE PGM-NAME TO DEST-PGM
           .

       P-EMPTY-CHOICE.
           MOVE ERR-NO-CHOICE TO MSGFLDO
           MOVE PGM-NAME TO DEST-PGM
           .

       P-ON-LEAVE.
           EXIT
           .

      * -------------------------------------------------------------- *
      *                                                                *
      *                         COMMANDES CICS                         *
      *                                                                *
      * -------------------------------------------------------------- *

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
           EVALUATE MAP
               WHEN     1 PERFORM P-CICS-RECV-01
               WHEN     2 PERFORM P-CICS-RECV-02
               WHEN OTHER PERFORM P-CICS-RECV-01
           END-EVALUATE
           .

       P-CICS-RECV-01.
           EXEC CICS
               RECEIVE
               MAPSET (MAPSET)
               MAP ('MAP01')
               RESP (RC-1)
           END-EXEC
           .

       P-CICS-RECV-02.
           EXEC CICS
               RECEIVE
               MAPSET (MAPSET)
               MAP ('MAP02')
               RESP (RC-1)
           END-EXEC
           .

       P-CICS-SEND.
           EVALUATE MAP
               WHEN     1 PERFORM P-CICS-SEND-01
               WHEN     2 PERFORM P-CICS-SEND-02
               WHEN OTHER PERFORM P-CICS-SEND-01
           END-EVALUATE
           .

       P-CICS-SEND-01.
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

       P-CICS-SEND-02.
           IF PREV-PGM = PGM-NAME THEN
               EXEC CICS
                   SEND
                   MAPSET (MAPSET)
                   MAP ('MAP02')
                   FROM (MAP02O)
                   LENGTH (LENGTH OF MAP02O)
               END-EXEC
           ELSE
               EXEC CICS
                   SEND
                   MAPSET (MAPSET)
                   MAP ('MAP02')
                   FROM (MAP02O)
                   LENGTH (LENGTH OF MAP02O)
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
