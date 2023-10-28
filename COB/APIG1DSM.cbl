       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIG1DSM.
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
       COPY MBLG1SM.
       COPY APIG1DWK.

       77 MAP02O   PIC X.
       77 MAP      PIC 9(02) VALUE 1.
       77 TRANS-ID PIC X(04) VALUE 'G1SM'.
       77 MAPSET   PIC X(07) VALUE 'MBLG1SM'.
       77 PGM-NAME PIC X(08) VALUE 'APIG1DSM'.
       77 PF3-PGM  PIC X(08) VALUE 'APIG1DMP'.

      * SOUS-PARAGRAPHES

       77 V-TITLE    PIC X(41).
       77 V-LABEL    PIC X(42).
       77 V-MAP      PIC X(06) VALUE 'ABCDEF'.

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
           MOVE CHOICEI TO CHOICE-2
           .

       P-ON-DISPLAY.
           EVALUATE TRUE
               WHEN ALBUMS
                   MOVE 'GESTION DES ALBUMS' TO V-TITLE
                   MOVE 'UN ALBUM' TO V-LABEL
                   PERFORM P-INIT-BLUE-FIELDS
               WHEN CHANSONS
                   MOVE 'GESTION DES CHANSONS' TO V-TITLE
                   MOVE 'UNE CHANSON' TO V-LABEL
                   PERFORM P-INIT-BLUE-FIELDS
               WHEN PERSONNES
                   MOVE 'GESTION DES PERSONNES' TO V-TITLE
                   MOVE 'UNE PERSONNE' TO V-LABEL
                   PERFORM P-INIT-BLUE-FIELDS
               WHEN CONTRATS
                   MOVE 'GESTION DES CONTRATS' TO V-TITLE
                   MOVE 'UN CONTRAT' TO V-LABEL
                   PERFORM P-INIT-BLUE-FIELDS
               WHEN VENTES
                   MOVE 'GESTION DES VENTES' TO V-TITLE
                   MOVE 'UNE VENTE' TO V-LABEL
                   PERFORM P-INIT-BLUE-FIELDS
               WHEN MAISON-DI
                   MOVE 'GESTION DES MAISONS DE DISTRIBUTION' TO V-TITLE
                   MOVE 'UNE MAISON DE DISTRIBUTION' TO V-LABEL
                   PERFORM P-INIT-BLUE-FIELDS
           END-EVALUATE
           .

       P-INIT-BLUE-FIELDS.
           MOVE V-TITLE TO TITRFLDO
           MOVE V-LABEL TO CREFLDO
                           CONFLDO
                           SUPFLDO
                           MODFLDO
                           LSTFLDO
           .

       P-ON-SUBMIT.
           EVALUATE TRUE
               WHEN CHOICEI = LOW-VALUE
                   PERFORM P-EMPTY-CHOICE
               WHEN CHOICEI IS NUMERIC AND CHOICEI > 6
                   PERFORM P-BAD-CHOICE
               WHEN NOT CONTRATS AND CHOICEI = 5
                   PERFORM P-NOT-AVAILABLE
               WHEN ALBUMS    AND CHOICEI = 1 OR 4
                   PERFORM P-GOTO-2
               WHEN ALBUMS    AND CHOICEI = 2 OR 3
                   PERFORM P-GOTO-1
               WHEN CHANSONS  AND CHOICEI = 2 OR 3
                   PERFORM P-GOTO-1
               WHEN CHANSONS  AND CHOICEI = 1 OR 4
                   PERFORM P-GOTO-2
               WHEN PERSONNES AND CHOICEI = 2 OR 3
                   PERFORM P-GOTO-1
               WHEN PERSONNES AND CHOICEI = 1 OR 4
                   PERFORM P-GOTO-2
               WHEN CONTRATS  AND CHOICEI = 2 OR 3
                   PERFORM P-GOTO-1
               WHEN CONTRATS  AND CHOICEI = 1 OR 4
                   PERFORM P-GOTO-2
               WHEN CONTRATS  AND CHOICEI = 5
                   PERFORM P-GOTO-3
               WHEN VENTES    AND CHOICEI = 1
                   PERFORM P-GOTO-2
               WHEN VENTES
                   PERFORM P-NOT-AVAILABLE
               WHEN MAISON-DI AND CHOICEI = 2 OR 3
                   PERFORM P-GOTO-1
               WHEN MAISON-DI AND CHOICEI = 1 OR 4
                   PERFORM P-GOTO-2
               WHEN CHOICEI = 'L'
                   PERFORM P-LIST
               WHEN OTHER
                   PERFORM P-BAD-CHOICE
           END-EVALUATE
           .

       P-LIST.
           MOVE PREFIX TO DEST-PGM
           MOVE 'D3' TO DEST-PGM(7:2)
           .

      * SI CHOIX 2 OU 3 (CONSULTATION OU SUPPRESSION)
       P-GOTO-1.
           MOVE PREFIX            TO DEST-PGM
           MOVE V-MAP(CHOICE-1:1) TO DEST-PGM(7:1)
           MOVE '1'               TO DEST-PGM(8:1)
           .

      * SI CHOIX 1 OU 4 (CREATION OU MODIFICATION)
       P-GOTO-2.
           MOVE PREFIX            TO DEST-PGM
           MOVE V-MAP(CHOICE-1:1) TO DEST-PGM(7:1)
           MOVE '2'               TO DEST-PGM(8:1)
           .

      * SI CHOIX 3 (LISTE)
       P-GOTO-3.
           MOVE PREFIX            TO DEST-PGM
           MOVE V-MAP(CHOICE-1:1) TO DEST-PGM(7:1)
           MOVE '3'               TO DEST-PGM(8:1)
           .

       P-GOOD-CHOICE.
           MOVE ERR-NOT-AVAIL TO MSGFLDO
           MOVE PGM-NAME TO DEST-PGM
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
