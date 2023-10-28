       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIG1DE1.
       AUTHOR. RAZ.
       DATE-WRITTEN. 23/10/23.

      * ============================================================== *
      *                                                                *
      *                   D A T A   D I V I S I O N                    *
      *                                                                *
      * ============================================================== *

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       COPY DFHAID.
       COPY DFHBMSCA.
       COPY MBLG1E1.
       COPY APIG1DWK.
       COPY VENTES.

       77 MAP02O   PIC X.
       77 MAP      PIC 9(02) VALUE 1.
       77 TRANS-ID PIC X(04) VALUE 'G1E1'.
       77 MAPSET   PIC X(07) VALUE 'MBLG1E1'.
       77 PGM-NAME PIC X(08) VALUE 'APIG1DE1'.
       77 PF3-PGM  PIC X(08) VALUE 'APIG1DSM'.

      * SOUS-PARAGRAPHES

       01 E-RC.
           02 E-RC-1 PIC 9(04).
           02 E-RC-2 PIC 9(04).

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
           EXIT
           .

       P-ON-DISPLAY.
           EVALUATE CHOICE-2
               WHEN 2
                   MOVE 'CONSULTATION DES VENTES' TO TITRFLDO
               WHEN 3
                   MOVE 'SUPPRESSION DE VENTES'   TO TITRFLDO
               WHEN OTHER
                   MOVE 'RETOURNEZ AU MENU PRECEDENT' TO MSGFLDO
            END-EVALUATE
           .

       P-ON-SUBMIT.
      *    ON FAIT DU SURPLACE
           MOVE PGM-NAME TO DEST-PGM
           EVALUATE CHOICE-2
               WHEN 2
                   PERFORM P-CONSULTATION-VENTES
               WHEN 3
                   PERFORM P-SUPPRESSION-VENTES
           END-EVALUATE
           .

       P-CONSULTATION-VENTES.

           MOVE CODEAI    TO VE-CODEA
           MOVE SIRENI    TO VE-SIREN
           MOVE DATVNTI   TO VE-DATE-VENTE
           MOVE PRIXVNTI  TO VE-PRIX
           MOVE QTEVNTI   TO VE-QTE
           EXEC CICS
               LINK
               PROGRAM ('PGMG1VE2')
               INPUTMSG (E-VENTES)
               INPUTMSGLEN (LENGTH OF E-VENTES)
           END-EXEC
           EXEC CICS
               RECEIVE
               INTO (W-VENTES)
           END-EXEC
           EVALUATE TRUE
               WHEN W-VE-RC-NORMAL
                   MOVE 'VENTE TROUVEE' TO MSGFLDO
                   PERFORM P-AFFICHER-VENTES
               WHEN W-VE-RC-NOTOPEN
                   MOVE 'FICHIER FERME' TO MSGFLDO
               WHEN W-VE-RC-NOTFND
                   MOVE 'VENTE NON TROUVEE' TO MSGFLDO
               WHEN OTHER
                   MOVE ERR-UNKNOWN TO MSGFLDO
      *            DEBUGGING PURPOSE ONLY§
      *            MOVE W-VE-RC-CICS-1 TO E-RC-1
      *            MOVE W-VE-RC-CICS-2 TO E-RC-2
      *            MOVE E-RC TO MSGFLDO
           END-EVALUATE
           .

       P-SUPPRESSION-VENTES.
           MOVE CODEAI    TO VE-CODEA
           MOVE SIRENI    TO VE-SIREN
           MOVE DATVNTI   TO VE-DATE-VENTE
           MOVE PRIXVNTI  TO VE-PRIX
           MOVE QTEVNTI   TO VE-QTE
           EXEC CICS
               LINK
               PROGRAM ('PGMG1VE3')
               INPUTMSG (E-VENTES)
               INPUTMSGLEN (LENGTH OF E-VENTES)
           END-EXEC
           EXEC CICS
               RECEIVE
               INTO (W-VENTES)
           END-EXEC
           EVALUATE TRUE
               WHEN W-VE-RC-NORMAL
                   MOVE 'VENTE SUPPRIMEE' TO MSGFLDO
                   PERFORM P-AFFICHER-VENTES
               WHEN W-VE-RC-NOTOPEN
                   MOVE 'FICHIER FERME' TO MSGFLDO
               WHEN W-VE-RC-NOTFND
                   MOVE 'VENTE NON TROUVEE' TO MSGFLDO
               WHEN OTHER
                   MOVE ERR-UNKNOWN TO MSGFLDO
      *            DEBUGGING PURPOSE ONLY§
      *            MOVE W-VE-RC-CICS-1 TO E-RC-1
      *            MOVE W-VE-RC-CICS-2 TO E-RC-2
      *            MOVE E-RC TO MSGFLDO
           END-EVALUATE
           .

       P-AFFICHER-VENTES.
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
