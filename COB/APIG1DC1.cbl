       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIG1DC1.
       AUTHOR. RAZ.
       DATE-WRITTEN. 20/10/2023.

      * ============================================================== *
      *                                                                *
      *                   D A T A   D I V I S I O N                    *
      *                                                                *
      * ============================================================== *

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       COPY DFHAID.
       COPY DFHBMSCA.
       COPY MBLG1C1.
       COPY APIG1DWK.
       COPY PERSONNE.

       77 MAP02O   PIC X.
       77 MAP      PIC 9(02) VALUE 1.
       77 TRANS-ID PIC X(04) VALUE 'G1C1'.
       77 MAPSET   PIC X(07) VALUE 'MBLG1C1'.
       77 PGM-NAME PIC X(08) VALUE 'APIG1DC1'.
       77 PF3-PGM  PIC X(08) VALUE 'APIG1DSM'.

      * SOUS-PARAGRAPHES

       01  W-PERSONNE.
           05 W-PE-CODEP           PIC X(04).
           05 W-PE-CODE-TYPE       PIC X(01).
           05 W-PE-IDENTITE        PIC X(25).
           05 W-PE-CODE-CONSULT    PIC 9(01).
           05 FILLER               PIC X(49).

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
                   MOVE 'CONSULTATION D''UNE PERSONNE' TO TITRFLDO
                   MOVE 'CONSULTER' TO ACTFLDO
               WHEN 3
                   MOVE 'SUPPRESSION D''UNE PERSONNE'  TO TITRFLDO
                   MOVE 'SUPPRIMER' TO ACTFLDO
               WHEN OTHER
                   MOVE ERR-BAD-CHOICE TO MSGFLDO
           END-EVALUATE
           .

       P-ON-SUBMIT.
      *    ON FAIT DU SURPLACE
           MOVE PGM-NAME TO DEST-PGM
           EVALUATE CHOICE-2
               WHEN 2
                   PERFORM P-CONSULTATION-PERSONNE
               WHEN 3
                   PERFORM P-SUPPRESSION-PERSONNE
           END-EVALUATE
           .

       P-CONSULTATION-PERSONNE.
           MOVE CODEPI TO PE-CODEP
           EXEC CICS
               LINK PROGRAM('PGMG1VC2')
               INPUTMSG(E-PERSONNE)
               INPUTMSGLEN(LENGTH OF E-PERSONNE)
           END-EXEC

           EXEC CICS
               RECEIVE
               INTO(W-PERSONNE)
           END-EXEC

           EVALUATE W-PE-CODE-CONSULT
               WHEN 1
                   MOVE 'PERSONNE TROUVE'     TO MSGFLDO
                   MOVE W-PE-IDENTITE         TO IDENTO
                   MOVE W-PE-CODE-TYPE        TO CODETO
               WHEN 2
                   MOVE 'PERSONNE NON TROUVE' TO MSGFLDO
               WHEN OTHER
                   MOVE 'ECHEC CONSULTATION, PROBLEME FIC' TO MSGFLDO
           END-EVALUATE
           .

       P-SUPPRESSION-PERSONNE.

           MOVE CODEPI TO PE-CODEP

           EXEC CICS
               LINK
               PROGRAM ('PGMG1VC3')
               INPUTMSG (E-PERSONNE)
               INPUTMSGLEN (LENGTH OF E-PERSONNE)
           END-EXEC

           EXEC CICS
               RECEIVE
               INTO (W-PERSONNE)
           END-EXEC

           EVALUATE W-PE-CODE-CONSULT
               WHEN 1
                   MOVE W-PE-CODEP     TO CODEPO
                   MOVE W-PE-IDENTITE  TO IDENTO
                   MOVE W-PE-CODE-TYPE TO CODETO
                   MOVE 'PERSONNE SUPPRIMEE'   TO MSGFLDO
               WHEN 2
                   MOVE 'PERSONNE NON TROUVEE' TO MSGFLDO
               WHEN OTHER
                   MOVE 'PROBLEME FICHIER'     TO MSGFLDO
           END-EVALUATE
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
