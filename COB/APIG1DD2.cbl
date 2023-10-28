      * ////////////////////////////////////////////////////////////// *
      *                                                                *
      *          ECRAN DE CREATION MODIFICATION D UN CONTRAT           *
      *                                                                *
      * ////////////////////////////////////////////////////////////// *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIG1DD2.
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
       COPY MBLG1D2.
       COPY APIG1DWK.

       77 MAP02O   PIC X.
       77 MAP      PIC 9(02) VALUE 1.
       77 TRANS-ID PIC X(04) VALUE 'G1D2'.
       77 MAPSET   PIC X(07) VALUE 'MBLG1D2'.
       77 PGM-NAME PIC X(08) VALUE 'APIG1DD2'.
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
               WHEN 1
                   MOVE 'CREATION D''UN CONTRAT' TO TITRFLDO
                   MOVE 'SAISIR LES CHAMPS DU CONTRAT A CREER'
                       TO FIELD2O
               WHEN 4
                   MOVE 'MODIFICATION D''UN CONTRAT'  TO TITRFLDO
                   MOVE 'ETAPE 1 - SAISIR LE CODE DU CONTRAT A MODIFIER
      -'+ ENTREE'
                       TO FIELD1O
                   MOVE 'ETAPE 2 - SAISIR LES CHAMPS DU CONTRAT A MODIFI
      -'ER + ENTREE'
                       TO FIELD3O
               WHEN OTHER
                   MOVE ERR-BAD-CHOICE TO MSGFLDO
           END-EVALUATE
           .

       P-ON-SUBMIT.
      *    ON FAIT DU SURPLACE
           MOVE PGM-NAME TO DEST-PGM
           EVALUATE CHOICE-2
               WHEN 1
                   PERFORM P-CREATION-CONTRAT
               WHEN 4
                   PERFORM P-MODIF-CONTRAT
           END-EVALUATE
           .

       P-CREATION-CONTRAT.
           UNSTRING DROITSI
               DELIMITED BY ','
               INTO DROITS-INT, DROITS-DEC
           MOVE CODEAI   TO E-CO-CODEA
           MOVE CODEPI   TO E-CO-CODEP
           MOVE SIRENI   TO E-CO-SIREN
           MOVE FONCTIOI TO E-CO-FONCTION
           MOVE DATSIGNI TO E-CO-DATE-SIGN
           MOVE DROITS-PIC9 TO E-CO-DROITS
           EXEC CICS
               LINK
               PROGRAM ('PGMG1VD1')
               INPUTMSG (E-CONTRAT)
               INPUTMSGLEN (LENGTH OF E-CONTRAT)
           END-EXEC
           EXEC CICS
               RECEIVE
               INTO (W-CONTRAT)
           END-EXEC
           EVALUATE TRUE
               WHEN W-CO-RC-NORMAL
                   MOVE 'CONTRAT CREE' TO MSGFLDO
               WHEN W-CO-RC-NOTOPEN
                   MOVE 'FICHIER FERME' TO MSGFLDO
               WHEN W-CO-RC-DUPREC
                   MOVE 'CONTRAT EXISTANT' TO MSGFLDO
               WHEN OTHER
                   MOVE W-CO-RC-CICS-1 TO E-RC-1
                   MOVE W-CO-RC-CICS-2 TO E-RC-2
                   MOVE E-RC TO MSGFLDO
           END-EVALUATE
           .

       P-MODIF-CONTRAT.
           MOVE ERR-NOT-AVAIL TO MSGFLDO
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
