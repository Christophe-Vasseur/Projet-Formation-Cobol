       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIG1DB2.
       AUTHOR. EDR.
       DATE-WRITTEN. 21/10/23.

      * ============================================================== *
      *                                                                *
      *            E N V I R O N M E N T   D I V I S I O N             *
      *                                                                *
      * ============================================================== *

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.

      * ============================================================== *
      *                                                                *
      *                   D A T A   D I V I S I O N                    *
      *                                                                *
      * ============================================================== *

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       COPY DFHAID.
       COPY DFHBMSCA.
       COPY MBLG1B2.
       COPY APIG1DWK.
       COPY CHANSON.

       77 MAP02O   PIC X.
       77 MAP      PIC 9(02) VALUE 1.
       77 TRANS-ID PIC X(04) VALUE 'G1B2'.
       77 MAPSET   PIC X(07) VALUE 'MBLG1B2'.
       77 PGM-NAME PIC X(08) VALUE 'APIG1DB2'.
       77 PF3-PGM  PIC X(08) VALUE 'APIG1DSM'.

      * SOUS-PARAGRAPHES

       01 CODE-ERREUR.
           02 CODE-ERR-1 PIC 9(04).
           02 CODE-ERR-2 PIC 9(04).

       01 W-CHANSON-MODIF.
           05 W-CH-CODEC           PIC X(04).
           05 W-CH-CODEA           PIC X(04).
           05 W-CH-TITREC          PIC X(40).
           05 W-CH-CODE-MODIF      PIC 9(01).
           05 FILLER               PIC X(31).

       01 W2-CHANSON-MODIF.
           05 W2-CH-CODEC          PIC X(04).
           05 W2-CH-CODEA          PIC X(04).
           05 W2-CH-TITREC         PIC X(40).
           05 FILLER               PIC X(32).

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
           INITIALIZE W-CHANSON-MODIF
           .

       P-ON-DISPLAY.
           EVALUATE CHOICE-2
               WHEN 1
                   MOVE 'CREATION D''UNE CHANSON'  TO TITRFLDO
                   MOVE 'SAISIR LES CHAMPS DE LA CHANSON A CREER'
                       TO FIELD2O
               WHEN 4
                   MOVE 'MODIFICATION D''UNE CHANSON'  TO TITRFLDO
                   MOVE 'ETAPE 1 - SAISIR LE CODE DE LA CHANSON A MODIFI
      -'ER + ENTREE'
                       TO FIELD1O
                   MOVE 'ETAPE 2 - SAISIR LES CHAMPS DE LA CHANSON A MOD
      -'IFIER + ENTREE'
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
                   PERFORM P-CREATION-CHANSON
               WHEN 4
                   PERFORM P-MODIF-CHANSON
           END-EVALUATE
           .

       P-CREATION-CHANSON.
           MOVE CODECI TO CH-CODEC
           MOVE CODEAI TO CH-CODEA
           MOVE TITRECI TO CH-TITREC

           EXEC CICS
               LINK PROGRAM('PGMG1VB1')
               INPUTMSG(E-CHANSON)
               INPUTMSGLEN(LENGTH OF E-CHANSON)
           END-EXEC

      *    ON REVIENT DU PROG D ACCES AUX DONNEES
      *    ET ON RECOIT DES PARAM EN RETOUR (CODE
      *    RETOUR = ECRITURE EFFECTUE OU NON)

           EXEC CICS RECEIVE
               INTO (CODE-ERREUR)
           END-EXEC
           EVALUATE CODE-ERR-1
               WHEN 1
                   MOVE 'ENREGISTREMENT CREE AVEC SUCCES' TO MSGFLDO
               WHEN 2
                   MOVE 'CLE EXISTANTE, ECHEC CREATION'   TO MSGFLDO
               WHEN 3
                   MOVE 'ECHEC CREATION, PROBLEME FICHER' TO MSGFLDO
               WHEN OTHER
                   MOVE CODE-ERREUR                       TO MSGFLDO
           END-EVALUATE
           .

       P-MODIF-CHANSON.
           EVALUATE FLAG-MODIF
               WHEN 0
                   MOVE CODECI TO W-CH-CODEC
                   MOVE FLAG-MODIF TO W-CH-CODE-MODIF

                   EXEC CICS
                       LINK
                       PROGRAM ('PGMG1VB4')
                       INPUTMSG (W-CHANSON-MODIF)
                       INPUTMSGLEN (LENGTH OF W-CHANSON-MODIF)
                   END-EXEC

                   EXEC CICS
                       RECEIVE
                       INTO (W-CHANSON-MODIF)
                   END-EXEC

                   EVALUATE W-CH-CODE-MODIF
                       WHEN 1
                           MOVE 'CHANSON TROUVEE'  TO MSGFLDO
                           MOVE W-CH-TITREC        TO TITRECO
                           MOVE W-CH-CODEA         TO CODEAO
                           MOVE 1 TO FLAG-MODIF
                       WHEN 2
                           MOVE 'CHANSON NON TROUVEE' TO MSGFLDO
                       WHEN OTHER
                           MOVE 'ERREUR FICHIER'   TO MSGFLDO
                   END-EVALUATE

               WHEN 1
                   MOVE FLAG-MODIF TO W-CH-CODE-MODIF
                   MOVE CODECI TO W-CH-CODEC
                   MOVE TITRECI TO W-CH-TITREC
                   MOVE CODEAI TO W-CH-CODEA

                   EXEC CICS
                       LINK
                       PROGRAM ('PGMG1VB4')
                       INPUTMSG (W-CHANSON-MODIF)
                       INPUTMSGLEN (LENGTH OF W-CHANSON-MODIF)
                   END-EXEC

                   EXEC CICS
                       RECEIVE
                       INTO (W-CHANSON-MODIF)
                   END-EXEC

                   EVALUATE W-CH-CODE-MODIF
                       WHEN 1
                           MOVE 'MODIFICATION ENREGISTREE' TO MSGFLDO
                       WHEN 2
                           MOVE 'ECHEC DE LA MODIFICATION' TO MSGFLDO
                       WHEN OTHER
                           MOVE 'ERREUR FICHIER'           TO MSGFLDO
                   END-EVALUATE

                   MOVE 0 TO FLAG-MODIF

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
